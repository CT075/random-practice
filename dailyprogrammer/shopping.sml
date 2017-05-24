
signature PRICES =
sig
  datatype tix = OH | BC | SK

  val price : tix -> int
end

signature CART =
sig
  type tix
  type deal

  val parseToken : string -> tix

  val deals : deal list
  val total : tix list -> int
end

functor ShoppingCart (P:PRICES) : CART =
struct
  type tix = P.tix
  type deal = (tix list -> int) -> (tix list -> int)

  fun parseToken "OH" = P.OH
    | parseToken "BC" = P.BC
    | parseToken "SK" = P.SK
    | parseToken s = raise Fail ("Invalid ticket token: " ^ s ^ ", aborting")

  fun threeForTwoOH ttl = (fn cart =>
    let
      val (os, rest) = List.partition (fn x => x=P.OH) cart
      val rest_price = ttl rest
      val oh_num = List.length os
      val oh_num' = ((oh_num div 3) * 2 + (oh_num mod 3))
      val os_price = ttl (List.tabulate (oh_num', fn _ => P.OH))
    in
      os_price + rest_price
    end)

  fun skForOH ttl = (fn cart =>
    let
      val (os, rest) = List.partition (fn x => x=P.OH) cart
      val (ss, bs) = List.partition (fn x => x=P.SK) rest
      val b_price = ttl bs
      val o_price = ttl os
      val ss_num = Int.max (List.length ss - List.length os, 0)
      val s_price = ttl (List.tabulate (ss_num, fn _ => P.OH))
    in
      o_price + b_price + s_price
    end)

  fun bulkBridge ttl = (fn cart =>
    let
      val (bs, rest) = List.partition (fn x => x=P.BC) cart
      val rest_price = ttl rest
      val bs_price =
        if List.length bs <= 4
          then ttl bs
          else
            (* We lower-bound it to zero so we don't end up with a negative
             * price kek
             *)
            Int.max (ttl bs - (List.length bs * 20), 0)
    in
      bs_price + rest_price
    end)

  fun addItem cart itm = itm::cart
  val deals = [threeForTwoOH, skForOH, bulkBridge]

  fun total cart =
    let
      val sumCart = foldl (op+) 0 o map P.price
      (* Apply each deal individually *)
      val dealsApplied = map (fn d => d sumCart cart) deals
    in
      foldl Int.min (sumCart cart) dealsApplied
    end
end

structure Client =
struct
  structure C = ShoppingCart(
    struct
      datatype tix = OH | BC | SK

      fun price OH = 300
        | price BC = 110
        | price SK = 30
    end)

  fun split l : string list =
    let
      fun split' acc [] =
          if List.null acc
            then []
            else [(String.implode o List.rev) acc]
        | split' acc (c::cs) =
          if Char.isSpace c
            then ((String.implode o List.rev) acc)::split' [] cs
            else split' (c::acc) cs
    in
      split' [] (String.explode l)
    end

  fun processLine l =
    let
      val toks = split l
      val tix_list = map C.parseToken toks
      val ttl = C.total tix_list
    in
      String.concatWith ", " toks ^ " = " ^ Int.toString ttl
    end

  fun getLines path =
    let
      val ins = TextIO.openIn path
      fun loop ins =
        case TextIO.inputLine ins of
             SOME line => line :: loop ins
           | NONE => []
    in
      loop ins before TextIO.closeIn ins
    end

  val processFile =
    print o String.concatWith "\n" o map processLine o getLines
end

