
(* Implementation of Conway's GOL with red/blue cells as specified by
 * /r/dailyprogrammer's Intermediate challenge #315
 *)

structure Conway =
struct
  (* this seed is fixed, because SML *sucks* and there's no way to get the
   * system time that I could find.
   *)
  val SEED = (100, 73)

  datatype color = RED | BLUE
  datatype cell = Off | On of color
  type board = cell list list * int * int

  fun swapColor RED = BLUE
    | swapColor BLUE = RED
  fun fst (x,y) = x

  fun initRand w h =
    let
      val rng = Random.rand SEED
      fun randCell () =
        let
          val col = Random.randRange (0,1) rng
          val st = Random.randRange (0,100) rng
        in
          case (st > 45, col=1) of
               (true, _) => Off
             | (_, true) => On RED
             | _ => On BLUE
        end
    in
      (List.tabulate (h, (fn y =>
        List.tabulate (w, (fn x => (randCell ())))
      )), w, h)
    end

    fun initFromString s =
      let
        fun fromChar #"." = Off
          | fromChar #"*" = On RED
          | fromChar #"#" = On BLUE
          | fromChar _ = raise Fail "Unknown character"
        (* asdfasdfasdf why does #"\n" not work *)
        val rows = String.tokens Char.isSpace s
        val cells = map (map fromChar o String.explode) rows
      in
        (cells, List.length (List.nth (cells, 0)), List.length rows)
      end

  fun update (brd as (b,w,h):board) (x:int,y:int) =
    let
      fun getSq (xoffs, yoffs) =
        List.nth (List.nth (b, (y+yoffs) mod h), (x+xoffs) mod w)
      val c = getSq (0,0)
      val neighbors = [ getSq (1,0)
                      , getSq (0,1)
                      , getSq (1,1)
                      , getSq (~1,0)
                      , getSq (0,~1)
                      , getSq (~1,~1)
                      , getSq (~1,1)
                      , getSq (1,~1)
                      ]
      val reds = foldl (op+) 0 (map (fn (On RED) => 1 | _ => 0) neighbors)
      val blues = foldl (op+) 0 (map (fn (On BLUE) => 1 | _ => 0) neighbors)
    in
      (* This whole expression is pretty gross, but I'm sticking with it
       * because it keeps the exact case layout pretty clear
       *)
      case c of
           Off =>
             if reds+blues = 3 then On (if reds > blues then RED else BLUE)
                               else Off
         | On col =>
             let
               val (same, diff) =
                 if col=RED then (reds,blues) else (blues,reds)
               val keepLive = reds+blues <= 3 andalso reds+blues >= 2
             in
               case (same < diff, keepLive) of
                    (true, _) => On (swapColor col)
                  | (_, true) => On col
                  | _ => Off
             end
    end

  fun step (brd as (b,w,h) : board) : board =
    (List.tabulate (h, (fn y =>
      List.tabulate (w, fn x => update brd (x,y))
    )), w, h)

  fun format (b,_,_) : string =
    let
      fun toChr Off = #"."
        | toChr (On RED) = #"#"
        | toChr (On BLUE) = #"*"
    in
      String.concatWith "\n" (map (String.implode o map toChr) b)
    end
end

structure Client =
struct
  open Conway

  fun id x = x
  (* If I had more time/energy, I would have it print out each intermediate
   * step. It honestly wouldn't be that difficult, but meh
   *)
  fun simulate w h n =
    let
      val game = initRand w h
      val game' = (foldl (op o) id (List.tabulate (n, fn _ => step))) game
    in
      (
      print "Initial state:\n";
      print (format game ^ "\n");
      print "\nFinal state:\n";
      print (format game' ^ "\n")
      )
    end
end

