
fun squash (w1:string) (w2:string) : string option =
  let
    fun squash' [] w2 = NONE
      | squash' (w1' as c::cs) w2 =
        if String.isPrefix (String.implode w1') w2
          then SOME w2
          else SOME ((Char.toString c) ^ (valOf (squash' cs w2)))
               handle Option => NONE
  in
    squash' (String.explode w1) w2
  end

fun condense (x1::x2::xs:string list) : string list =
      (case (squash x1 x2) of
           NONE => x1 :: condense (x2 :: xs)
         | SOME x => condense (x::xs))
  | condense ws = ws

fun words l : string list =
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

val process = String.concatWith " " o condense o words

