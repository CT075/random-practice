
fun battle (p1 as x::xs) (p2 as y::ys) =
  (case (Int.compare(x,y)) of
       LESS => battle xs (ys @ [y,x])
     | GREATER => battle (xs @ [x,y]) ys
     | EQUAL => war xs ys (fn l => l @ [x,y], fn l => l @ [y,x]))
  | battle [] [] = (0,[])
  | battle [] p2 = (2,p2)
  | battle p1 [] = (1,p1)
(* int list -> int list -> (int list->int list*int list->int list) -> int *)
and war [] [] _ = (0, [])
  | war [] p2 (_,p_y) = (2,p_y p2)
  | war p1 [] (p_x,_) = (1,p_x p1)
  | war p1 p2 (p_x, p_y) =
      let
        val left = Int.min(List.length p1, List.length p2)
        val top = Int.min(left, 4)
        val (fx, bx) = (List.take(p1, top-1), List.drop(p1, top-1))
        val (fy, by)= (List.take(p2, top-1), List.drop(p2, top-1))
        val ((x::xs), (y::ys)) = (bx, by)
        fun p_x' l = p_x (l @ fx @ [x] @ fy @ [y])
        fun p_y' l = p_y (l @ fy @ [y] @ fx @ [x])
      in
        case (Int.compare(x,y)) of
             LESS => battle xs (p_y (ys @ fy @ [y] @ fx @ [x]))
           | GREATER => battle (p_x (xs @ fx @ [x] @ fy @ [y])) ys
           | EQUAL => war xs ys (p_x', p_y')
      end

