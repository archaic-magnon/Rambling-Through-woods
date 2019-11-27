datatype 'a bintree =Empty |
        Node of 'a * 'a bintree * 'a bintree
signature BINTREE =
        sig
                
                val root : 'a bintree -> 'a
                val leftSubtree : 'a bintree -> 'a bintree
                val rightSubtree: 'a bintree -> 'a bintree
                val height : 'a bintree -> int
                val size : 'a bintree -> int
                val isLeaf : 'a bintree -> bool
                val inorder : 'a bintree -> ('a *int) list
                val getRoot : ('a * int) list -> ('a * int) option
                val split : int * ('a * int) list * ('a * int) list -> ('a * int) list * ('a * int) list
                val inorderInverse: ('a * int) list -> 'a bintree
        end (* sig *)
structure Bintree : BINTREE =
   struct
   exception Empty_bintree;
   fun root Empty = raise Empty_bintree
           | root (Node (x, _, _)) = x;
   fun leftSubtree Empty = raise Empty_bintree
           | leftSubtree (Node (_, LST, _)) = LST;
   fun rightSubtree Empty = raise Empty_bintree
           | rightSubtree (Node (_, _, RST)) = RST;
   fun height Empty = 0
           | height (Node (_, left, right)) =
                   let
                           val lh = height left
                           val rh = height right
                   in 1 + Int.max(lh, rh)
                   end;
   fun size Empty = 0
           | size (Node (_, left, right)) =
                   let
                           val ls = size left
                           val rs = size right
                   in 1 + ls + rs
                   end;
   fun isLeaf Empty = false
           | isLeaf (Node (_, Empty, Empty)) = true
           | isLeaf _ = false;
   local
           fun ino (Empty, Llist, Level) = Llist
                   | ino (Node (N, LST, RST), Llist, Level) =
                           let val Mlist = ino (RST, Llist, Level+1)
                                   val Nlist = ino (LST, (N,Level)::Mlist, Level+1)
                           in Nlist
                           end
   in
           fun inorder T = ino (T, [],0)
   end
   fun getRoot [] = NONE
           |  getRoot(List) = 
                   let
                           fun getMin(List: ('a * int) list) = 
                                   if null (tl List) then (hd List)
                                   else
                                           let
                                                   val tl_ans = getMin(tl List)
                                           in
                                                   if #2 (hd List) < #2 (tl_ans) then (hd List)
                                                   else tl_ans
                                           end
                   in
                           SOME (getMin(List))
                   end
   fun split(Min: int, L1: ('a * int) list, L2: ('a * int) list) =
           if Min = #2 (hd L2) then (L1, tl L2)
           else
                   let
                           val new_l1 = L1@[hd L2]
                           val new_l2 = tl L2
                   in
                           split(Min, new_l1, new_l2)
                   end
   fun inorderInverse [] = Empty
           |       inorderInverse List = 
                   let
                           val root = valOf(getRoot(List))
                           val rootVal = #1 root
                           val rootLevel = #2 root
                           val (LSTList, RSTList) = split(rootLevel, [], List)
                           val (LST, RST) = (inorderInverse(LSTList), inorderInverse(RSTList))
                   in
                           Node(rootVal, LST, RST )
                   end
   end
