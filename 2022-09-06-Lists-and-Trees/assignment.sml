(*
    1. For lists define the function map : ('a -> 'b) -> 'a list -> 'b list. 
    The semantics of map is that it applies the given function on all the elements of the list, i.e.
    map f [x1, x2,....,xn] = [f x1, f x2, ... f xn]
*)

fun map F nil = nil
|   map F (x::xs) = (F x)::(map F xs)

(* 
    2. Define the data type 'a tree that captures a binary tree.
*)

datatype 'a tree = nulltree 
| node of 'a tree * 'a * 'a tree

(*
    3. Can you write a function treemap analogues to map for list ?
    First write its type and then complete its definition.

    type(treemap): ('a tree -> 'b tree) -> 'a tree -> 'b tree

    It is possible as every tree can be treated as a list and vice versa, and a map funtion can be defined
    from a list to a list.
*)

fun treemap F nulltree = nulltree
|   treemap F (node(left,root,right)) = node(treemap F left , F root , treemap F right)

(*
    4. Define the in-order, pre-order and post-order traversal of the
    binary tree returning the list of nodes in the given order. First
    write down the type of the function(s) and then go about defining
    them.

    inorder: 'a tree -> 'a list
    Nulltree -> Empty List
    Node(l,x,r) -> Inorder(l)+ x + Inorder(r)

    preorder: 'a tree -> 'a list
    Nulltree -> Empty List
    Node(l,x,r) -> x + preorder(l) + preorder(r)

    inorder: 'a tree -> 'a list
    Nulltree -> Empty List
    Node(l,x,r) -> Postorder(l) + postorder(r) + x
*)

fun inorder nulltree = []
|   inorder (node(left, root, right)) = inorder(left)@root::inorder(right)

fun preorder nulltree = []
|   preorder (node(left, root, right)) = root::preorder(left)@preorder(right)

fun postorder nulltree = []
|   postorder (node(left, root, right)) = postorder(left)@postorder(right)@[root]

(*
    5. Define the rotate clockwise function for binary trees.
    rotate_clockwise: 'a tree -> 'a tree

    If tree == nulltree or tree->left == nulltree, then no change; 
    Behaves as identity function

    Else, rotation takes place in the clockwise direction as defined:
        root                     c
       /    \                  /   \
      c     right    ->       cl   root
     / \                          /    \
    cl cr                        cl     cr
    
*)

fun rotate_clockwise (node(node(cl, c, cr), root, right)) = node(cl, c, node(cr, root, right))
|   rotate_clockwise (node(nulltree, root, right)) = node(nulltree, root, right)
|   rotate_clockwise nulltree = nulltree
