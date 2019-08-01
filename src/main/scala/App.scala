import BST.BST

object Main extends App{
  {
    val tree = BST((4,"foo"))
    println("tree",tree)
    val tree1 = tree.include((2,"two"))
    println("tree1", tree1)
    val tree2 = tree.include((5),"five")
    println("tree2", tree2)
    val tree3 = tree2.include(3,"three")
    println("tree3", tree3)
    println(tree3.contains(4))
    println(tree3.remove("three"))
    println(tree3.contains(5))
    println("Some", tree3.find("five"))

    val tree4 = tree2.include("String","this is string")
    println(tree4.find("this is string"))
  }
//  {
//    val tree = BST(3,4,5,6)
//    println(tree)
//  }
//  {
//    val tree = BST("a","b","C","n")
//    println(tree)
//  }
}
