import BST.BST

object Main extends App{
  {
    val tree = BST(4)
    println("tree",tree)
    val tree1 = tree.include(2)
    println("tree1", tree1)
    val tree2 = tree.include(5)
    println("tree2", tree2)
    val tree3 = tree2.include(-3)
    println("tree3", tree3)
    println(tree3.contains(4))
    println(tree3.remove(-3))
    println(tree3.contains(5))

  }
  {
    val tree = BST(3,4,5,6)
    println(tree)
  }
  {
    val tree = BST("a","b","C","n")
    println(tree)
  }
}
