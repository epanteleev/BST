package BST

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BSTtest extends FunSuite {

  trait TestSetsSimple {
    val tree = BST((2,"two"))
    val tree2 = tree.include((4,"foo"))
    val tree3 = tree2.include((5,"five")).include((-3,"minus three"))
    val tree4 = tree3 include (0,"zero")
  }

  test("BST: contains") {
    new TestSetsSimple {
      assert(tree2.contains(4) ===  true)
      assert(tree2.contains(5) === false)
      assert(tree4.contains(0) === true)
    }
  }

//  test("BST: remove") {
//    new TestSetsSimple {
//      assert(tree4.remove("minus three")._2 === -3)
//    }
//    new TestSetsSimple {
//      assert(tree.remove("two")._2 ===  2)
//    }
//  }

  test("BST: size") {
    new TestSetsSimple {
      assert(tree.size ===  1)
      assert(tree2.size === 2)
      assert(tree3.size === 4)
      assert(tree4.size === 5)
    }
  }
}
