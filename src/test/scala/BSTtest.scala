package BST

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BSTtest extends FunSuite {

  trait TestSetsSimple {
    val tree = BST(2)
    val tree2 = tree.include(4)
    val tree3 = tree2.include(5).include(-3)
    val tree4 = tree3 include 0
  }

  test("BST: contains") {
    new TestSetsSimple {
      assert(tree2.contains(4) ===  true)
      assert(tree2.contains(5) === false)
      assert(tree4.contains(0) === true)
    }
  }

  test("BST: remove") {
    new TestSetsSimple {
      assert(tree4.remove(-3).contains(-3) ===  false)
    }
    new TestSetsSimple {
      assert(tree.remove(2).contains(-3) ===  false)
    }
  }

  test("BST: size") {
    new TestSetsSimple {
      assert(tree.size ===  1)
      assert(tree2.size === 2)
      assert(tree3.size === 4)
      assert(tree4.size === 5)
    }
  }
}
