package BST
import math.Ordering

abstract class BST[A]{
  def union(tree: BST[A]): BST[A]

  def include(data: A): BST[A]

  def remove(data: A): BST[A]

  def foreach(f: A => Unit): Unit

  def size: Int

  def contains(data: A): Boolean
}

class NonEmpty[A](elem: A, left: BST[A], right: BST[A] )(implicit ord: Ordering[A]) extends BST[A] {

  override def foreach  (f: A => Unit): Unit = {
    f(elem)
    right.foreach(f)
    left.foreach(f)
  }

  override def union(tree: BST[A]): BST[A] = {
    ((left union right) union tree) include elem
  }

  override def include (data: A):BST[A]= {
    if(ord.lt(data , elem)) new NonEmpty(elem, left.include(data), right)
    else if( ord.gt(data, elem)) new NonEmpty(elem, left, right.include(data))
    else this
  }

  override def remove(data: A): BST[A] = {
    if(ord.lt(data , elem)) new NonEmpty(elem,left.remove(data), right)
    else if (ord.gt(data, elem)) new NonEmpty(elem,left, right.remove(data))
    else left.union(right)
  }

  override def toString: String = {
    "{" + left.toString + "{"+ elem + "}" + right.toString + "}"
  }

  override def size: Int = right.size + left.size + 1

  override def contains(data: A): Boolean = {
    if(ord.lt(data,elem)) left.contains(data)
    else if(ord.gt(data,elem)) right.contains(data)
    else true
  }
}


class Null[A](implicit ord: Ordering[A]) extends BST[A]{

  override def union(tree:  BST[A]): BST[A]  = tree

  override def foreach(f: A => Unit): Unit = ()

  override def size: Int = 0

  override def remove(data: A): BST[A] = this

  override def include(data: A): BST[A] = new NonEmpty[A](data, new Null[A], new Null[A])(ord)

  override def toString: String = "."

  override def contains(data: A): Boolean = false

}


object BST {

  def apply[T](elem: T, data: T*)(implicit ord: Ordering[T]) :BST[T] ={
    var tree: BST[T] = new NonEmpty[T](elem, new Null[T], new Null[T])(ord)
    for (i <- data)
      tree = tree.include(i)
    tree
  }
}


