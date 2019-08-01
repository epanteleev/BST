package BST
import math.Ordering

abstract class BST[+A,K]{
  def union[B >: A](tree: BST[B,K]): BST[B,K]

  def include[B >: A] (data:(B,K)): BST[B,K]

  def remove(k: K): Option[(BST[A,K],A)]

  def foreach(f: (A,K) => Unit): Unit

  def size: Int

  def contains[B >: A](data: B)(implicit ordA : Ordering[B]): Boolean

  def find(k: K): Option[A]

  def filter(f: (A,K) => Boolean): BST[A,K]

  def filterIter[B >: A](f: (B,K) => Boolean,acc: BST[B,K]): BST[B,K]
}

class NonEmpty[A,K](elem: A,key: K, left: BST[A,K], right: BST[A,K] )(implicit ord: Ordering[K]) extends BST[A,K] {

  override def foreach  (f: (A,K) => Unit): Unit = {
    f(elem,key)
    right.foreach(f)
    left.foreach(f)
  }

  override def union[B >: A](tree: BST[B, K]): BST[B, K] = {
    ((left union right) union tree) include (elem,key)
  }

  override def include[B >: A](data: (B, K)): BST[B, K] = {
    if(ord.lt(data._2 , key)) new NonEmpty(elem,key, left.include(data), right)
    else if( ord.gt(data._2, key)) new NonEmpty(elem,key, left, right.include(data))
    else this
  }

  override def remove(k: K): Option[(BST[A,K],A)] = {
    if(ord.lt(k, key)) {
      val pair = left.remove(k)
      pair match {
        case Some(value) => Some((new NonEmpty(elem,  key,value._1, right),value._2))
        case None => None
      }
    }
    else if (ord.gt(k, key)){
      val pair = right.remove(k)
      pair match {
        case Some(value) => Some((new NonEmpty(elem, key,left, value._1),value._2))
        case None => None
      }
    }
    else Some(left.union(right),elem)
  }


  override def toString: String = {
    "{" + left.toString + "{"+ elem + ":" + key + "}" + right.toString + "}"
  }

  override def size: Int = right.size + left.size + 1

  override def contains[B >: A](data: B)(implicit ordA: Ordering[B]): Boolean = {
    if(ordA.equiv(data, elem)) true
    else right.contains(data) | left.contains(data)
  }

  override def filterIter[B >: A](f: (B, K) => Boolean, acc: BST[B, K]): BST[B, K] = {
    val ac = {
      if(f(elem,key)) acc.include((elem,key))
      else acc
    }
    left.filterIter(f,right.filterIter(f,ac))
  }

  override def filter(f: (A,K) => Boolean): BST[A,K] = {
    filterIter(f, new Null[A,K])
  }

  override def find(k: K): Option[A] = {
    if(ord.lt(k,key)) left.find(k)
    else if(ord.gt(k,key)) right.find(k)
    else Some(elem)
  }
}

class Null[A,K](implicit ord: Ordering[K]) extends BST[A,K]{

  override def union[B >: A](tree: BST[B, K]): BST[B, K] = tree

  override def foreach(f: (A,K) => Unit): Unit = ()

  override def size: Int = 0

  override def remove(key: K): Option[(BST[A,K],A)] = None

  override def include[B >: A](data: (B, K)): BST[B, K] = new NonEmpty[B,K](data._1,data._2, new Null[B,K], new Null[B,K])(ord)

  override def toString: String = "."

  override def contains[B >: A](data: B)(implicit ordA: Ordering[B]): Boolean = false

  override def filter(f: (A,K) => Boolean): BST[A,K] = this

  override def filterIter[B >: A](f: (B, K) => Boolean, acc: BST[B, K]): BST[B, K] = this

  override def find(key: K): Option[A] = None
}

object BST {

  def apply[T,K](elem: (T,K), data:(T,K)*)(implicit ord: Ordering[K]) :BST[T,K] ={
    var tree: BST[T,K] = new NonEmpty[T,K](elem._1,elem._2, new Null[T,K], new Null[T,K])(ord)
    for (i <- data)
      tree = tree.include(i)
    tree
  }
}


