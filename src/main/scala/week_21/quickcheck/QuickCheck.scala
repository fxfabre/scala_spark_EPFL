package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      y <- arbitrary[Int]
      z <- arbitrary[Int]
      m <- genHeap
    } yield insert(z, insert(y, insert(x, m)))
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def str_heap(heap: H): String = {
    if (isEmpty(heap)) ""
    else findMin(heap).toString + deleteMin(heap)
  }
  def len(heap: H, count: Int = 0): Int = {
    if (isEmpty(heap)) 0
    else len(deleteMin(heap), count + 1)
  }

  property("gen1") = forAll { (h: H) =>
    println("len h : " + len(h))
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // insert any two elements into an empty heap, the min should get the smallest
  property("2Elements") = forAll{ (x: Int, y: Int) =>
    val min_value = if (x < y) x else y
    val heap = insert(y, insert(x, this.empty))
    findMin(heap) == min_value
  }

  // If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("insert_delete") = forAll { (x: Int) =>
    val heap = deleteMin(insert(x, empty))
    isEmpty(heap)
  }

  // Given any heap, you should get a sorted sequence of elements
  // when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  def test_min_value(heap: H, lower_bound: Int): Boolean = {
    if (isEmpty(heap)) true
    else {
      val lower = findMin(heap)
      if (lower < lower_bound) false
      else test_min_value(deleteMin(heap), lower)
    }
  }
  property("del_all_nodes") = forAll { (heap: H) =>
    println("len h : " + len(heap))
    if (isEmpty(heap)) true
    else test_min_value(heap, findMin(heap))
  }

  // Finding a minimum of the melding of any two heaps
  // should return a minimum of one or the other.
  property("merge_heap") = forAll{ (h1: H, h2: H) =>
    println("len h1 : " + len(h1) + ", len h2 : " + len(h2))
    val merged = meld(h1, h2)
    if (isEmpty(merged)) {
      isEmpty(h1) && isEmpty(h2)
    } else if (isEmpty(h1)) {
      findMin(merged) == findMin(h2)
    }
    else if (isEmpty(h2)) {
      findMin(merged) == findMin(h1)
    }
    else {
      val min_val = findMin(merged)
      if (min_val == findMin(h1)) {
        min_val <= findMin(h2)
      } else {
        (min_val <= findMin(h1)) & (min_val == findMin(h2))
      }
    }
  }
}
