package exercice_42

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class MyList[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object EmptyList extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new Exception("Empty list")
  def tail: Nothing = throw new Exception("Empty list")
}

object List {
  def apply[T](): List[Nothing] = EmptyList
  def apply[T](item1: T): List[T] = new MyList[T](item1, EmptyList)
  def apply[T](item1: T, item2: T): List[T] = new MyList[T](item1, List(item2))
}
