package immutable

/**
  * Scala-way custom implementation of immutable Singly LinkedList
  */
sealed trait SinglyLinkedList {
  def value: Int

  def next: SinglyLinkedList

  def length: Int

  def apply(index: Int): Int

  def ::(elem: Int): SinglyLinkedList = Cons(elem, this)

  def ++(lList: SinglyLinkedList): SinglyLinkedList = {
    if (this.next.length == 0) Cons(this.value, lList)
    else Cons(this.value, this.next ++ lList)
  }

  def splitAt(i: Int): (SinglyLinkedList, SinglyLinkedList) = {
    (getBefore(i), getAfter(i))
  }

  def getBefore(n: Int): SinglyLinkedList = {
    if (n == 1) Cons(this.value, NilCons) else Cons(this.value, this.next.getBefore(n - 1))
  }

  def sumBefore(n: Int): Int = {
    if (n == 1) this.value else this.value + this.next.sumBefore(n - 1)
  }

  def getAfter(n: Int): SinglyLinkedList = {
    if (n == 0) this else this.next.getAfter(n - 1)
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var rover: SinglyLinkedList = SinglyLinkedList.this

    def hasNext: Boolean = !(rover.length == 0)

    def next: Int = {
      val ret = rover.value
      rover = rover.next
      ret
    }
  }

  def mergeSort: SinglyLinkedList = {
    val n = this.length / 2
    if (n == 0) this
    else {
      def merge(xs: SinglyLinkedList, ys: SinglyLinkedList): SinglyLinkedList =
        (xs, ys) match {
          case (NilCons, ys) => ys
          case (xs, NilCons) => xs
          case (Cons(value1, next1), Cons(value2, next2)) =>
            if (value1 < value2) value1 :: merge(next1, ys)
            else value2 :: merge(xs, next2)
        }

      val (left, right) = this splitAt n
      println("left: " + left + " || right: " + right)
      merge(left.mergeSort, right.mergeSort)
    }
  }

  def getAllAfterSumMoreThanW(w: Int): SinglyLinkedList = {
    def recurse(n: Int, w: Int): SinglyLinkedList = {
      if (sumBefore(n) > w) getAfter(n - 1) else recurse(n + 1, w)
    }

    recurse(1, w)
  }
}

case class Cons(value: Int, next: SinglyLinkedList) extends SinglyLinkedList {

  override def length: Int = 1 + next.length

  override def apply(index: Int): Int = if (index == 0) value else next(index - 1)

  override def toString = s"$value, $next"
}

object Cons {
  def apply(elems: Int*): Cons = {
    if (elems.size == 1) new Cons(elems.head, NilCons)
    else new Cons(elems.head, apply(elems.drop(1): _*))
  }
}

object NilCons extends SinglyLinkedList {
  override def value = throw new NoSuchElementException("head of empty list")

  override def next = throw new UnsupportedOperationException("tail of empty list")

  override def length: Int = 0

  override def toString: String = ""

  override def apply(index: Int): Int = throw new IllegalArgumentException("index of empty list")
}

