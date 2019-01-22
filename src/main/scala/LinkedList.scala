sealed trait LinkedList {
  def value: Int

  def next: LinkedList

  def length: Int

  def apply(index: Int): Int

  def ::(elem: Int): LinkedList = Cons(elem, this)

  def ++(lList: LinkedList): LinkedList = {
    if (this.next.length == 0) Cons(this.value, lList)
    else Cons(this.value, this.next ++ lList)
  }

  def splitAt(i: Int): (LinkedList, LinkedList) = {
    (getBefore(i), getAfter(i))
  }

  def getBefore(n: Int): LinkedList = {
    if (n == 1) Cons(this.value, NilCons) else Cons(this.value, this.next.getBefore(n - 1))
  }

  def sumBefore(n: Int): Int = {
    if (n == 1) this.value else this.value + this.next.sumBefore(n - 1)
  }

  def getAfter(n: Int): LinkedList = {
    if (n == 0) this else this.next.getAfter(n - 1)
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var rover: LinkedList = LinkedList.this

    def hasNext: Boolean = !(rover.length == 0)

    def next: Int = {
      val ret = rover.value
      rover = rover.next
      ret
    }
  }

  def mergeSort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = {
        println("left: " + xs + " || right: " + ys)
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
      }

      val (left, right) = xs splitAt (n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def mergeSort: LinkedList = {
    val n = this.length / 2
    if (n == 0) this
    else {
      def merge(xs: LinkedList, ys: LinkedList): LinkedList =
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

  def getAllAfterSumMoreThanW(w: Int): LinkedList = {
    def recurse(n: Int, w: Int): LinkedList = {
      if (sumBefore(n) > w) getAfter(n - 1) else recurse(n + 1, w)
    }

    recurse(1, w)
  }
}

case class Cons(value: Int, next: LinkedList) extends LinkedList {

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

object NilCons extends LinkedList {
  override def value = throw new NoSuchElementException("head of empty list")

  override def next = throw new UnsupportedOperationException("tail of empty list")

  override def length: Int = 0

  override def toString: String = ""

  override def apply(index: Int): Int = throw new IllegalArgumentException("index of empty list")
}

object ConsTutorialDriver extends App {

  val ress = List(2, 5, 8, 5, 1, 3, 5, 4)
  println(Cons(1).mergeSort(ress))

  val smt = Cons(1, 2, 3) ++ Cons(4, 5, 6) ++ NilCons
  println(smt)

  val (a, b) = smt.splitAt(2)
  println(a)
  println(b)

  println("new")
  val ll = Cons(2, 5, 8, 5, 1, 3, 5, 4)
  println(ll)
  val llSorted = ll.mergeSort
  println("sorted: " + llSorted)
  println("old: " + ll)
  println("sum in sorted before n=2: " + llSorted.sumBefore(2))
  println("removed all after sum more than 6: " + llSorted.getAllAfterSumMoreThanW(6))

  val c1 = Cons(1, NilCons)
  val c2 = Cons(2, c1)
  val c3 = Cons(3, c2)
  println(c1)
  println(c2)
  println(c3)


  println()

  val res2 = Cons(1, 2, 3, 4)
  println(res2)

  println()

  List(1, 2) ++ List(2)

  val res3 = 10 :: res2
  println(res3)

  val res4 = 10 :: NilCons
  println(res4)

  val iterator = res3.iterator
  println(iterator.hasNext)
  println(iterator.next())
  println(iterator.next())
}

