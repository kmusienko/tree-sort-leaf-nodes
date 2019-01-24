package immutable

object TestMain extends App {
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