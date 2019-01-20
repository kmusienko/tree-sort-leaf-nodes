case class SinglyLinkedList() {

  var head: Option[Node] = None
  var size: Int = 0

  case class Node(var value: Int, var next: Option[Node] = None) {
    def print(): Unit = {
      println(value)
      next.foreach(_.print())
    }

    override def toString: String = String.valueOf(value) + next.fold("")(_.toString)
  }

  def this(elems: Int*) = {
    this()
    addAll(elems)
  }

  def addAll(elems: Seq[Int]): Unit = {
    elems.foreach {
      elem =>
        val newNode = Node(elem)
        newNode.next = head
        head = Some(newNode)
    }
    size += elems.size
  }

  def print(): Unit = head.foreach(_.print())

  override def toString: String = head.fold("Empty")(_.toString)

  def isEmpty: Boolean = size == 0
}

object SinglyLinkedList {
  def empty: SinglyLinkedList = new SinglyLinkedList()
}