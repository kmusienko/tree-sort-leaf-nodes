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

  //TODO: rework the code
  def removeAfterSumMoreThanW(w: Int): List[Int] = {
    head match {
      case Some(node) =>
        var current = node
        var sum = node.value
        if (sum > w) {
          val listValuesToReturn = toScalaList()
          head = None
          size -= listValuesToReturn.size
          return listValuesToReturn
        }

        while (current.next.nonEmpty) {
          sum += current.next.get.value
          if (sum > w) {
            val listValuesToReturn = toScalaList(fromNode = current.next)
            current.next = None
            size -= listValuesToReturn.size
            return listValuesToReturn
          }
          current = current.next.get
        }
        List.empty

      case None => List.empty
    }
  }

  def toScalaList(fromNode: Option[Node] = head): List[Int] = {
    fromNode match {
      case Some(node) => List(node.value) ++ toScalaList(node.next)
      case _ => List.empty
    }
  }

  def print(): Unit = head.foreach(_.print())

  override def toString: String = head.fold("Empty")(_.toString)

  def isEmpty: Boolean = size == 0
}

object SinglyLinkedList {
  def empty: SinglyLinkedList = new SinglyLinkedList()
}