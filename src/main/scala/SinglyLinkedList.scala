/**
  * Implementation of a singly linked list of integers.
  */
case class SinglyLinkedList() {

  /**
    * Head of the list.
    */
  var head: Option[Node] = None

  /**
    * Size of the list
    */
  var size: Int = 0

  /**
    * Represents a node of a singly linked list. Singly linked list consists of 'linked' nodes.
    *
    * @param value value that associated with this node
    * @param next  link to the next node
    */
  case class Node(var value: Int, var next: Option[Node] = None) {

    override def toString: String = value.toString + next.fold("")("," + _.toString)
  }

  def this(elems: Int*) = {
    this()
    addAll(elems)
  }

  /**
    * Adds a sequence of int elements to the singly linked list.
    *
    * @param elems elements to be added
    */
  def addAll(elems: Seq[Int]): Unit = {
    elems.foreach {
      elem =>
        val newNode = Node(elem)
        newNode.next = head
        head = Some(newNode)
    }
    size += elems.size
  }

  /**
    * Iterates over the nodes and removes the rest of the nodes starting from a node when a sum of iterated node values
    * is more than given constant W.
    *
    * @param w given constant
    * @return list of removed node values
    */
  def removeAfterSumMoreThanW(w: Int): List[Int] = {
    head match {
      case Some(node) =>
        var sum = node.value
        if (sum > w) {
          val removedValues = toScalaList()
          head = None
          size = 0
          removedValues
        } else {
          var current = node
          while (current.next.isDefined && (sum + current.next.get.value) <= w) {
            sum += current.next.get.value
            current = current.next.get
          }
          removeAllAfter(current)
        }
      case None => List.empty
    }
  }

  /**
    * Removes all the nodes after a specified node.
    *
    * @param node node after which the rest of the linked list should be removed
    * @return list of removed node values
    */
  def removeAllAfter(node: Node): List[Int] = {
    val removedValues = toScalaList(fromNode = node.next)
    node.next = None
    size -= removedValues.size
    removedValues
  }

  /**
    * Creates a Scala List with the values from this SinglyLinkedList with the same order.
    *
    * @param fromNode node from which to start copying values
    * @return new scala list with copied values
    */
  def toScalaList(fromNode: Option[Node] = head): List[Int] = {
    fromNode match {
      case Some(node) => List(node.value) ++ toScalaList(node.next)
      case _ => List.empty
    }
  }

  /**
    * Sorts the SinglyLinkedList in ascending order.
    * Actually, it just calls 'mergeSort' method and overrides the head node.
    */
  def sort(): Unit = {
    head = mergeSort(head)
  }

  /**
    * Creates a sorted SingleLinkedList in ascending order. Uses Merge Sort algorithm.
    *
    * @param maybeNode node to start sorting from
    * @return sorted SingleLinkedList
    */
  def mergeSort(maybeNode: Option[Node]): Option[Node] = {
    maybeNode.flatMap {
      node =>
        node.next match {
          case Some(_) =>
            val maybeMiddle: Option[Node] = findMiddle(node)
            val maybeNextOfMiddle: Option[Node] = maybeMiddle.flatMap(_.next)

            maybeMiddle.foreach(_.next = None)

            val maybeLeft = mergeSort(maybeNode)
            val maybeRight = mergeSort(maybeNextOfMiddle)

            val sortedList: Option[Node] = mergeSorted(maybeLeft, maybeRight)
            sortedList
          case _ => Option(node)
        }
    }
  }

  /**
    * Merges two sorted SingleLinkedLists into one.
    *
    * @param maybeLeft  left linked list
    * @param maybeRight right linked list
    * @return SingleLinkedList as a result of merging left and right lists
    */
  def mergeSorted(maybeLeft: Option[Node], maybeRight: Option[Node]): Option[Node] = {
    (maybeLeft, maybeRight) match {
      case (Some(nodeA), Some(nodeB)) if nodeA.value <= nodeB.value =>
        val result = Some(nodeA)
        result.get.next = mergeSorted(nodeA.next, Some(nodeB))
        result
      case (Some(nodeA), Some(nodeB)) =>
        val result = Some(nodeB)
        result.get.next = mergeSorted(Some(nodeA), nodeB.next)
        result
      case (Some(nodeA), None) => Some(nodeA)
      case (None, Some(nodeB)) => Some(nodeB)
      case (None, None) => None
    }
  }

  /**
    * Finds the middle of the SinglyLinkedList.
    *
    * @param node head of the linked list
    * @return middle node
    */
  def findMiddle(node: Node): Option[Node] = {
    var fastPointer: Option[Node] = node.next
    var slowPointer: Option[Node] = Option(node)

    while (fastPointer.isDefined) {
      fastPointer = fastPointer.get.next
      fastPointer.foreach {
        fastPointerValue =>
          slowPointer = slowPointer.get.next
          fastPointer = fastPointerValue.next
      }
    }
    slowPointer
  }

  override def toString: String = head.fold("LinkedList()")("LinkedList(" + _.toString + ")")

  def isEmpty: Boolean = size == 0
}

object SinglyLinkedList {
  def empty: SinglyLinkedList = new SinglyLinkedList()
}