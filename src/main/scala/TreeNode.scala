import TreeTraversalTypes.TraversalType

import scala.collection.immutable.Queue

case class TreeNode(nodes: Queue[TreeNode] = Queue.empty,
                    leafList: SinglyLinkedList = SinglyLinkedList.empty) {

  private def sortLeafNodes(iterator: Iterator[TreeNode], w: Int): Unit = {
    if (leafList.isEmpty) {
      if (iterator.hasNext) {
        val next = iterator.next
        next.sortLeafNodes(iterator, w)
      }
    } else {
      leafList.sort()
      val removedValues = leafList.removeAfterSumMoreThanW(w)
      if (iterator.hasNext) {
        val nextNode = iterator.next()
        nextNode.leafList.addAll(removedValues)
        nextNode.sortLeafNodes(iterator, w)
      }
    }
  }
}