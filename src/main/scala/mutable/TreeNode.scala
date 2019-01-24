package mutable

import mutable.TreeTraversalTypes.TraversalType

import scala.collection.immutable.Queue

/**
  * Implementation of a tree.
  *
  * @param nodes    queue of nodes of the current tree node
  * @param leafList singly linked list of leaf nodes of the current tree node
  */
case class TreeNode(nodes: Queue[TreeNode] = Queue.empty,
                    leafList: SinglyLinkedList = SinglyLinkedList.empty) {


  /**
    * Sorts leaf nodes in a tree and moves extra leaf nodes from each node to the next node.
    *
    * If extra leaf nodes are present in the last node, they are deleted.
    *
    * Definition of the extra leaf nodes:
    *
    * If we start iterating over the leaf nodes of a some particular node and count sum of their values,
    * extra leaf nodes will be a right part of a leaf nodes starting from a node when the sum begins being more than
    * given constant W.
    *
    * @param traversalType a way to traverse the tree
    * @param w             given constant W
    */
  def sortLeafNodes(traversalType: TraversalType, w: Int): Unit = {
    val iterator: Iterator[TreeNode] = traversalType.iterator[TreeNode](this, _.nodes)
    sortLeafNodes(iterator, w)
  }

  /**
    * Sorts leaf nodes in a tree and moves extra leaf nodes from each node to the next node.
    *
    * If extra leaf nodes are present in the last node, they are deleted.
    *
    * Definition of extra leaf nodes:
    *
    * If we start iterating over the leaf nodes of a some particular node and count sum of their values,
    * extra leaf nodes will be a right part of a leaf nodes starting from a node when the sum begins being more than
    * given constant W.
    *
    * @param iterator iterator to traverse the tree
    * @param w        given constant W
    */
  def sortLeafNodes(iterator: Iterator[TreeNode], w: Int): Unit = {
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
