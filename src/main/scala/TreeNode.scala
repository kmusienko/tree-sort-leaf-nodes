import scala.collection.immutable.Queue

case class TreeNode(nodes: Queue[TreeNode] = Queue.empty,
                    leafList: SinglyLinkedList = SinglyLinkedList.empty)