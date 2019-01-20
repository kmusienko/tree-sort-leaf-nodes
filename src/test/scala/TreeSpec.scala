import TreeTraversalTypes.BreadthFirst
import org.scalatest.{BeforeAndAfter, MustMatchers, WordSpec}

import scala.collection.immutable.Queue

class TreeSpec extends WordSpec with BeforeAndAfter with MustMatchers {

  "TreeSpec" must {
    "sort leaf nodes in tree #1 where w = 5" in {
      val tree =
        TreeNode(
          nodes = Queue(
            TreeNode(
              leafList = new SinglyLinkedList(2, 1, 4, 3)
            ),
            TreeNode(
              nodes = Queue(
                TreeNode(
                  leafList = new SinglyLinkedList(1, 1)
                )
              ),
              leafList = new SinglyLinkedList(2)
            )
          )
        )

      tree.sortLeafNodes(traversalType = BreadthFirst, w = 5)

      tree.leafList mustBe empty
      tree.nodes.head.leafList.toScalaList() mustBe List(1, 2)
      tree.nodes(1).leafList.toScalaList() mustBe List(2, 3)
      tree.nodes(1).nodes.head.leafList.toScalaList() mustBe List(1, 1)
      tree.nodes(1).nodes.head.nodes mustBe empty
    }

    "sort leaf nodes in tree #1 where w = 3" in {
      val tree =
        TreeNode(
          nodes = Queue(
            TreeNode(
              leafList = new SinglyLinkedList(2, 1, 4, 3)
            ),
            TreeNode(
              nodes = Queue(
                TreeNode(
                  leafList = new SinglyLinkedList(1, 1)
                )
              ),
              leafList = new SinglyLinkedList(2)
            )
          )
        )

      tree.sortLeafNodes(traversalType = BreadthFirst, w = 3)

      tree.leafList mustBe empty
      tree.nodes.head.leafList.toScalaList() mustBe List(1, 2)
      tree.nodes(1).leafList.toScalaList() mustBe List(2)
      tree.nodes(1).nodes.head.leafList.toScalaList() mustBe List(1, 1)
      tree.nodes(1).nodes.head.nodes mustBe empty
    }

    "sort leaf nodes in tree #2 where w = 5" in {
      val tree =
        TreeNode(
          nodes = Queue(
            TreeNode(),
            TreeNode(),
            TreeNode(),
          ),
          leafList = new SinglyLinkedList(3, 4, 1, 2)
        )

      tree.sortLeafNodes(traversalType = BreadthFirst, w = 5)

      tree.leafList.toScalaList() mustBe List(1, 2)
      tree.nodes.head.leafList.toScalaList() mustBe List(3)
      tree.nodes(1).leafList.toScalaList() mustBe List(4)
      tree.nodes(2).leafList mustBe empty
    }

    "sort leaf nodes in tree #2 where w = 3" in {
      val tree =
        TreeNode(
          nodes = Queue(
            TreeNode(),
            TreeNode(),
            TreeNode(),
          ),
          leafList = new SinglyLinkedList(3, 4, 1, 2)
        )

      tree.sortLeafNodes(traversalType = BreadthFirst, w = 3)

      tree.leafList.toScalaList() mustBe List(1, 2)
      tree.nodes.head.leafList.toScalaList() mustBe List(3)
      tree.nodes(1).leafList mustBe empty
      tree.nodes(2).leafList mustBe empty
    }
  }
}