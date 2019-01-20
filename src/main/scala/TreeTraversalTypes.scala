import scala.collection.immutable.Queue

object TreeTraversalTypes {

  sealed trait TraversalType {
    def iterator[Node](node: Node, f: Node => Queue[Node]): Iterator[Node]
  }
}