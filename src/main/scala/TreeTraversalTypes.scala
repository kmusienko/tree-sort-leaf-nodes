import scala.collection.immutable.Queue

object TreeTraversalTypes {

  sealed trait TraversalType {
    def iterator[Node](node: Node, f: Node => Queue[Node]): Iterator[Node]
  }

  case object BreadthFirst extends TraversalType {
    override def iterator[Node](node: Node, f: Node => Queue[Node]): Iterator[Node] = {
      def recurse(q: Queue[Node]): List[Node] = {
        if (q.isEmpty) List.empty
        else {
          val (node, tail) = q.dequeue
          node :: recurse(tail ++ f(node))
        }
      }

      (node :: recurse(Queue.empty ++ f(node))).toIterator
    }
  }
}