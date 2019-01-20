import scala.collection.immutable.Queue

/**
  * An object where tree traversal types are defined.
  */
object TreeTraversalTypes {

  /**
    * Trait that declares an iterator by which a tree can be traversed.
    */
  sealed trait TraversalType {

    /**
      * Returns an iterator by which a tree can be traversed.
      *
      * @param node tree node to start traverse from
      * @param f    function that takes a parent Node as an argument and returns a Queue containing all its children
      * @tparam Node type of the tree node
      * @return iterator by which a tree can traversed.
      */
    def iterator[Node](node: Node, f: Node => Queue[Node]): Iterator[Node]
  }

  /**
    * The Breadth First Traversal type with provided iterator.
    */
  case object BreadthFirst extends TraversalType {

    /** @inheritdoc*/
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