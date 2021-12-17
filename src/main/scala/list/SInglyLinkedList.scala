package list

object SinglyLinkedList {

  sealed trait Node[A] {

    def fold[R](rightf: ValueNode[A] => R, leftf: EmptyNode[A] => R): R =
      this match {
        case n: ValueNode[A] => rightf(n)
        case e: EmptyNode[A] => leftf(e)
      }

    def toOpt: Option[A] = this.fold(n => Some(n.v), _ => None)

    def isEmpty: Boolean = this.fold(_ => false, _ => true)

    def next: Node[A] = this.fold(n => n.nxt, identity)

    override def toString(): String =
      this.fold(
        n => if (n.nxt.isEmpty) n.v.toString else s"${n.v}, ${n.nxt.toString()}",
        _ => ""
      )

  }

  case class ValueNode[A](v: A, nxt: Node[A]) extends Node[A]
  case class EmptyNode[A]()                   extends Node[A]

  trait SinglyLinkedList[A] extends List[A] {
    val curr: Node[A]
  }

  def apply[A](values: A*): SinglyLinkedList[A] =
    makeSinglyLinkedListFrom(nodesFrom(values: _*))

  def empty[A]: SinglyLinkedList[A] = SinglyLinkedList()

  private def nodesFrom[A](values: A*): Node[A] =
    if (values.isEmpty) EmptyNode() else ValueNode(values.head, nodesFrom(values.tail: _*))

  private def makeSinglyLinkedListFrom[A](nodes: Node[A]): SinglyLinkedList[A] =
    new SinglyLinkedList[A] {

      override val curr: Node[A] = nodes

      override def first: Option[A] = nodes.toOpt

      override def last: Option[A] = {
        def findLast[R](n: Node[A]): Option[A] =
          n.fold(
            n => if (n.nxt.isEmpty) Some(n.v) else findLast(n.nxt),
            _ => None
          )
        findLast(curr)
      }

      override def get(pos: Int): Option[A] = {
        def getVal[A](n: Node[A], p: Int): Option[A] =
          p match {
            case p if (p < 1)  => None
            case p if (p == 1) => n.toOpt
            case _             => getVal(n.next, p - 1)
          }
        getVal(curr, pos)
      }

      override def addFirst(ele: A): List[A] =
        makeSinglyLinkedListFrom(
          curr.fold(n => ValueNode(ele, n), e => ValueNode(ele, EmptyNode()))
        )

      override def addLast(ele: A): List[A] = {
        def addToLast(node: Node[A]): Node[A] =
          node.fold(
            n => ValueNode(n.v, addToLast(n.nxt)),
            e => ValueNode(ele, EmptyNode())
          )
        makeSinglyLinkedListFrom(addToLast(curr))
      }

      override def addAt(ele: A, pos: Int): List[A] = {
        if (pos < 0) return this
        def findNode(n: Node[A], p: Int): Node[A] =
          n.fold(
            node =>
              p match {
                case 0 => ValueNode(ele, node.nxt)
                case _ => ValueNode(node.v, findNode(node, p - 1))
              },
            e => ValueNode(ele, EmptyNode())
          )
        makeSinglyLinkedListFrom(findNode(curr, pos - 1))
      }

      override def deleteFirst: List[A] = makeSinglyLinkedListFrom(curr.next)

      override def deleteLast: List[A] = {
        def delLast(node: Node[A]): Node[A] =
          node match {
            case ValueNode(v, nxt) if (nxt == EmptyNode()) => EmptyNode()
            case ValueNode(v, nxt)                         => ValueNode(v, delLast(nxt))
            case _                                         => EmptyNode()
          }
        makeSinglyLinkedListFrom(delLast(curr))
      }

      override def deleteAt(pos: Int): List[A] = {
        if (pos < 0) return this
        def delAt(n: Node[A], p: Int): Node[A] =
          n.fold(
            node =>
              p match {
                case 0 => node.nxt
                case _ => ValueNode(node.v, delAt(node.nxt, p - 1))
              },
            identity
          )
        makeSinglyLinkedListFrom(delAt(curr, pos - 1))
      }

      override def lenght: Int = {
        def count(node: Node[A]): Int = node.fold(n => 1 + count(n.nxt), _ => 0)
        count(curr)
      }

      override def equals(that: Any): Boolean =
        that match {
          case l: SinglyLinkedList[_] =>
            l.curr.toOpt == this.curr.toOpt && l.curr.next == this.curr.next
          case _ => false
        }

      override def toString(): String =
        curr.fold(n => s"SinglyLinkedList(${n.toString})", e => "EmptyList()")

    }

}
