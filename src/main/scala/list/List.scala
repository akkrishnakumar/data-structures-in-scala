package list

trait List[A] {

  def first: Option[A]
  def last: Option[A]
  def get(pos: Int): Option[A]

  def addFirst(ele: A): List[A]
  def addLast(ele: A): List[A]
  def addAt(ele: A, pos: Int): List[A]

  def deleteFirst: List[A]
  def deleteLast: List[A]
  def deleteAt(pos: Int): List[A]

  def lenght: Int

}
