package list.implementation

import list.traits.IntList

case class Cons(head: Int, tail: IntList) extends SinglyLinkedIntList {
  override def isEmpty = false

  override def get(index: Int): Int = index match {
    case 0 => head
    case i => tail.get(i - 1)
  }

  override def append(elem: Int): IntList = Cons(head, tail.append(elem))

  override def contains(elem: Int): Boolean =
    if (elem == head) true
    else tail.contains(elem)

  override def prepend(elem: Int): IntList = Cons(elem, this)

  override def delete(elem: Int): IntList =
    if (elem != head) new Cons(head, tail.delete(elem))
    else tail

  override def deleteAll(elem: Int): IntList =
    if (elem != head) new Cons(head, tail.deleteAll(elem))
    else tail.deleteAll(elem)
}