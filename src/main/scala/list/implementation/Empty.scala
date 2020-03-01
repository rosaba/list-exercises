package list.implementation

import list.traits.IntList

case object Empty extends SinglyLinkedIntList {

  override def head: Int = throw new IllegalArgumentException("head.nil")

  override def tail: IntList = throw new IllegalArgumentException("tail.nil")

  override def isEmpty = true

  override def get(index: Int) = throw new IndexOutOfBoundsException()

  override def append(elem: Int): IntList = Cons(elem, this)

  override def contains(elem: Int): Boolean = false

  override def prepend(elem: Int): IntList = Cons(elem, this)

  override def delete(elem: Int): IntList = this

  override def deleteAll(elem: Int): IntList = this

  override def prefix(other: IntList): IntList = other
}