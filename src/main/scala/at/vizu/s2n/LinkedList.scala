package at
package vizu
package s2n

/**
  * Phil on 13.01.16.
  */
class LinkedList[T] {
  private var first: Link[T] = null

  def isEmpty: Boolean = first == null

  def prepend(data: T): Unit = {
    val link = new Link(data)
    link.next = first
    first = link
  }

  def reverseMap[U](f: T => U): LinkedList[U] = {
    val l = new LinkedList[U]
    var currentLink = first
    while (currentLink != null) {
      l.prepend(f(currentLink.data))
      currentLink = currentLink.next
    }
    l
  }
}

class Link[T](val data: T) {
  var next: Link[T] = null

  override def toString: String = {
    data.toString
  }
}