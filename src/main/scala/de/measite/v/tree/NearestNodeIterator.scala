package de.measite.v.tree

import java.lang.Math.signum
import java.util.TreeSet

import de.measite.v.data.KVector

case class NearestNodeIterator[T](position: KVector, root: RTreeNode[T]) {

  case class Entry(element: RTreeNode[T]) extends Comparable[Entry] {
    val score : double = element.rectangle.distance(position)
    val area  : double = element.rectangle.area1p

    override def compareTo(that : Entry) : int = {
      if (this.score != that.score) {
        signum(this.score - that.score).asInstanceOf[int]
      } else {
        if (this.area != that.area) {
          signum(that.area - this.area).asInstanceOf[int]
        } else {
          this.element.rectangle.compareTo(that.element.rectangle)
        }
      }
    }
  }


  val border = new TreeSet[Entry]()

  {
    border.add( new Entry(root) )
  }

  def next() : RTreeNode[T] = {
    while (border.size > 0) {
      val element = border.pollFirst.element
      if (element.isLeafLevel) {
        return element
      } else {
        var i = 0
        while (i < element.childs) {
          border.add(
            new Entry(
              element.child(i).asInstanceOf[RTreeNode[T]]
            )
          )
          i += 1
        }
      }
    }
    null
  }

}
