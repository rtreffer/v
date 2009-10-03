package de.measite.v.tree

import java.lang.Math.signum
import java.util.TreeSet

import de.measite.v.data.KVector

case class NearestNodeIterator[T](position: KVector, root: RTreeNode[T]) {

  case class Entry(element: RTreeNode[T], level : int) extends Comparable[Entry] {
    val score : double = element.rectangle.distance(position)
    val area  : double = element.rectangle.diagonal.length2

    override def compareTo(that : Entry) : int = {
      if (this.score != that.score) {
        signum(this.score - that.score).asInstanceOf[int]
      } else
      if (this.level != that.level) {
        signum(that.level - this.level).asInstanceOf[int]
      } else
      if (this.area != that.area) {
        signum(that.area - this.area).asInstanceOf[int]
      } else {
        this.element.rectangle.compareTo(that.element.rectangle)
      }
    }
  }


  val border = new TreeSet[Entry]()

  {
    border.add( new Entry(root, 0) )
  }

  def next() : RTreeNode[T] = {
    while (border.size > 0) {
      val b = border.pollFirst
      if (b.element.isLeafLevel) {
        return b.element
      } else {
        var i = 0
        while (i < b.element.childs) {
          border.add(
            new Entry(
              b.element.child(i).asInstanceOf[RTreeNode[T]],
              b.level + 1
            )
          )
          i += 1
        }
      }
    }
    null
  }

}
