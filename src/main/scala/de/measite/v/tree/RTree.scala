package de.measite.v.tree

import java.util.ArrayList

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle

case class RTree[T](width: Int) extends RTreeParent {

  val root = new RTreeNode[T](width)

  root.parent = this

  def +(position: KVector) : RTreeLeaf[T] = {
    val search = new NearestNodeIterator(position, root)
    var result = search.next()
    result + position
  }

  override def toString() : String = {
    "RTree{" + root + "}"
  }

}

