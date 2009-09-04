package de.measite.v.tree

import java.util.ArrayList

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle

/**
 * The RTree root class. Mainly intaracting on the root node.
 */
case class RTree[T](width: Int) extends RTreeParent {

  /**
   * The root node of this tree.
   */
  val root = new RTreeNode[T](width)

  {
    root.parent = this
  }

  /**
   * Add a new position by finding the nearest node and adding
   * the requested element.
   */
  def +(position: KVector) : RTreeLeaf[T] = {
    val search = new NearestNodeIterator(position, root)
    var result = search.next()
    result + position
  }

  /**
   * Return a string representation of this object instance.
   */
  override def toString() : String = {
    "RTree{" + root + "}"
  }

}

