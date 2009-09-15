package de.measite.v.tree

import java.util.ArrayList
import java.util.Iterator

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle
import de.measite.v.tree.partitioner.Partitioner

/**
 * The RTree root class. Mainly intaracting on the root node.
 */
case class RTree[T](width: Int, partitioner: Partitioner) extends RTreeParent {

  /**
   * The root node of this tree.
   */
  val root = new RTreeNode[T](width, partitioner)

  {
    root.parent = this
  }

  /**
   * Add a new position by finding the nearest node and adding
   * the requested element.
   */
  def +(position: KVector) : RTreeLeaf[T] = {
    val search = new NearestNodeIterator(position, root)
    search.next() + position
  }

  def search(position: KVector) : Iterator[RTreeLeaf[T]] = {
    new NearestLeafIterator(position, root)
  }

  /**
   * Return a string representation of this object instance.
   */
  override def toString() : String = {
    "RTree{" + root + "}"
  }

}

