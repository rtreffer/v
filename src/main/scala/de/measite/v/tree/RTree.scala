package de.measite.v.tree

import java.util.ArrayList
import java.util.Iterator

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle
import de.measite.v.tree.partitioner.Partitioner
import de.measite.v.searchtree.PriorityIterator
import de.measite.v.measure.SearchStatistic

/**
 * The RTree root class. Mainly intaracting on the root node.
 */
class RTree[T](
  width: Int,
  partitioner: Partitioner
) extends RTreeParent {

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
  def +(
    position : KVector,
    stats    : SearchStatistic
  ) : RTreeLeaf[T] = {
    if (stats ne null) { stats.start = System.currentTimeMillis }
    val search = new PriorityIterator[RTreeNode[T], RTreeNode[T]](
      (element) => { // terminal
        if (element.isLeafLevel) { element } else { null }
      },
      (element) => { // expand
        if (stats ne null) {
          stats.visits  += element.childs
          stats.expands += 1
        }
        val result = new Array[RTreeNode[T]](element.childs)
        var i = 0;
        while (i < result.length) {
          result(i) = element.child(i).asInstanceOf[RTreeNode[T]]
          i += 1
        }
        result
      },
      (element) => { // score
        Array(
          element.rectangle.distance(position),
          element.rectangle. low.distance(position) +
          element.rectangle.high.distance(position)
        )
      },
      root
    )
    search.next() + position
  }
  def +(position : KVector) : RTreeLeaf[T] = this.+(position, null)

  def search(position : KVector) : Iterator[RTreeLeaf[T]]
    = search(position, null)

  def search(
    position : KVector,
    stats    : SearchStatistic
  ) : Iterator[RTreeLeaf[T]] = {
    if (stats ne null) { stats.start = System.currentTimeMillis }
    new PriorityIterator[RTreeElement, RTreeLeaf[T]](
      (element) => { // terminal
        if (element.isInstanceOf[RTreeLeaf[T]]) {
          element.asInstanceOf[RTreeLeaf[T]]
        } else {
          null
        }
      },
      (element) => { // expand
        val node = element.asInstanceOf[RTreeNode[T]]
        if (stats ne null) {
          stats.visits  += node.childs
          stats.expands += 1
        }
        node.child
      },
      (element) => { // score
        element match {
          case leaf : RTreeLeaf[T] => {
            Array(
              leaf.position.distance(position),
              0d
            )
          }
          case node : RTreeNode[T] => {
            Array(
              node.rectangle.distance(position),
              node.rectangle. low.distance(position) +
              node.rectangle.high.distance(position)
            )
          }
        }
      },
      root
    )
  }

  def optimize() : Unit = {
    root.optimize()
  }

  /**
   * Return a string representation of this object instance.
   */
  override def toString() : String = {
    "RTree{" + root + "}"
  }

}

