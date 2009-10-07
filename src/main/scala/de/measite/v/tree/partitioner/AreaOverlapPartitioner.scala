package de.measite.v.tree.partitioner

import de.measite.v.data.{KVector, RRectangle}
import de.measite.v.tree.{RTreeLeaf, RTreeNode}
import de.measite.v.searchtree.PriorityStragety
import java.util.Comparator
import java.util.ArrayList

object AreaOverlapPartitioner extends Partitioner {

  /*
   * :: ALGORITHM ::
   * 
   * (1) Detect the center of the rects/points
   * ---- This is the point where most cuts will occur
   * ---- Order all points by (distance to center asc),(area desc)
   * (2) Create the initial state (search tree stat)
   * ---- Basis for stragety
   * (3) return the optimal solution
   *
   */

  def split[T](
    nodes   : Array[RTreeLeaf[T]],
    current : RTreeNode[T],
    score   : Double
  ) : (RTreeNode[T], RTreeNode[T]) = {
    val positions = nodes.map(node => node.position)
    var state = new PartitioningKVectorState(positions)
    var istate = state.next()(0)
    istate = PriorityStragety.search(istate)
    if (istate eq null) {
      return null
    }
    state = istate.asInstanceOf[PartitioningKVectorState]
    val left  = new RTreeNode[T](current.tree)
    val right = new RTreeNode[T](current.tree)
    for (i <- 0 until nodes.length) {
      if (state.state(i) == -1) {
        left  + nodes(i)
      } else {
        right + nodes(i)
      }
    }
    (left, right)
  }

  def split[T](
    nodes   : Array[RTreeNode[T]],
    current : RTreeNode[T],
    score   : Double
  ) : (RTreeNode[T], RTreeNode[T]) = {
    val rects = nodes.map(node => node.rectangle)
    var state = new PartitioningRRectangleState(rects)
    var istate = state.next()(0)
    istate = PriorityStragety.search(istate)
    if (istate eq null) {
      return null
    }
    state = istate.asInstanceOf[PartitioningRRectangleState]
    val left  = new RTreeNode[T](current.tree)
    val right = new RTreeNode[T](current.tree)
    for (i <- 0 until nodes.length) {
      if (state.state(i) == -1) {
        left  + nodes(i)
      } else {
        right + nodes(i)
      }
    }
    (left, right)
  }

}
