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

  def split[T](nodes : Array[RTreeLeaf[T]]) : (RTreeNode[T], RTreeNode[T]) = {
    var vector = new KVector(new Array[Double](0))
    nodes.foreach(node => {
      vector += node.position
    })
    vector.*!(1d/nodes.length)
    java.util.Arrays.sort(nodes, new Comparator[RTreeLeaf[T]]() {
      def compare(l : RTreeLeaf[T], r: RTreeLeaf[T]) : int = {
        val dl = l.position - vector
        val dr = r.position - vector
        Math.signum(dl - dr).asInstanceOf[int]
      }
    })
    val positions = nodes.map(node => node.position)
    var state = new PartitioningKVectorState(positions)
    var istate = state.next()(0)
    istate = PriorityStragety.search(istate)
    if (istate == null) {
      return null
    }
    state = istate.asInstanceOf[PartitioningKVectorState]
    val width = nodes(0).parent.asInstanceOf[RTreeNode[T]].width
    val left  = new RTreeNode[T](width)
    val right = new RTreeNode[T](width)
    for (i <- 0 until nodes.length) {
      if (state.state(i) == -1) {
        left  + nodes(i)
      } else {
        right + nodes(i)
      }
    }
    (left, right)
  }

  def split[T](nodes : Array[RTreeNode[T]]) : (RTreeNode[T], RTreeNode[T]) = {
    var vector = new KVector(new Array[Double](0))
    nodes.foreach(node => {
      vector += node.rectangle.low + node.rectangle.high
    })
    vector.*!(0.5d/nodes.length)
    java.util.Arrays.sort(nodes, new Comparator[RTreeNode[T]]() {
      def compare(l : RTreeNode[T], r: RTreeNode[T]) : int = {
        var dl1 = l.rectangle.low  - vector
        var dl2 = l.rectangle.high - vector
        if (dl2 < dl1) { val t = dl1; dl1 = dl2; dl2 = t }
        var dr1 = r.rectangle.low  - vector
        var dr2 = r.rectangle.high - vector
        if (dr2 < dr1) { val t = dr1; dr1 = dr2; dr2 = t }
        if (dl1 == dr1) {
          dl1 = -dl2
          dr1 = -dr2
        }
        Math.signum(dl1 - dr1).asInstanceOf[int]
      }
    })
    val rects = nodes.map(node => node.rectangle)
    var state = new PartitioningRRectangleState(rects)
    var istate = state.next()(0)
    istate = PriorityStragety.search(istate)
    if (istate == null) {
      return null
    }
    state = istate.asInstanceOf[PartitioningRRectangleState]
    val left  = new RTreeNode[T](nodes(0).width)
    val right = new RTreeNode[T](nodes(0).width)
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
