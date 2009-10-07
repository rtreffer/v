package de.measite.v.tree.partitioner

import de.measite.v.data.{KVector, RRectangle}
import de.measite.v.tree.{RTreeLeaf, RTreeNode}
import de.measite.v.searchtree.AbstractPriorityIterator
import java.util.Comparator
import java.util.ArrayList

class KVectorPriorityIterator(__start: HotspotKVectorState)
  extends AbstractPriorityIterator[HotspotKVectorState, HotspotKVectorState]
{
  def start               = __start
  def terminal(
    element : HotspotKVectorState
  ) : HotspotKVectorState = {
    if (element.isTerminal && element.isValid) { element } else { null }
  }
  def expand(
    element : HotspotKVectorState
  ) : Array[HotspotKVectorState] = {
    element.next
  }
  def score(
    element : HotspotKVectorState
  ) = {
    Array(element.score, element.pos.asInstanceOf[Double])
  }
}

/**
 * A hotspot partitioner computes the propability that a query stays
 * in one of the new child nodes. It is assumed that data and queries
 * are somewhat equaly distributed, thus data hotspos are query hotspots,
 * too.
 */
object HotspotPartitioner extends Partitioner {

  def split[T](
    nodes   : Array[RTreeLeaf[T]],
    current : RTreeNode[T],
    score   : Double
  ) : (RTreeNode[T], RTreeNode[T]) = {
    val positions = new Array[KVector](nodes.length)
    var i = 0
    while (i < positions.length) {
      positions(i) = nodes(i).position
      i += 1
    }
    var state  = new HotspotKVectorState(positions, current.rectangle)
    val iter   = new KVectorPriorityIterator(state.next()(0))
        state  = iter.next
    var bestScore = state.score
    var bestState = state
    while (state != null && state.score > bestScore) {
      val sscore = state.exactScore
      if (sscore > bestScore) {
        bestScore = sscore
        bestState = state
      }
      state = iter.next
    }
    state = bestState
    val left  = new RTreeNode[T](current.tree)
    val right = new RTreeNode[T](current.tree)
    i = 0
    while (i < nodes.length) {
      val side = if (state.state(i) == -1) { left } else { right }
          side + nodes(i)
      i += 1
    }
    (left, right)
  }

  def split[T](
    nodes   : Array[RTreeNode[T]],
    current : RTreeNode[T],
    score   : Double
  ) : (RTreeNode[T], RTreeNode[T]) = {
    QuadraticPartitioner.split(nodes, current, score)
  }

}

