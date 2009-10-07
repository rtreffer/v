package de.measite.v.tree

import de.measite.v.data.KVector

/**
 * A RTree data leaf.
 */
class RTreeLeaf[T](
  __position : KVector,
  __tree     : RTree[T]
) extends RTreeElement[T] {

  tree = __tree

  val position = __position

  /**
   * The data payload.
   */
  var data: T = _

  /**
   * Create a new data node with a custom payload.
   */
  def this(position : KVector, _tree: RTree[T], _data : T) {
    this(position, _tree)
    data = _data
    this
  }

  /**
   * Implicit cast to the underlaying data type
   */
  def coerce : T = {
    data
  }

}
