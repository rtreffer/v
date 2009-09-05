package de.measite.v.tree

import de.measite.v.data.KVector

/**
 * A RTree data leaf.
 */
case class RTreeLeaf[T](position: KVector) extends RTreeElement {

  /**
   * The data payload.
   */
  var data: T = _

  /**
   * Create a new data node with a custom payload.
   */
  def this(position: KVector, d: T) {
    this(position)
    data = d
    this
  }

  /**
   * Implicit cast to the underlaying data type
   */
  def coerce : T = {
    data
  }

}
