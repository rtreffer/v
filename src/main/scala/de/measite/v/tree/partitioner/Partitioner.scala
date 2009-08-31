package de.measite.v.tree.partitioner

import de.measite.v.tree.{RTreeLeaf, RTreeNode}

/**
 * A partitioner is a simple interface to split a leaf or inner node based
 * on various heuristics.
 */
trait Partitioner {

  /**
   * Split leaf content into two new nodes
   */
  def split[T](nodes : Array[RTreeLeaf[T]]) : (RTreeNode[T], RTreeNode[T])

  /**
   * Split node content into two new nodes
   */
  def split[T](nodes : Array[RTreeNode[T]]) : (RTreeNode[T], RTreeNode[T])

}
