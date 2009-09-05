package de.measite.v.tree

import de.measite.v.data.RRectangle
import de.measite.v.data.KVector
import de.measite.v.tree.partitioner.AreaOverlapPartitioner

/**
 * A tree node, the most intelligent class within the tree.
 */
case class RTreeNode[T](width: Int)
  extends  RTreeParent
  with     RTreeElement {

  /**
   * The bounding box of this subtree.
   */
  var rectangle = new RRectangle()
  /**
   * The number of non-null children
   */
  var childs = 0
  /**
   * The child array
   */
  val child = new Array[RTreeElement](width + 1)

  /**
   * A simple foreach facility. This method hides the leaflevel complexity.
   */
  def foreach(
    leaf: (Int, RTreeLeaf[T]) => Unit,
    node: (Int, RTreeNode[T]) => Unit
  ) : Unit =  {
    if (isLeafLevel) {
      for (i <- 0 until childs) {
        leaf(i, child(i).asInstanceOf[RTreeLeaf[T]])
      }
    } else {
      for (i <- 0 until childs) {
        node(i, child(i).asInstanceOf[RTreeNode[T]])
      }
    }
  }

  /**
   * Check if this node is the root node.
   */
  def isRoot() : Boolean = {
    parent != null && parent.isInstanceOf[RTree[T]]
  }

  /**
   * Check if this node is the leaf level node.
   */
  def isLeafLevel : Boolean = {
    (childs == 0) || child(0).isInstanceOf[RTreeLeaf[_]]
  }

  /**
   * Add a now leaf node at this level.
   */
  def +(position: KVector) : RTreeLeaf[T] = {
    val result = new RTreeLeaf[T](position)
    this + result
    result
  }

  /**
   * Add a child element at this level.
   */
  def +(c: RTreeElement) : Unit = {
    child(childs) = c
    c.parent = this
    childs += 1
    c match {
      case leaf : RTreeLeaf[T] => { chainBounds(leaf.position ) }
      case node : RTreeNode[T] => { chainBounds(node.rectangle) }
    }
    if (childs > width) { split() }
  }

  /**
   * Remove a child from this level. Checks the elments by reference.
   */
  def -(node: RTreeNode[T]) : Unit = {
    for (i <- 0 until childs) {
      if (child(i) eq node) {
        child(i) = child(childs - 1)
        child(childs - 1) = null
        childs -= 1
      }
    }
  }

  /**
   * Recursively update the bounding box.
   */
  private def chainBounds(position: KVector) {
    if (rectangle contains position) {
      return
    }
    rectangle = rectangle + position
    if (isRoot) {
      return
    }
    if (parent == null) {
      return
    }
    parent.asInstanceOf[RTreeNode[T]].chainBounds(position)
  }

  /**
   * Recursively update the bounding box.
   */
  private def chainBounds(position: RRectangle) {
    if (rectangle contains position) {
      return
    }
    rectangle += position
    if (isRoot) {
      return
    }
    if (parent == null) {
      return
    }
    parent.asInstanceOf[RTreeNode[T]].chainBounds(position)
  }

  /**
   * Split logic, in case this node overflows.
   */
  def split() : Unit = {
    val nodes =
      if (isLeafLevel) {
        var c = new Array[RTreeLeaf[T]](childs)
        System.arraycopy(child, 0, c, 0, childs)
        AreaOverlapPartitioner.split[T](c)
      } else {
        var c = new Array[RTreeNode[T]](childs)
        System.arraycopy(child, 0, c, 0, childs)
        AreaOverlapPartitioner.split[T](c)
      }
    if (isRoot) {
      splitRoot(nodes._1, nodes._2)
    } else {
      splitReplace(nodes._1, nodes._2)
    }
  }

  private def splitRoot(
    left  : RTreeNode[T],
    right : RTreeNode[T]
  ) : Unit = {
    for (i <- 0 until childs) {
      child(i) = null
    }
    childs = 0
    this + left
    this + right
  }

  private def splitReplace(
    left  : RTreeNode[T],
    right : RTreeNode[T]
  ) : Unit = {
    val p = parent.asInstanceOf[RTreeNode[T]]
    p - this
    p + left
    p + right
  }

}
