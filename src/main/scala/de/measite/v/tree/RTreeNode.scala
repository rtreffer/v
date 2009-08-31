package de.measite.v.tree

import de.measite.v.data.RRectangle
import de.measite.v.data.KVector
import de.measite.v.tree.partitioner.AreaOverlapPartitioner

case class RTreeNode[T](width: Int) extends RTreeParent with RTreeElement {

  var rectangle = new RRectangle()
  var childs = 0
  var child = new Array[RTreeElement](width + 1)

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

  def +(position: KVector) : RTreeLeaf[T] = {
    val result = new RTreeLeaf[T](position)
    this + result
    result
  }

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

  def -(node: RTreeNode[T]) : Unit = {
    for (i <- 0 until childs) {
      if (child(i) eq node) {
        child(i) = child(childs - 1)
        child(childs - 1) = null
        childs -= 1
        return
      }
    }
  }

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

  private def recomputeRectangle() : Unit = {
    rectangle = new RRectangle()
    foreach(
      (i, l) => { rectangle += l.position  },
      (i, n) => { rectangle += n.rectangle }
    )
  }

  def split() : Unit = {
    val newNodes =
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
      for (i <- 0 until childs) {
        child(i) = null
      }
      childs = 0
      this + newNodes._1
      this + newNodes._2
      return
    }
    val p = parent.asInstanceOf[RTreeNode[T]]
    p - this
    p + newNodes._1
    p + newNodes._2
  }

}
