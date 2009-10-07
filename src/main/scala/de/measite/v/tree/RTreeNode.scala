package de.measite.v.tree

import java.util.ArrayList

import de.measite.v.data.RRectangle
import de.measite.v.data.KVector
import de.measite.v.tree.partitioner.Partitioner

/**
 * A tree node, the most intelligent class within the tree.
 */
class RTreeNode[T](
  __tree  : RTree[T]
) extends  RTreeParent
  with     RTreeElement[T] {

  tree = __tree

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
  var child = new Array[RTreeElement[T]](__tree.width + 1)

  var maxChilds = __tree.width

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

  def foreachNode( node: (Int, RTreeNode[T]) => Unit ) : Unit = {
    foreach( (p, leaf) => { }, node )
  }

  def foreachLeaf( leaf: (Int, RTreeLeaf[T]) => Unit ) : Unit = {
    foreach( leaf, (p, node) => { } )
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
    val result = new RTreeLeaf[T](position, tree)
    this + result
    result
  }

  /**
   * Add a child element at this level.
   */
  def +(c: RTreeElement[T]) : Unit = {
    child(childs) = c
    c.parent = this
    childs += 1
    c match {
      case leaf : RTreeLeaf[T] => { chainBounds(leaf.position ) }
      case node : RTreeNode[T] => { chainBounds(node.rectangle) }
    }
    if (childs >= maxChilds) { split() }
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
    val rect = rectangle + position
    if (rect eq rectangle) { return }
    rectangle = rect
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
    val rect = rectangle + position
    if (rect eq rectangle) { return }
    rectangle = rect
    if (isRoot) {
      return
    }
    if (parent == null) {
      return
    }
    parent.asInstanceOf[RTreeNode[T]].chainBounds(position)
  }

  def innerOverlap() : double = {
    if (isLeafLevel) {
      0d
    } else {
      var result = 0d
      for (i <- 0 until childs) {
        val l = child(i).asInstanceOf[RTreeNode[T]].rectangle
        for (j <- (i + 1) until childs) {
          val r = child(j).asInstanceOf[RTreeNode[T]].rectangle
          result += l.intersection(r).area1p
        }
      }
      result
    }
  }

  /**
   * Split logic, in case this node overflows.
   */
  def split() : Unit = {
    val nodes =
      if (isLeafLevel) {
        var leafs = new Array[RTreeLeaf[T]](childs)
        System.arraycopy(child, 0, leafs, 0, childs)
        tree.partitioner.split[T](leafs, this, 0d)
      } else {
        var node = new Array[RTreeNode[T]](childs)
        System.arraycopy(child, 0, node, 0, childs)
        tree.partitioner.split[T](node, this, 0d)
      }
    if (isRoot) {
      splitRoot(nodes._1, nodes._2)
    } else {
      splitReplace(nodes._1, nodes._2)
    }
  }

  def grow(
    _width: Int
  ) : Unit = {
    val _child = new Array[RTreeElement[T]](_width + 1)
    System.arraycopy(child, 0, _child, 0, child.length)
    child = _child
    maxChilds = _width
  }

  private def splitRoot(
    left  : RTreeNode[T],
    right : RTreeNode[T]
  ) : Unit = {
    var i = 0
    while (i < childs) {
      child(i) = null
      i += 1
    }
    childs = 0
    this + left
    this + right
  }

  private def splitReplace(
    left  : RTreeNode[T],
    right : RTreeNode[T]
  ) : Unit = {
    if ( parent == null ) { return }
    val p = parent.asInstanceOf[RTreeNode[T]]
    p - this
    p + left
    p + right
  }

}
