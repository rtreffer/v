package de.measite.v.tree.partitioner

import java.util.Random
import java.util.BitSet
import java.util.ArrayList

import de.measite.v.data.{KVector,  RRectangle}
import de.measite.v.tree.{RTreeLeaf, RTreeNode}

object QuadraticPartitioner extends Partitioner {

  def split[T](
    nodes   : Array[RTreeLeaf[T]],
    current : RTreeNode[T],
    score   : Double
  ) : (RTreeNode[T], RTreeNode[T]) = {
    val set = new BitSet(nodes.length)
    var maxArea  = new RRectangle(nodes(0).position, nodes(1).position).area1p
    var maxLeft  = 0
    var maxRight = 1
    var i = 0
    while (i < nodes.length) {
      var j = i + 1
      while (j < nodes.length) {
        val area = new RRectangle(nodes(i).position, nodes(j).position).area1p 
        if (area > maxArea) {
          maxArea  = area
          maxLeft  = i
          maxRight = j
        }
        j += 1
      }
      i += 1
    }
    set.set(maxLeft )
    set.set(maxRight)
    val lefts  = new RTreeNode[T](current.tree)
    val rights = new RTreeNode[T](current.tree)
    var leftPos   = nodes(maxLeft) .position
    var rightPos  = nodes(maxRight).position
    lefts  + nodes(maxLeft )
    rights + nodes(maxRight)
    var limit = (nodes.length + 1) / 2
    while (lefts.childs <= limit && rights.childs <= limit) {
      if (lefts.childs == limit) {
        var i = 0
        while (i < nodes.length) {
          if (!set.get(i)) { rights + nodes(i) }
          i += 1
        }
        limit = 0
      } else
      if (rights.childs == limit) {
        var i = 0
        while (i < nodes.length) {
          if (!set.get(i)) { lefts  + nodes(i) }
          i += 1
        }
        limit = 0
      } else {
        val lpre = lefts .rectangle.diagonal.length2
        val rpre = rights.rectangle.diagonal.length2
        var best = -1
        var dir  = 0
        var bestScore = java.lang.Double.MAX_VALUE
        var i = 0
        while (i < nodes.length) {
          if (!set.get(i)) {
            if (best == -1) { best = i }
            val node = nodes(i)
            val li = (lefts .rectangle + node.position).diagonal.length2 - lpre
            if (li < bestScore) { dir = -1 ;  best = i ; bestScore = li }
            val ri = (rights.rectangle + node.position).diagonal.length2 - rpre
            if (ri < bestScore) { dir =  1 ;  best = i ; bestScore = ri }
          }
          i += 1
        }
        if (best == -1) {
          limit = -1
        } else {
          set.set(best)
          if (dir == -1) {
            lefts + nodes(best)
          } else {
            rights + nodes(best)
          }
        }
      }
    }
    (lefts, rights)
  }

  override def split[T](
    nodes   : Array[RTreeNode[T]],
    current : RTreeNode[T],
    score   : Double
  ) : (RTreeNode[T], RTreeNode[T]) = {
    val set = new BitSet(nodes.length)
    var maxArea  = (nodes(0).rectangle + nodes(1).rectangle).area1p
    var maxLeft  =  0
    var maxRight =  1
    var i = 0
    while (i < nodes.length) {
      var j = 0
      while (j < nodes.length) {
        val area = (nodes(i).rectangle + nodes(j).rectangle).area1p
        if (area > maxArea) {
          maxArea  = area
          maxLeft  = i
          maxRight = j
        }
        j += 1
      }
      i += 1
    }
    set.set(maxLeft )
    set.set(maxRight)
    val lefts  = new RTreeNode[T](current.tree)
    val rights = new RTreeNode[T](current.tree)
    var leftRect  = nodes(maxLeft) .rectangle
    var rightRect = nodes(maxRight).rectangle
    var leftArea  = leftRect.area1p
    var rightArea = rightRect.area1p
    lefts  + nodes(maxLeft )
    rights + nodes(maxRight)
    var limit = (nodes.length + 1) / 2
    while (lefts.childs <= limit && rights.childs <= limit) {
      if (lefts.childs == limit - 1) {
        var i = 0
        while (i < nodes.length) {
          if (!set.get(i)) { rights + nodes(i) }
          i += 1
        }
        limit = 0
      } else
      if (rights.childs == limit - 1) {
        var i = 0
        while (i < nodes.length) {
          if (!set.get(i)) { lefts  + nodes(i) }
          i += 1
        }
        limit = 0
      } else {
        val lp = leftRect .diagonal.length2
        val rp = rightRect.diagonal.length2
        var best = -1
        var dir  = 0
        var bestScore = java.lang.Double.MAX_VALUE
        var i = 0
        while (i < nodes.length) {
          if (!set.get(i)) {
            if (best == -1) { best = i }
            val node = nodes(i)
            val li = (leftRect + node.rectangle).diagonal.length2 - lp
            if (li < bestScore) { dir = -1 ;  best = i ; bestScore = li }
            val ri = (rightRect + node.rectangle).diagonal.length2 - rp
            if (ri < bestScore) { dir =  1 ;  best = i ; bestScore = ri }
          }
          i += 1
        }
        if (best == -1) {
          best = 0
        } else {
          set.set(best)
          if (dir == -1) {
            lefts + nodes(best)
            leftRect = leftRect + nodes(best).rectangle
            leftArea = leftRect.area1p - 1d
          } else {
            rights + nodes(best)
            rightRect = rightRect + nodes(best).rectangle
            rightArea = rightRect.area1p - 1d
          }
        }
      }
    }
    (lefts, rights)
  }

}
