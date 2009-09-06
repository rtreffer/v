package de.measite.v.tree

import java.lang.UnsupportedOperationException
import java.util.TreeMap

import de.measite.v.data.KVector

class NearestLeafIterator[T](
  position: KVector,
  root: RTreeNode[T]
) extends java.util.Iterator[RTreeLeaf[T]] {

    private val best = new TreeMap[Double, RTreeLeaf[T]]()
    private val border = new TreeMap[Double, RTreeNode[T]]()
    private var _next : RTreeLeaf[T] = _
    private var _cur  : RTreeLeaf[T] = _
    private var _hasNext = true

    {
      border.put(root.rectangle.distance(position), root)
    }

    def hasNext() : boolean = {
      if (!hasNext) {
        false
      } else {
        if (_next eq null) { _next = inext() }
        if (_next eq null) {
          _hasNext = false
        }
        _hasNext
      }
    }

    def next() : RTreeLeaf[T] = {
      if (!hasNext) {
        throw new IllegalStateException
      }
      if (_next eq null) { _next = inext() }
      _cur  = _next
      _next = null
      _cur
    }

    def remove() : Unit = {
      throw new UnsupportedOperationException
    }

    def inext() : RTreeLeaf[T] = {
      while (border.size > 0
         && (best.size == 0 || border.firstKey < best.firstKey)
      ) {
        val expandKey = border.firstKey
        val expandNode = border.get(expandKey)
        border.remove(expandKey)
        if (expandNode.isLeafLevel) {
          var i = 0;
          while (i < expandNode.childs && expandNode.child(i) != null) {
              val child = expandNode.child(i).asInstanceOf[RTreeLeaf[T]]
              best.put(
                child.position - position,
                child
              )
          }
        } else {
          var i = 0;
          while (i < expandNode.childs && expandNode.child(i) != null) {
            val child = expandNode.child(i).asInstanceOf[RTreeNode[T]]
            border.put(
              child.rectangle.distance(position),
              child
            )
            i += 1
          }
        }
      }
      if (best.size == 0) {
        null
      } else {
        val key = best.firstKey
        val node = best.get(key)
        best.remove(key)
        node
      }
    }

}
