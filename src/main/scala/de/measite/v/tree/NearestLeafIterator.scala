package de.measite.v.tree

import java.lang.UnsupportedOperationException
import java.util.Comparator
import java.util.TreeSet

import de.measite.v.data.KVector

class NearestLeafIterator[T](
  position: KVector,
  root: RTreeNode[T]
) extends java.util.Iterator[RTreeLeaf[T]]
  with    Comparator[Any]
{

    private val border = new TreeSet[RTreeElement](this)
    private var _next : RTreeLeaf[T] = _
    private var _cur  : RTreeLeaf[T] = _
    private var _hasNext = true

    {
      border.add(root)
    }

    def hasNext() : boolean = {
      if (!_hasNext) {
        false
      } else {
        if (_next eq null) { _next = inext()  }
        if (_next eq null) { _hasNext = false }
        _hasNext
      }
    }

    def next() : RTreeLeaf[T] = {
      if (!_hasNext) {
        throw new IllegalStateException
      }
      if (_next eq null) {
        _cur = inext()
        _hasNext = (_cur ne null)
      } else {
        _cur  = _next
        _next = null
      }
      _cur
    }

    def remove() : Unit = {
      throw new UnsupportedOperationException
    }

    private def inext() : RTreeLeaf[T] = {
      while (border.size > 0) {
        val expand = border.pollFirst
        if (expand.isInstanceOf[RTreeLeaf[T]]) {
          return expand.asInstanceOf[RTreeLeaf[T]]
        } else {
          val node = expand.asInstanceOf[RTreeNode[T]]
          node.child.foreach(
            c => { if (c ne null) {
              border.add( c )
            }}
          )
        }
      }
      null
    }

  override def compare(x: Any, y: Any) : int = {
    val bx = x.isInstanceOf[RTreeNode[T]]
    val dx =
      if (bx) {
        x.asInstanceOf[RTreeNode[T]].rectangle.distance(position)
      } else {
        x.asInstanceOf[RTreeLeaf[T]].position - position
      }
    val by = y.isInstanceOf[RTreeNode[T]]
    val dy =
      if (by) {
        y.asInstanceOf[RTreeNode[T]].rectangle.distance(position)
      } else {
        y.asInstanceOf[RTreeLeaf[T]].position - position
      }
    if (dx != dy) {
      Math.signum(dx - dy).asInstanceOf[int]
    } else {
      if (bx) {
        if (by) {
          x.asInstanceOf[RTreeNode[T]].rectangle.compareTo(
            y.asInstanceOf[RTreeNode[T]].rectangle
          )
        } else {
          -1
        }
      } else {
        if (by) {
           1
        } else {
          x.asInstanceOf[RTreeLeaf[T]].position.compareTo(
            y.asInstanceOf[RTreeLeaf[T]].position
          )
        }
      }
    }
  }

}
