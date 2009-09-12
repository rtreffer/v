package de.measite.v.tree

import java.lang.UnsupportedOperationException
import java.lang.Math.signum
import java.util.Comparator
import java.util.TreeSet

import de.measite.v.data.KVector

class NearestLeafIterator[T](
  position: KVector,
  root: RTreeNode[T]
) extends java.util.Iterator[RTreeLeaf[T]] {

    case class Entry(element: RTreeElement) extends Comparable[Entry] {
      val node  : boolean = element.isInstanceOf[RTreeNode[T]]
      val score : double  =
        if (node) {
          element.asInstanceOf[RTreeNode[T]].rectangle.distance(position)
        } else {
          element.asInstanceOf[RTreeLeaf[T]].position.distance(position)
        }
      override def compareTo(that : Entry) : int = {
        if (this.score != that.score) {
          return signum(this.score - that.score).asInstanceOf[int]
        }
        if (!node) {
          if (!that.node) {
            element.asInstanceOf[RTreeLeaf[T]].position.compareTo(
              that.element.asInstanceOf[RTreeLeaf[T]].position
            )
          } else {
            -1
          }
        } else {
          if (!that.node) {
             1
          } else {
            val l = this.element.asInstanceOf[RTreeNode[T]]
            val r = that.element.asInstanceOf[RTreeNode[T]]
            l.rectangle.compareTo(r.rectangle)
          }
        }
      }
    }

    private val border = new TreeSet[Entry]()
    private var _next : RTreeLeaf[T] = _
    private var _cur  : RTreeLeaf[T] = _
    private var _hasNext = true

    {
      border.add(new Entry(root))
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
      var i = 0;
      while (border.size > 0) {
        i += 1
        val expand = border.pollFirst
        if ( !expand.node ) {
          return expand.element.asInstanceOf[RTreeLeaf[T]]
        } else {
          val node = expand.element.asInstanceOf[RTreeNode[T]]
          node.foreach(
            (p, c) => { border.add(new Entry(c)) },
            (p, c) => { border.add(new Entry(c)) }
          )
        }
      }
      null
    }

}
