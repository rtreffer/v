package de.measite.v.searchtree

import java.lang.UnsupportedOperationException

import java.util.TreeSet
import java.util.Comparator
import java.util.Iterator

/**
 * A Priority Iterater is a priority based iterator implementation.
 * Priorities (scores), isTerminal and expansion are defined as
 * functions..
 */
trait AbstractPriorityIterator[V <: AnyRef, T <: AnyRef] extends Iterator[T] {

  def start    (             ) : V
  def terminal ( element : V ) : T
  def expand   ( element : V ) : Array[V]
  def score    ( element : V ) : Array[Double]

  private class Entry(
    eelement : V,
    escore   : Array[Double],
    euuid    : Int
  ) extends Comparable[Entry] {

    val element = eelement
    val score   = escore
    val uuid    = euuid

    override def compareTo(that : Entry) : Int = {
      val len = score.length
      var i = 0
      while (i < len) {
        val l = this.score(i)
        val r = that.score(i)
        if (l != r) {
          return Math.signum( l - r ).asInstanceOf[Int]
        }
        i += 1
      }
      val result = 
      if (element.isInstanceOf[Comparable[V]]) {
        element.asInstanceOf[Comparable[V]].compareTo(
          that.element
        )
      } else {
        0
      }
      if (result != 0) {
        result
      } else {
        Math.signum(that.uuid - uuid)
      }
    }

  }

  def remove() : Unit = {
    throw new UnsupportedOperationException
  }

  private val queue = new TreeSet[Entry]

  { queue.add(new Entry(start, score(start), -1)) }

  private var _next : T = null.asInstanceOf[T]
  private var _cur  : T = null.asInstanceOf[T]
  private var _hasNext  = (queue.size > 0)
  private var _uuid     = 0

  def hasNext() : Boolean = {
    if (!_hasNext) {
      false
    } else {
      if (_next eq null) { _next = inext()  }
      if (_next eq null) { _hasNext = false }
      _hasNext
    }
  }

  def next() : T = {
    if (!_hasNext) {
      throw new IllegalStateException
    }
    if (_next eq null) {
      _cur = inext()
      _hasNext = (_cur ne null)
    } else {
      _cur  = _next
      _next = null.asInstanceOf[T]
    }
    _cur
  }

  private def inext() : T = {
    while (queue.size > 0) {
      val e = queue.pollFirst.element
      val t = terminal(e)
      if (t eq null) {
        val array = expand(e)
        var i = 0
        while (i < array.length) {
          val child = array(i)
          if (child ne null) {
            queue.add(new Entry(child, score(child), _uuid))
          }
          i += 1
        }
      } else {
        return t
      }
    }
    null.asInstanceOf[T]
  }

}
