package de.measite.v.tree.partitioner

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle
import de.measite.v.searchtree.State
import java.lang.Comparable

class PartitioningKVectorState(val position : Array[KVector])
  extends State
  with Comparable[PartitioningKVectorState] {

  private var left  : RRectangle = new RRectangle
  private var right : RRectangle = new RRectangle
  private var la    : Double     = 0d
  private var ra    : Double     = 0d
  private var score = 1d
  private var pos = 0
  private var intersection = 0d
  private var leftSet = 0
  private var rightSet = 0
  val state = new Array[int](position.length)

  def this(that: PartitioningKVectorState, set: int) = {
    this(that.position)

    System.arraycopy(that.state, 0, state, 0, state.length)

    leftSet = that.leftSet
    rightSet = that.rightSet

    pos = that.pos
    while (state(pos) != 0) { pos += 1 }

    set match {
      case -1 => { // left
        state(pos) = set
        left  = that.left + position(pos)
        la    = if (left eq that.left) { that.la } else { left.area1p + 1d }
        right = that.right
        ra    = that.ra
        leftSet += 1
      }
      case  1 => { // right
        state(pos) = set
        left  = that.left
        la    = that.la
        right = that.right + position(pos)
        ra    = if (right eq that.right) { that.ra } else { right.area1p + 1d }
        rightSet += 1
      }
      case  _ => { // illegal
        throw new IllegalArgumentException();
      }
    }

    score = left.diagonal.length2 + right.diagonal.length2
  }

  def next() : Array[State] = {
    if (isTerminal) {
      new Array[State](0)
    } else {
      val l = new PartitioningKVectorState(this, -1)
      val r = new PartitioningKVectorState(this,  1)
      if (l.isValid) {
        if (r.isValid) { Array(l, r) } else { Array(l) }
      } else {
        if (r.isValid) { Array(r)    } else { Array()  }
      }
    }
  }

  def isValid() : Boolean = {
    val limit = (position.length + 1) / 2
    (leftSet <= limit) && (rightSet <= limit)
  }

  def isTerminal : Boolean = {
    pos == position.length - 1
  }

  override def equals(obj: Any) : boolean = {
    if (!obj.isInstanceOf[PartitioningKVectorState]) {
      false
    } else {
      val that = obj.asInstanceOf[PartitioningKVectorState]
      java.util.Arrays.equals(state, that.state)
    }
  }

  override def hashCode : int = {
    java.util.Arrays.hashCode(state)
  }

  override def compareTo(that: PartitioningKVectorState) : int = {
    if (score < that.score) {
      -1
    } else
    if (score > that.score) {
       1
    } else
    if (pos != that.pos) {
      that.pos - pos
    } else {
      var result = 0
      var i = 0
      while (i < state.length && result == 0) {
        result = (state(i) - that.state(i))
        if (i % 2 == 1) {
          result = -result
        }
        i += 1
      }
      result
    }
  }

}
