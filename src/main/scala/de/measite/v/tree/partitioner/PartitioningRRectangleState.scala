package de.measite.v.tree.partitioner

import de.measite.v.data.RRectangle
import de.measite.v.searchtree.State

class PartitioningRRectangleState(val rect : Array[RRectangle]) extends State {

  var innerOverlap = 0d
  var totalOverlap = 0d
  var left  : RRectangle = new RRectangle
  var right : RRectangle = new RRectangle
  var total : RRectangle = new RRectangle
  val state = new Array[int](rect.length)
  var pos = 0
  var leftSet = 0
  var rightSet = 0

  def this(that: PartitioningRRectangleState, set: int) = {
    this(that.rect)

    System.arraycopy(that.state, 0, state, 0, state.length)

    leftSet = that.leftSet
    rightSet = that.rightSet

    pos = that.pos
    while (state(pos) != 0) { pos += 1 }

    set match {
      case -1 => { // left
        state(pos) = set
        left = that.left + rect(pos)
        leftSet += 1
      }
      case  1 => { // right
        state(pos) = set
        right = that.right + rect(pos)
        rightSet += 1
      }
      case  _ => { // illegal
        throw new IllegalArgumentException();
      }
    }

    total = that.total + rect(pos)

    totalOverlap = left.intersection(right).area1p
    for (i <- 0 until pos) {
      if (state(i) != set) {
        innerOverlap += rect(i).intersection(rect(pos)).area1p
      }
    }

  }

  def score() : Array[Double] = {
    return Array(innerOverlap, totalOverlap, -pos)
  }

  def next() : Array[State] = {
    if (isTerminal) {
      return new Array[State](0)
    }
    val l = new PartitioningRRectangleState(this, -1)
    val r = new PartitioningRRectangleState(this,  1)
    if (l.isValid) {
      if (r.isValid) { Array(l, r) } else { Array(l) }
    } else {
      if (r.isValid) { Array(r)    } else { Array()  }
    }
  }

  def isValid() : Boolean = {
    val limit = (rect.length + 1) / 2
    (leftSet <= limit) && (rightSet <= limit)
  }

  def isTerminal : Boolean = {
    isValid && (pos == rect.length - 1)
  }

}
