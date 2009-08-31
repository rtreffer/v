package de.measite.v.tree.partitioner

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle
import de.measite.v.searchtree.State

class PartitioningKVectorState(val position : Array[KVector]) extends State {

  var left  : RRectangle = new RRectangle
  var right : RRectangle = new RRectangle
  val state = new Array[int](position.length)
  var pos = 0
  var leftSet = 0
  var rightSet = 0

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
        right = that.right
        leftSet += 1
      }
      case  1 => { // right
        state(pos) = set
        left  = that.left
        right = that.right + position(pos)
        rightSet += 1
      }
      case  _ => { // illegal
        throw new IllegalArgumentException();
      }
    }

  }

  def score() : Array[Double] = {
    return Array(left.intersection(right).area1p, -pos)
  }

  def next() : Array[State] = {
    if (isTerminal) {
      return new Array[State](0)
    }
    val l = new PartitioningKVectorState(this, -1)
    val r = new PartitioningKVectorState(this,  1)
    if (l.isValid) {
      if (r.isValid) { Array(l, r) } else { Array(l) }
    } else {
      if (r.isValid) { Array(r)    } else { Array()  }
    }
  }

  def isValid() : Boolean = {
    val limit = (position.length + 1) / 2
    (leftSet <= limit) && (rightSet <= limit)
  }

  def isTerminal : Boolean = {
    pos == position.length - 1
  }

}
