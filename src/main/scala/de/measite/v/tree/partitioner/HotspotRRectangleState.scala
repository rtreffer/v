package de.measite.v.tree.partitioner

import de.measite.v.data.RRectangle
import de.measite.v.searchtree.State

class HotspotRRectangleState(
  val rect : Array[RRectangle]
) {

  var left  : RRectangle = new RRectangle
  var la                 = 1d
  var right : RRectangle = new RRectangle
  var ra                 = 1d
  var areaScore = 1d
  val state = new Array[int](rect.length)
  var pos = 0
  var leftSet = 0
  var rightSet = 0

  def this(that: HotspotRRectangleState, set: int) = {
    this(that.rect)

    System.arraycopy(that.state, 0, state, 0, state.length)

    leftSet = that.leftSet
    rightSet = that.rightSet

    pos = that.pos
    while (state(pos) != 0) { pos += 1 }

    set match {
      case -1 => { // left
        state(pos) = set
        left  = that.left + rect(pos)
        la    = if (left eq that.left) { that.la } else { left.area1p + 1d }
        right = that.right
        ra    = that.ra
        leftSet += 1
      }
      case  1 => { // right
        state(pos) = set
        right = that.right + rect(pos)
        ra    = if (right eq that.right) { that.ra } else { right.area1p + 1d }
        left  = that.left
        la    = that.la
        rightSet += 1
      }
      case  _ => { // illegal
        throw new IllegalArgumentException();
      }
    }

    areaScore = left.diagonal.length2 + right.diagonal.length2

  }

  def next() : Array[HotspotRRectangleState] = {
    if (isTerminal) {
      return new Array[HotspotRRectangleState](0)
    }
    val l = new HotspotRRectangleState(this, -1)
    val r = new HotspotRRectangleState(this,  1)
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
