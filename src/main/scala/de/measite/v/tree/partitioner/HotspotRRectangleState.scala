package de.measite.v.tree.partitioner

import de.measite.v.data.RRectangle
import de.measite.v.data.DataHelper._
import de.measite.v.searchtree.State

import java.util.ArrayList

class HotspotRRectangleState(
  val rect       : Array[RRectangle],
  val uniqueArea : Double
) {

  var left         : RRectangle = RRectangle.NULL
  var right        : RRectangle = RRectangle.NULL
  var intersection : RRectangle = RRectangle.NULL
  var score    = 1d
  val state    = new Array[int](rect.length)
  var pos      = 0
  var leftSet  = 0
  var rightSet = 0

  def this(that: HotspotRRectangleState, set: int) = {
    this(that.rect, that.uniqueArea)

    System.arraycopy(that.state, 0, state, 0, state.length)

    leftSet = that.leftSet
    rightSet = that.rightSet

    pos = that.pos
    while (state(pos) != 0) { pos += 1 }

    if (set == -1) { // left
      state(pos) = set
      left  = that.left + rect(pos)
      if (left eq that.left) {
        intersection = that.intersection
      } else {
        intersection = left.intersection(right)
      }
      right = that.right
      leftSet += 1
    } else
    if (set == 1) { // right
      state(pos) = set
      right = that.right + rect(pos)
      if (right eq that.right) {
        intersection = that.intersection
      } else {
        intersection = left.intersection(right)
      }
      left  = that.left
      rightSet += 1
    } else { // illegal
      throw new IllegalArgumentException();
    }

    val la = if (isNaN(left.area) || left.area <= 0d) {
      0d
    } else {
      left.area
    }
    val ra = if (isNaN(right.area) || right.area <= 0d) {
      0d
    } else {
      right.area
    }
    val la = if (isNaN(intersection.area) || intersection.area <= 0d) {
      0d
    } else {
      intersection.area
    }

    score = Math.sqrt(uniqueArea / (la + ra - ia))

    if (isNaN(score) || score <= 0d) {
      score = 0d
    }
  }

  def exactScore() : Double = {
    if (isNaN(_exactScore)) {
      val larray = new Array[RRectangle[leftSet]]
      val lpos   = 0
      val rarray = new Array[RRectangle[rightSet]]
      val rpos   = 0
      val clist  = new ArrayList[RRectangle](rect.length)
      var i = 0
      while (i <= pos) {
        if (state(i) == -1) {
          larray(lpos) = rect(i)
          lpos += 1
        } else
        if (state(i) ==  1) {
          rarray(rpos) = rect(i)
          rpos += 1
        }
        val inter = intersection.intersection(rect(i))
        if (inter ne RRectangle.NULL) {
          clist.add(inter)
        }
        i += 1
      }
      val carray = new Array[RRectangle](clist.size())
      i = 0
      while (i < carray.length) {
        carray(i) = clist.get(i)
        i += 1
      } 
      val la = uniqueArea(larray)
      val ra = uniqueArea(rarray)
      val ca = uniqueArea(carray)
      _exactScore = Math.sqrt(
        score * score *
        ((la - ca) / left.area + (ra - ca) / right.area)*0.5
      )
    }
    _exactScore
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

