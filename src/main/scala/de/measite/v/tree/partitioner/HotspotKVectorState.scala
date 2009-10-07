package de.measite.v.tree.partitioner

import de.measite.v.data.KVector
import de.measite.v.data.RRectangle
import de.measite.v.searchtree.State
import java.lang.Double._

/**
 * The hotspot kvector state sorts based on the following heuristic:
 * 2
 * - min(A.area, intersection(A,B).area) / A.area
 * - min(B.area, intersection(A,B).area) / B.area
 */
class HotspotKVectorState(
  val position  : Array[KVector],
  val totalArea : RRectangle
) {

  var left         : RRectangle = new RRectangle
  var right        : RRectangle = left
  var intersection : RRectangle = left
  var score        : Double     = 1d
  var _exactScore  : Double     = Double.NaN
  var pos          : Int        = 0
  var leftSet      : Int        = 0
  var rightSet     : Int        = 0
  var intersected  : Int        = 0

  val state = new Array[int](position.length)

  def this(that: HotspotKVectorState, set: int) = {
    this(that.position, that.totalArea)

    System.arraycopy(that.state, 0, state, 0, state.length)

    leftSet = that.leftSet
    rightSet = that.rightSet

    pos = that.pos
    while (state(pos) != 0) { pos += 1 }

    set match {
      case -1 => { // left
        state(pos) = set
        right = that.right
        left  = that.left + position(pos)
        if (left eq that.left) {
          intersection = that.intersection
        } else {
          intersection = left.intersection(right)
        }
        leftSet += 1
      }
      case  1 => { // right
        state(pos) = set
        left  = that.left
        right = that.right + position(pos)
        if (right eq that.right) {
          intersection = that.intersection
        } else {
          intersection = left.intersection(right)
        }
        rightSet += 1
      }
      case  _ => { // illegal
        throw new IllegalArgumentException();
      }
    }

    if (intersection eq that.intersection) {
      intersected = that.intersected
      if (intersection.contains(position(pos))) {
        intersected += 1
      }
    } else {
      var i = 0
      intersected = 0
      while (i <= pos) {
        if (intersection.contains(position(i))) {
          intersected += 1
        }
        i += 1
      }
    }

    val ia = intersection.area
    val pi = intersected / position.length.asInstanceOf[Double]
    score = Math.sqrt(
      Math.max(0d, 1d - ia / totalArea.area)
    * Math.max(0d, 1d - pi)
    )
    if (java.lang.Double.isNaN(score)) {
      throw new IllegalStateException()
    }
  }

  def exactScore : Double = {
    if (_exactScore == Double.NaN) {
      val ra = right.area
      val la = left.area
      val ia = intersection.area
      val ql = if (isNaN(la) || la == 0d) { 0d } else { ia / la }
      val qr = if (isNaN(ra) || ra == 0d) { 0d } else { ia / ra }
      val pi = intersected / position.length.asInstanceOf[Double]
      _exactScore = Math.sqrt(
          Math.max(0d, 1d - (ql + qr)*0.5)
        * Math.max(0d, 1d - pi)
      )
    }
    _exactScore
  }

  def next() : Array[HotspotKVectorState] = {
    if (isTerminal) {
      new Array[HotspotKVectorState](0)
    } else {
      val l = new HotspotKVectorState(this, -1)
      val r = new HotspotKVectorState(this,  1)
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
    if (!obj.isInstanceOf[HotspotKVectorState]) {
      false
    } else {
      val that = obj.asInstanceOf[HotspotKVectorState]
      java.util.Arrays.equals(state, that.state)
    }
  }

}

