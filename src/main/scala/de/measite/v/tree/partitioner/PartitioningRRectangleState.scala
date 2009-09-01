package de.measite.v.tree.partitioner

import de.measite.v.data.RRectangle
import de.measite.v.searchtree.State

class PartitioningRRectangleState(val rect : Array[RRectangle])
  extends State
  with Comparable[PartitioningRRectangleState] {

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
        left  = that.left + rect(pos)
        right = that.right
        leftSet += 1
      }
      case  1 => { // right
        state(pos) = set
        right = that.right + rect(pos)
        left  = that.left
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

  override def equals(obj: Any) : boolean = {
    if (!obj.isInstanceOf[PartitioningKVectorState]) {
      return false
    }
    val that = obj.asInstanceOf[PartitioningKVectorState]
    java.util.Arrays.equals(this.state, that.state)
  }

  override def hashCode : int = {
    java.util.Arrays.hashCode(state)
  }

  override def compareTo(that: PartitioningRRectangleState) : int = {
    if (this.innerOverlap < that.innerOverlap) { return -1 }
    if (this.innerOverlap > that.innerOverlap) { return  1 }
    if (this.totalOverlap < that.totalOverlap) { return -1 }
    if (this.totalOverlap > that.totalOverlap) { return  1 }
    if (this.pos > that.pos) { return -1 }
    if (this.pos < that.pos) { return  1 }
    for (i <- 0 until state.length) {
      if (this.state(i) < that.state(i)) { return -1 }
      if (this.state(i) > that.state(i)) { return  1 }
    }
    0
  }

}
