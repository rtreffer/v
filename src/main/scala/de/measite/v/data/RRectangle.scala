package de.measite.v.data

import java.lang.Double.isNaN

/**
 * A RRectangle is the Rectangle spawned by a minimum and maximum vector.
 *
 * This class is collections safe. It implements equals, compareTo and
 * hashCode
 */
case class RRectangle(
  val low: KVector,
  val high: KVector
) extends Comparable[RRectangle] {

  /**
   * Create a new zero-dimensional rectangle.
   */
  def this() {
    this(
      new KVector(new Array[Double](0)),
      new KVector(new Array[Double](0))
    )
  }

  /**
   * Check if this area contains a vector. Return false if the vector contains
   * dimensions unknown by this rectangle.
   */
  def contains(that: KVector) : Boolean = {
    ((low <= that) && (that <= high))
  }

  /**
   * Check if this rectangle contains another rectangle. False if the other
   * rectangle contains dimensions unknown to this dimension.
   */
  def contains(that: RRectangle) : Boolean = {
    contains(that.low) &&  contains(that.high)
  }

  /**
   * Compute the distance of this rectangle and a given vector.
   */
  def distance(that : KVector) : Double = {
    var result = 0d
    var i = 0
    val len = that.dimension.length
    val lenlow = low.dimension.length
    val lenhigh = high.dimension.length
    val limit = Math.min(len, Math.max(lenlow, lenhigh))
    while (i < limit) {
      val v = that.dimension(i)
      if (!isNaN(v)) {
        if (i < lenlow) {
          val l = low.dimension(i)
          if ((!isNaN(l)) && (v < l)) { result += (v - l) * (v - l) }
        }
        if (i < lenhigh) {
          val h = high.dimension(i)
          if ((!isNaN(h)) && (v > h)) { result += (v - h) * (v - h) }
        }
      }
      i += 1
    }
    result
  }

  /**
   * Add another area, return a rectangle containing both areas
   */
  def +(that: RRectangle) : RRectangle = {
    val l = this.low .min(that.low )
    val h = this.high.max(that.high)
    if ((l eq that.low) && (h eq that.high)) {
      return that
    }
    if ((l eq low) && (h eq high)) {
      return this
    }
    new RRectangle(l, h)
  }

  /**
   * Add another vector, return a rectangle containing the new vector
   */
  def +(that: KVector) : RRectangle = {
    val l = this.low .min(that)
    val h = this.high.max(that)
    if ((l eq low) && (h eq high)) {
      return this
    }
    new RRectangle(l, h)
  }

  /**
   * Compute the intersection with another area
   */
  def intersection(that: RRectangle) : RRectangle = {
    val maxlow  = this.low .max(that.low,  false)
    val minhigh = this.high.min(that.high, false)
    if ((this.low eq maxlow) && (this.high eq minhigh)) { return this }
    if ((that.low eq maxlow) && (that.high eq minhigh)) { return that }
    val len =
      Math.min(
        maxlow.dimension.length,
        minhigh.dimension.length
      )
    val low  = new Array[Double](len)
    val high = new Array[Double](len)
    var i = 0;
    while (i < len) {
      val l = maxlow.dimension(i)
      val h = minhigh.dimension(i)
      if (!isNaN(l) && !isNaN(h) && l <= h) {
        low(i)  = l
        high(i) = h
      } else {
        low(i)  = Double.NaN
        high(i) = Double.NaN
      }
      i += 1
    }
    new RRectangle(new KVector(low), new KVector(high))
  }

  var _area1p : Double = Double.NaN

  /**
   * Compute the area of this rectangle such that
   * <ul>
   *   <li>result  = 1</li>
   *   <li>result *= (high(i) - low(i) + 1) // for every i</li>
   *   <li>result  = result - 1</li>
   * </ul>
   */
  def area1p : Double = {
    if (isNaN(_area1p)) {
      _area1p = 1d
      var i = 0
      val lowlen = low.dimension.length
      val highlen = high.dimension.length
      val limit = Math.min(lowlen, highlen)
      while (i < limit) {
        val l = low.dimension(i)
        val h = high.dimension(i)
        if ((!isNaN(l)) && (!isNaN(h))) {
          _area1p *= (h - l + 1d)
        }
        i += 1
      }
      _area1p -= 1d
    }
    _area1p
  }

  override def toString() : String = {
    val sb = new StringBuilder()
    sb.append("RRectangle<")
    low.apply(
      high,
      (p,l,h) => { sb.append("(" + p + "," + l + "," + h + ")") },
      (p,l)   => {                                              },
      (p,h)   => {                                              },
      (p)     => {                                              }
    )
    sb.append(">")
    sb.toString
  }

  override def equals(obj : Any) : boolean = {
    if ((obj == null)||(!obj.isInstanceOf[RRectangle])) {
      return false
    } else {
      val that = obj.asInstanceOf[RRectangle]
      if (this eq that) {
        true
      } else {
        that.low.equals(this.low) && that.high.equals(this.high)
      }
    }
  }

  override def hashCode() : int = {
    low.hashCode * 31 + high.hashCode
  }

  override def compareTo(that: RRectangle) : int = {
    val v = this.low.compareTo(that.low)
    if (v != 0) {
      v
    } else {
      this.high.compareTo(that.high)
    }
  }

}
