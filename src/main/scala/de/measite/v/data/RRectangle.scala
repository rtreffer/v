package de.measite.v.data

/**
 * A RRectangle is the Rectangle spawned by a minimum and maximum vector.
 */
case class RRectangle(val low: KVector, val high: KVector) {

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
    low.apply(
      that,
      high,
      (p,l,m,h) => {
        if (m < l) {
          result += (l-m) * (l-m)
        } else if (m > h) {
          result += (m-h) * (m-h)
        }
      },
      (p,l,m) => { if (m < l) { result += (l-m) * (l-m) } },
      (p,l,h) => {                                        },
      (p,m,h) => { if (m > h) { result += (m-h) * (m-h) } },
      (p,l)   => {                                        },
      (p,m)   => {                                        },
      (p,h)   => {                                        },
      (p)     => {                                        }
    )
    result
  }

  /**
   * Add another area, return a rectangle containing both areas
   */
  def +(that: RRectangle) : RRectangle = {
    if (this contains that) {
      this
    } else {
      new RRectangle(
        this.low .min(that.low),
        this.high.max(that.high)
      )
    }
  }

  /**
   * Add another vector, return a rectangle containing the new vector
   */
  def +(that: KVector) : RRectangle = {
    if (this contains that) {
      this
    } else {
      new RRectangle(
        this.low .min(that),
        this.high.max(that)
      )
    }
  }

  /**
   * Compute the intersection with another area
   */
  def intersection(that: RRectangle) = {
    val maxlow  = this.low .max(that.low,  false)
    val minhigh = this.high.min(that.high, false)
    val len =
      Math.max(
        Math.max(this.low .dimension.length, that.low .dimension.length),
        Math.max(this.high.dimension.length, that.high.dimension.length)
      )
    val low  = new Array[Double](len)
    val high = new Array[Double](len)
    maxlow.apply (
      minhigh,
      (p,l,h) => { if (l <= h)
                   { low(p)  = l          ; high(p) = h          }
              else { low(p)  = Double.NaN ; high(p) = Double.NaN } },
      (p,l)   =>   { low(p)  = Double.NaN ; high(p) = Double.NaN },
      (p,h)   =>   { low(p)  = Double.NaN ; high(p) = Double.NaN },
      (p)     =>   { low(p)  = Double.NaN ; high(p) = Double.NaN }
    )
    new RRectangle(new KVector(low), new KVector(high))
  }

  /**
   * Compute the area of this rectangle such that
   * <ul>
   *   <li>result  = 1</li>
   *   <li>result *= (high(i) - low(i) + 1) // for every i</li>
   *   <li>result  = result - 1</li>
   * </ul>
   */
  def area1p : Double = {
    var result = 1d
    low.apply(
      high,
      (p,l,h) => { result = result * (h - l + 1d) },
      (p,l)   => {                                },
      (p,h)   => {                                },
      (p)     => {                                }
    )
    result - 1d
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

}
