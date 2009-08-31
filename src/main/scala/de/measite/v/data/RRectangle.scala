package de.measite.v.data

case class RRectangle(val low: KVector, val high: KVector) {

  def this() {
    this(
      new KVector(new Array[Double](0)),
      new KVector(new Array[Double](0))
    )
  }

  def contains(that: KVector) : Boolean = {
    (low < that && that < high)
  }

  def contains(that: RRectangle) : Boolean = {
    contains(that.low) &&  contains(that.high)
  }

  def newDimensions(that: KVector) : Int = {
    var result = 0
    low.apply(
      that,
      high,
      (p,l,m,h) => {             },
      (p,l,m)   => {             },
      (p,l,h)   => {             },
      (p,m,h)   => {             },
      (p,l)     => {             },
      (p,m)     => { result += 1 },
      (p,h)     => {             },
      (p)       => {             }
    )
    result
  }

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

  def xdistance(that : KVector) : Double = {
    var result = 0d
    low.apply(
      that,
      high,
      (p,l,m,h) => {
        result += (l-m) * (l-m)
               +  (m-h) * (m-h)
      },
      (p,l,m) => { result += (l-m) * (l-m) },
      (p,l,h) => {                         },
      (p,m,h) => { result += (m-h) * (m-h) },
      (p,l)   => {                         },
      (p,m)   => {                         },
      (p,h)   => {                         },
      (p)     => {                         }
    )
    result
  }

  def +(that: RRectangle) : RRectangle = {
    new RRectangle(
      this.low .min(that.low),
      this.high.max(that.high)
    )
  }

  def +(that: KVector) : RRectangle = {
    if (this contains that) {
      return this
    }
    new RRectangle(
      this.low .min(that),
      this.high.max(that)
    )
  }

  def intersection(that: RRectangle) = {
    val maxlow  = this.low  max that.low
    val minhigh = this.high min that.high
    val len =
      Math.max(
        Math.max(this.low. dimension.length, that.low. dimension.length),
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

}
