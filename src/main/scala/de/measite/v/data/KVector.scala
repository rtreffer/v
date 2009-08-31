package de.measite.v.data

import java.lang.Double.isNaN

/**
 * A KVector is a dense k-dimensional vector.
 * 
 * The internal representation is an array with some syntax
 * sugar on top.
 */
case class KVector(dimension: Array[Double]) {

  def apply(
    set   : (Int, Double) => Unit,
    unset : (Int) => Unit
  ) : Unit = {
    val len = dimension.length
    var i = 0
    while (i < len) {
      val value = dimension(i);
      if (isNaN(value)) {
        unset(i)
      } else {
        set(i, value)
      }
      i += 1
    }
  }

  /**
   * Helper function to apply a function to every element of both vectors,
   * calling different methods in the NaN case
   */
  // TODO: ugly code
  def apply(
    that  : KVector,
    both  : (Int, Double, Double) => Unit,
    left  : (Int, Double) => Unit,
    right : (Int, Double) => Unit,
    none  : (Int) => Unit
  ) : Unit = {
    val lenthis = this.dimension.length
    val lenthat = that.dimension.length
    var i = 0
    while (i < lenthis || i < lenthat) {
      val l = if (i < lenthis) { this.dimension(i) } else { Double.NaN }
      val r = if (i < lenthat) { that.dimension(i) } else { Double.NaN }
      if (isNaN(l)) {
        if (isNaN(r)) { none(i)    } else { right(i, r)   }
      } else {
        if (isNaN(r)) { left(i, l) } else { both(i, l, r) }
      }
      i += 1
    }
  }

  /**
   * Helper function to apply a function to every element of three vectors.<br/>
   * The vectors are this=left, mid, right.<br/>
   * Methods supplied are:
   * <ul>
   * <li>lmr exist</li>
   * <li>lm exist</li>
   * <li>lr exist</li>
   * <li>mr exist</li>
   * <li>l exists</li>
   * <li>m exists</li>
   * <li>r exists</li>
   * <li>no dimension exists</li>
   * </ul>
   */
   // TODO: ugly code
  def apply(
    m     : KVector,
    r     : KVector,
    flmr  : (Int, Double, Double, Double) => Unit,
    flm   : (Int, Double, Double) => Unit,
    flr   : (Int, Double, Double) => Unit,
    fmr   : (Int, Double, Double) => Unit,
    fl    : (Int, Double) => Unit,
    fm    : (Int, Double) => Unit,
    fr    : (Int, Double) => Unit,
    fnone : (Int) => Unit
  ) : Unit = {
    val l = this
    val lenl = l.dimension.length
    val lenm = m.dimension.length
    val lenr = r.dimension.length
    var i = 0
    while (i < lenl || i < lenm || i < lenr) {
      val vl = if (i < lenl) { l.dimension(i) } else { Double.NaN }
      val vm = if (i < lenm) { m.dimension(i) } else { Double.NaN }
      val vr = if (i < lenr) { r.dimension(i) } else { Double.NaN }
      if (isNaN(vl)) {
        if (isNaN(vm)) {
          if (isNaN(vr)) { fnone(i)       } else { fr(i, vr)           }
        } else {
          if (isNaN(vr)) { fm(i, vm)      } else { fmr(i, vm, vr)      }
        }
      } else {
        if (isNaN(vm)) {
          if (isNaN(vr)) { fl(i, vl)      } else { flr(i, vl, vr)      }
        } else {
          if (isNaN(vr)) { flm(i, vl, vm) } else { flmr(i, vl, vm, vr) }
        }
      }
      i += 1
    }
  }

  /**
   * Apply a constant factor to every component of this vector.
   */
  def *!(d : Double) : Unit = {
    for (i <- 0 until dimension.length) {
      if (!isNaN(dimension(i))) {
        dimension(i) *= d
      }
    }
  }

  /**
   * Apply a constant factor to every component of this vector.
   */
  def *(d : Double) : Unit = {
    new KVector(dimension.map(x => {d*x}))
  }

  /*
   * The following methods are implemented based on apply.
   */

  /**
   * Add two vectors. Return a new vector of the
   * combined elements.
   */
  def +(that: KVector) : KVector = {
    val len = Math.max(dimension.length, that.dimension.length)
    val result = new Array[Double](len)
    apply(
      that,
      (p,u,v) => { result(p) = u + v      },
      (p,u)   => { result(p) = u          },
      (p,v)   => { result(p) = v          },
      (p)     => { result(p) = Double.NaN }
    )
    new KVector(result)
  }

  /**
   * Difference as metric distance.
   */
  def -(that: KVector) : Double = {
    var result = 0d
    apply(
      that,
      (p,u,v) => { val d = u - v; result += d * d },
      (p,u)   => {                                },
      (p,v)   => {                                },
      (p)     => {                                }
    )
    result
  }

  def avg(that: KVector) : KVector = {
    val v = this + that
    v *! 0.5d
    v
  }

  def min(that: KVector) : KVector = {
    val len = Math.max(dimension.length, that.dimension.length)
    val result = new Array[Double](len)
    apply(
      that,
      (p,u,v) => { result(p) = Math.min(u, v) },
      (p,u)   => { result(p) = u              },
      (p,v)   => { result(p) = v              },
      (p)     => { result(p) = Double.NaN     }
    )
    new KVector(result)
  }

  def max(that: KVector) : KVector = {
    val len = Math.max(this.dimension.length, that.dimension.length)
    val result = new Array[Double](len)
    apply(
      that,
      (p,u,v) => { result(p) = Math.max(u, v) },
      (p,u)   => { result(p) = u              },
      (p,v)   => { result(p) = v              },
      (p)     => { result(p) = Double.NaN     }
    )
    new KVector(result)
  }

  /**
   * Compare this vector with a second vector.
   * True if this vector is truly smaller than
   * the other vector (aka no single component is
   * greater than or equal to the other vectors
   * component)
   */
  def <(that: KVector) : Boolean = this < (that, true)

  def <(that: KVector, strict: Boolean) : Boolean = {
    var result = true
    apply(
      that,
      (p,u,v) => { result &= (u <  v) },
      (p,u)   => { result &= !strict  },
      (p,v)   => { result &= !strict  },
      (p)     => {                    }
    )
    result
  }

  def <=(that: KVector) : Boolean = this <= (that, true)

  def <=(that: KVector, strict: Boolean) : Boolean = {
    var result = true
    apply(
      that,
      (p,u,v) => { result &= (u <= v) },
      (p,u)   => { result &= !strict  },
      (p,v)   => { result &= !strict  },
      (p)     => {                    }
    )
    result
  }

  /**
   * Compare this vector to another vector.
   * True if this vector is truly greater
   * than the other vector.
   */
  def >(that: KVector) : Boolean = {
    this > (that, true)
  }

  def >(that: KVector, strict: Boolean) : Boolean = {
    var result = true
    apply(
      that,
      (p,u,v) => { result &= (u >  v) },
      (p,u)   => { result &= !strict  },
      (p,v)   => { result &= !strict  },
      (p)     => {                    }
    )
    result
  }

  def >=(that: KVector) : Boolean = {
    this >= (that, true)
  }

  def >=(that: KVector, strict: Boolean) : Boolean = {
    var result = true
    apply(
      that,
      (p,u,v) => { result &= (u >= v) },
      (p,u)   => { result &= !strict  },
      (p,v)   => { result &= !strict  },
      (p)     => {                    }
    )
    result
  }

  override def toString() : String = {
    val sb = new StringBuilder()
    sb append "v<"
    var init = true
    apply(
      (i,v) => {
        if (init) {
          init = false
        } else {
          sb append ','
        }
        sb append i
        sb append "("
        sb append v
        sb append ")"
      },
      (i)   => { }
    )
    sb append ">"
    sb.toString()
  }

}
