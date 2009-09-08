package de.measite.v.data

import java.lang.Double.isNaN

/**
 * A KVector is a dense k-dimensional vector.
 * 
 * The internal representation is an array with some syntax
 * sugar on top.
 * 
 * This class is collections safe. It implements a partial ordering
 * as well as comparable, equals and hashcode.
 */
case class KVector(dimension: Array[Double])
  extends Comparable[KVector] {

    /**
     * Helper function to apply a function to every element, handling NaN
     * as "not set".
     */
    // TODO: ugly code
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
      (p,u,v) => { result += (u - v) * (u - v) },
      (p,u)   => {                             },
      (p,v)   => {                             },
      (p)     => {                             }
    )
    result
  }

  def avg(that: KVector) : KVector = {
    val v = this + that
    v *! 0.5d
    v
  }

  def min(that: KVector, merge: boolean) : KVector = {
    val len = Math.max(dimension.length, that.dimension.length)
    val result = new Array[Double](len)
    apply(
      that,
      (p,u,v) => { result(p) = Math.min(u, v)                       },
      (p,u)   => { result(p) = if (merge) { u } else { Double.NaN } },
      (p,v)   => { result(p) = if (merge) { v } else { Double.NaN } },
      (p)     => { result(p) = Double.NaN                           }
    )
    new KVector(result)
  }

  def min(that: KVector) : KVector = min(that, true)

  def max(that: KVector, merge: boolean) : KVector = {
    val len = Math.max(this.dimension.length, that.dimension.length)
    val result = new Array[Double](len)
    apply(
      that,
      (p,u,v) => { result(p) = Math.max(u, v)                       },
      (p,u)   => { result(p) = if (merge) { u } else { Double.NaN } },
      (p,v)   => { result(p) = if (merge) { v } else { Double.NaN } },
      (p)     => { result(p) = Double.NaN                           }
    )
    new KVector(result)
  }

  def max(that: KVector) : KVector = max(that, true)

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

  /**
   * Transform this vector into a string representation.
   * Implicitly called by "" + kvector.
   */
  override def toString() : String = {
    val sb = new StringBuilder()
    sb append "KVector<"
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

  /**
   * Two vectors are equal if the set dimension on both vectors match.
   */
  override def equals(obj : Any) : boolean = {
    if ((obj == null)||(!obj.isInstanceOf[KVector])) {
      return false
    } else {
      val that = obj.asInstanceOf[KVector]
      var result = true
      apply(
        that,
        (p,l,r) => { result &= l == r },
        (p,l)   => { result  = false  },
        (p,r)   => { result  = false  },
        (p)     => {                  }
      )
      result
    }
  }

  /**
   * The hashCode of a KVector is defined by the position and value of all
   * set dimensions.
   */
  override def hashCode() : int = {
    val prime = 31
    var result = 0
    apply(
      (p,v) => {
          val k = java.lang.Double.doubleToLongBits(v)
          result = p*result + prime
          result = p*result + (k % p).asInstanceOf[int]
      },
      (p)   => { }
    )
    result
  }

  /**
   * Compares two KVectors by comparing all set dimensions.
   * Let k,v be KVectors.
   * Let i be a dimension index such that
   * <ul>
   *   <li>k(i) is the value of the ith dimension of k</li>
   *   <li>v(i) is the value of the ith dimension of v</li>
   *   <li>i is minimal with k(i) != v(i)</li>
   * <ul>
   * Under these preconditions
   * <ul>
   *   <li>k.compareTo(v) => -1 if v(i) is NaN</li>
   *   <li>k.compareTo(v) =>  1 if k(i) is NaN</li>
   *   <li>k.compareTo(v) => -1 if k(i) &lt; v(i)</li>
   *   <li>k.compareTo(v) =>  1 if k(i) &gt; v(i)</li>
   * </ul>
   */
  override def compareTo(that: KVector) : int = {
    var result = 0
    apply(
      that,
      (p,l,r) => {
        if (result == 0) {
          result = Math.signum(l - r).asInstanceOf[int]
        }
      },
      (p,l)   => { if (result == 0) { result = -1 } },
      (p,r)   => { if (result == 0) { result =  1 } },
      (p)     => { }
    )
    result
  }

}
