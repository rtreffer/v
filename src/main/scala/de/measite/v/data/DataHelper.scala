package de.measite.v.data

import java.lang.Double._
import java.util.ArrayList

object DataHelper {

  def uniqueArea(rect : Array[RRectangle]) : Double = {
    var list = new ArrayList[RRectangle](rect.length + 1)
    var i = 0
    while (i < rect.length) {
      if (rect(i) ne null) {
        list.add(rect(i))
      }
      i += 1
    }
    _uniqueArea(list, new java.util.Random())
  }

  private def _uniqueArea(
    rect : ArrayList[RRectangle],
    rnd  : java.util.Random
  ) : Double = {
    if (rect.size == 0) { return 0d }
    if (rect.size == 1) { return rect.get(0).area }
    if (rect.size == 1) {
      return rect.get(0).area + rect.get(1).area -
             rect.get(0).intersection(rect.get(1)).area
    }

    val left        = new ArrayList[RRectangle]
    val right       = new ArrayList[RRectangle]
    val intersected = new java.util.BitSet(rect.size)
    var intercount  = 0

    var dimension   = -1
    var value       = Double.NaN

    // compute containment and intersection areas

    var i = 0
    while (i < rect.size) {
      var j = i + 1
      var r = rect.get(i)
      var contained = false
      while (j < rect.size && !contained) {
        val s     = rect.get(j)
        val inter = r.intersection(s)
        if (inter ne RRectangle.NULL) {
          if (inter eq r) {
            contained = true
            rect.set(i, null)
          } else
          if (inter eq s) {
            contained = true
            rect.set(i, null)
            rect.set(j, r)
          }
          intersected.set(i, true)
          intersected.set(j, true)
          intercount += 1
          if (Math.abs(rnd.nextInt) % intercount == 0) {
            var d   = 0
            val len = inter.low.dimension.length
            while (d < len) {
              val l = inter.low.dimension (d)
              if (!isNaN(l)) {
                val h = inter.high.dimension (d)
                if (d >= r.low.dimension.length) {
                  dimension = d
                  value     = l
                  d         = len
                } else
                if (d >= s.low.dimension.length) {
                  dimension = d
                  value     = h
                  d         = len
                } else {
                  val  h = inter.high.dimension(d)
                  val ll = r.low.dimension(d)
                  val rl = s.low.dimension(d)
                  val lh = r.high.dimension(d)
                  val rh = s.high.dimension(d)
                  if (isNaN(ll) || ll != l || isNaN(rl) || rl != l) {
                    dimension = d
                    value     = l
                    d         = len
                  } else
                  if (isNaN(lh) || lh != h || isNaN(rh) || rh != h) {
                    dimension = d
                    value     = h
                    d         = len
                  }
                }
              }
              d += 1
            }
          }
        }
        j += 1
      }
      i += 1
    }

    // add non-intersecting areas, split the rest

    var area = 0d
    i = 0
    while (i < rect.size) {
      val r = rect.get(i)
      if (r ne null) {
        if (intersected.get(i)) {
          val s = r.split(dimension, value)
          if (s._1 ne null) {  left.add(s._1) }
          if (s._2 ne null) { right.add(s._2) }
        } else {
          area += r.area
        }
      }
      i += 1
    }
    area +
    _uniqueArea(left, rnd) +
    _uniqueArea(right, rnd)
  }

}

