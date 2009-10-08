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
    _uniqueArea(list)
  }

  private def _uniqueArea(rect : ArrayList[RRectangle]) : Double = {
    if (rect.size == 0) { return 0d }
    if (rect.size == 1) { return rect.get(0).area }
    if (rect.size == 2) {
      return rect.get(0).area + rect.get(1).area -
             rect.get(0).intersection(rect.get(1)).area
    }
    val list = new ArrayList[RRectangle](rect.size)
    var i = 0
    var area = 0d
    while (i < rect.size) {
      var j = i + 1
      val r = rect.get(i)
      var contained = false
      while (j < rect.size && !contained) {
        val inter = r.intersection(rect.get(j))
        if (inter eq r) {
          contained = true
        } else
        if (inter eq rect.get(j)) {
          contained = true
          rect.set(j,r)
        } else
        if (inter ne RRectangle.NULL) {
          list.add(inter)
        }
        j += 1
      }
      if (!contained) {
        area += r.area
      }
      i += 1
    }
    if (list.size == 0) {
      area
    } else {
      area - _uniqueArea(list)
    }
  }

}

