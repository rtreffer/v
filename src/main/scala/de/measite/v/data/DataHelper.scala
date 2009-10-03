package de.measite.v.data

import java.lang.Double._
import java.util.ArrayList

trait DataHeltper {

  def uniqueArea(rect : Array[RRectangle]) : Double = {
    if (rect.length == 0) { return 0d }
    val list = new ArrayList[RRectangle](rect.length)
    var i = 0
    var area = 0d
    while (i < rect.length) {
      var j = i + 1
      val r = rect(i)
      while (j < rect.length) {
        val inter = r.intersection(rect(j))
        val area  = inter.area
        if (area > 0d) {
          list.add(inter)
        }
        j += 1
      }
      area += r.area
      i += 1
    }
    if (list.size == 0) {
      area
    } else {
      val array = list.toArray(new Array[RRectangle](list.size))
      area - uniqueArea(array)
    }
  }

}
