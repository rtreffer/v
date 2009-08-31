package de.measite.v.searchtree

import java.util.TreeMap
import java.util.Comparator

object DoubleArrayComparator extends Comparator[Array[Double]] {

  def compare(l : Array[Double], r: Array[Double]) : int = {
    val len = Math.min(l.length, r.length)
    for (i <- 0 until len) {
      val delta = Math.signum(l(i) - r(i))
      delta match {
        case  1d => { return  1 }
        case -1d => { return -1 }
        case   _ => {           }
      }
    }
    l.length - r.length
  }

}

/**
 * Search for the first leaf based on the leafs priorities
 */
object PriorityStragety {

  def search(base : State) : State = {
    val tree = new TreeMap[Array[Double], State](DoubleArrayComparator)
    tree.put(base.score, base)
    while (tree.size() > 0) {
      val key = tree.firstKey();
      val value = tree.remove(key);
      val childs = value.next();
      if (childs.length == 0) {
        if (value.isTerminal) {
          return value;
        }
      }
      childs.foreach(child => {
        tree.put(child.score, child)
      })
    }
    null
  }

}
