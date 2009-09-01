package de.measite.v

import de.measite.v.tree.RTree
import de.measite.v.data.KVector

/**
 * Hello world!
 *
 */
object App extends Application {
  for (val r <- 1 to 5) {
    var time = -System.currentTimeMillis
    var tree = new RTree[String](5)
    var rnd = new java.util.Random()
    for (val i <- 0 to 100000) {
      if (i%10000 == 0) { 
        System.out.println(i)
      }
      val v = rnd.nextDouble
      (tree + new KVector(Array(v))) setData java.lang.Double.toString(v)
    }
    time += System.currentTimeMillis
    System.out.println("100000 ops in " + time + "ms")
    System.out.println((100000000d / time) + " ops/s")
  }
}
