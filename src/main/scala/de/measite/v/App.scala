package de.measite.v

import de.measite.v.tree.RTree
import de.measite.v.tree.partitioner._
import de.measite.v.measure.SearchStatistic
import de.measite.v.data.KVector

/**
 * Hello world!
 *
 */
object App extends Application {

  override def main(args: Array[String]) = {
  val rnd = new java.util.Random(31337);
  // Step 1: generate a 1'000'000 10d vectors
  System.out.println("(1) vectors");
  val vector = new Array[KVector](100000)
  for (i <- 0 until vector.length) {
    val v = new Array[Double](100)
    for (j <- 0 until v.length) {
      v(j) = rnd.nextDouble
    }
    vector(i) = new KVector(v)
    if ((i + 1) % 1000 == 0) {
      System.out.println("(1) " + ((i + 1) / 1000) + "%");
    }
  }
  // Step 2: build a tree
  System.out.println("(2) tree");
  var tree = new RTree[String](100, HotspotPartitioner)
  for (i <- 0 until vector.length) {
    tree + vector(i)
    if ((i + 1) % 1000 == 0) {
      System.out.println("(2) " + System.currentTimeMillis + " " + ((i + 1) / 1000) + "%");
    }
  }
  // Step 3: search
  for (i <- 0 until 1) {
    val v = vector(Math.abs(rnd.nextInt) % vector.length)
    var bestMatch : KVector = null
    var bestMatchScore = java.lang.Double.MAX_VALUE
    System.out.println("(3) Search (validation search)");
    var rtime = -System.currentTimeMillis
    var vi = 0
    while (vi < vector.length) {
      val vec = vector(vi)
      if (vec ne v) {
        val d = vec.distance(v)
        if (d < bestMatchScore) {
          bestMatchScore = d
          bestMatch = vec
        }
      }
      vi += 1
    }
    rtime += System.currentTimeMillis
    System.out.println("(3) Search tree");
    var iter = tree.search(v)
    var time = -System.currentTimeMillis
    var next = iter.next.position
    time += System.currentTimeMillis
    if (next ne v) {
      System.out.println("Searched: " + v);
      System.out.println("Found: " + next);
      System.out.println("Distance: " + (v distance next) + " : " + (next distance v));
      throw new IllegalStateException
    }
    System.out.println("(X) Need " + time + "ms instead of " + rtime + "ms");
    System.out.println("(3) Search tree: 1 passed");
    next = iter.next.position
    if (next ne bestMatch) {
      System.out.println("Searched: " + v);
      System.out.println("Expected: " + bestMatch);
      System.out.println("Found: " + next);
      System.out.println("Distance: " + (v distance next) + " : " + (next distance v));
      System.out.println("ExpectedDistance: " + bestMatchScore);
      throw new IllegalStateException
    }
    System.out.println("(3) Search tree: 2 passed");
    time = -System.currentTimeMillis
    var stats = new SearchStatistic
    iter = tree.search(v, stats)
    for (i <- 0 until 100) {
      iter.next
//      System.out.println("#" + i + ", v:" + stats.visits + ", e:" + stats.expands + " in " + stats.runtime + "ms");
    }
    time += System.currentTimeMillis
    System.out.println("Time for 100 elements: " + time + "ms instead of " + rtime + "ms");
    time  = -System.currentTimeMillis
    tree.optimize
    time +=  System.currentTimeMillis
    System.out.println("Time for optimize: " + time + "ms")
    stats = new SearchStatistic
    time = -System.currentTimeMillis
    iter = tree.search(v, stats)
    for (i <- 0 until 100) {
      iter.next
      System.out.println("#" + i + ", v:" + stats.visits + ", e:" + stats.expands + " in " + stats.runtime + "ms");
    }
    time += System.currentTimeMillis
    System.out.println("Time for 100 elements: " + time + "ms instead of " + rtime + "ms");
  }
  System.exit(0);

  for (val r <- 1 to 5) {
    var time = -System.currentTimeMillis
    var tree = new RTree[String](20, QuadraticPartitioner)
    var rnd = new java.util.Random()
    for (val i <- 0 to 100000) {
      if (i%10000 == 0) { 
        System.out.println(i)
      }
      val v = rnd.nextDouble
      val leaf = (tree + new KVector(Array(v)))
      leaf.data = java.lang.Double.toString(v)
    }
    time += System.currentTimeMillis
    throw new IllegalStateException
    System.out.println("100000 ops in " + time + "ms")
    System.out.println((100000000d / time) + " ops/s")
  }
  }

}
