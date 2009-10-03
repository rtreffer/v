package de.measite.v.tree.partitioner

import java.lang.Double
import java.util.Arrays

import de.measite.v.data.KVector
import de.measite.v.tree.RTreeLeaf

case class NodeDistance(
  distance : double,
  index    : int
) extends Comparable[NodeDistance] {
  def compareTo(that : NodeDistance) : int = {
    if (distance < that.distance) { return -1 }
    if (distance > that.distance) { return  1 }
    Math.signum(index - that.index).asInstanceOf[int]
  }
}

object KMeansPartitioner {

  val meanRuns = 10

  /*
   * Phases of a K-Means
   * (1) init (k initial cluster
   * (2) split into buckets
   * (3) adjust centers
   * (4) goto (2) unless abort condition
   */

  private def isplit[V](
    nodes  : Array[V],
    kmeans : int,
    width  : int,
    p      : (V) => KVector,
    d      : (V, KVector) => double
  ) : Array[int] = {
    val min = width / 2

    val center  = new Array[KVector](kmeans)
    val cluster = new Array[int](nodes.length)

    for (run <- 0 until meanRuns) {
      if (run == 0) {
        for (i <- 0 until kmeans) {
          center(i) = p(nodes(i))
        }
      } else {
        // adjust centers
        for (i <- 0 until center.length) {
          center(i) = new KVector(new Array[double](0))
        }
        for (i <- 0 until nodes.length) {
          center(cluster(i)) += p(nodes(i))
        }
        for (i <- 0 until center.length) {
          center(i) *= (1d / kmeans)
        }
        for (i <- 0 until center.length) {
          System.out.println(center(i));
        }
      }

      // split into buckets
      val clen    = new Array[int](kmeans)
      for (i <- 0 until nodes.length) {
        var best = Double.MAX_VALUE
        var bint = -1
        val node = nodes(i)
        for (j <- 0 until kmeans) {
          if (clen(j) < width) {
            val delta = d(node, center(j))
            if (delta < best) {
              best = delta
              bint = j
            }
          }
        }
        cluster(i)  = bint
        clen(bint) += 1
      }

      // balance
      var balance = true
      while (balance) {
        balance  = false
        var mini = -1
        var minv = min
        for (i <- 0 until kmeans) {
          if (clen(i) < minv) {
            minv = clen(i)
            mini = i
            balance = true
          }
        }

        // steal elements from the nearest overfull boxes
        if (balance) {
          val c      = center(cluster(mini))
          val sorted = new Array[NodeDistance](nodes.length)
          for (i <- 0 until nodes.length) {
            sorted(i) = new NodeDistance(d(nodes(i), c), i)
          }
          Arrays.sort(sorted.asInstanceOf[Array[Object]])
          var i = 1
          while (minv < min) {
            val node = sorted(i)
            if (clen(cluster(i)) > min && cluster(i) != mini) {
              minv             += 1
              clen(cluster(i)) -= 1
              clen(mini)       += 1
              cluster(i)        = mini
            }
            i += 1
          }
        }
      }
    }

    cluster
  }

  /**
   * Split performs a k-means clustering on top of a list of a list
   * of leafs. The result is a two level tree with a usually better
   * space utilization.
   * 
   * This method is part of the general tree optimization.
   *
   * kmeans should be chosen to maximalize the freedom of the child nodes.
   */
  def split[T](
    nodes  : Array[RTreeLeaf[T]],
    kmeans : int,
    width  : int
  ) : RTreeNode[T] = {
    val partitioner = nodes(0).parent.asInstanceOf[RTreeNode[T]].partitioner
    val cluster = isplit[RTreeLeaf[T]](
      nodes,
      kmeans,
      width,
      (leaf) => { leaf.position },
      (leaf, point) => { point.distance(leaf.position) }
    )
    // generate an array of RTreeNodes
    val inodes = new Array[RTreeNode[T]](kmeans)
    for (i <- 0 until kmeans) {
      inodes(i) = new RTreeNode[T](width, partitioner)
    }
    for (i <- 0 until nodes.length) {
      inodes(cluster(i)) + nodes(i)
    }
    val result = new RTreeNode[T](width, partitioner)
    inodes.foreach(in => { result + in })
    result
  }

  def split[T](
    nodes  : Array[RTreeNode[T]],
    kmeans : int,
    width  : int
  ) : RTreeNode[T] = {
    val partitioner = nodes(0).partitioner
    val cluster = isplit[RTreeNode[T]](
      nodes,
      kmeans,
      width,
      (node) => { node.rectangle.center },
      (node, point) => {
        (
          node.rectangle. low.distance(point) +
          node.rectangle.high.distance(point)
        ) / node.rectangle.diagonal.length2
      }
    )
    // generate an array of RTreeNodes
    val inodes = new Array[RTreeNode[T]](kmeans)
    for (i <- 0 until kmeans) {
      inodes(i) = new RTreeNode[T](width, partitioner)
    }
    for (i <- 0 until nodes.length) {
      inodes(cluster(i)) + nodes(i)
    }
    val result = new RTreeNode[T](width, partitioner)
    inodes.foreach(in => { result + in })
    result
  }

}
