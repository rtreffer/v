package de.measite.v.tree

import java.util.TreeMap

import de.measite.v.data.KVector

case class NearestNodeIterator[T](position: KVector, root: RTreeNode[T]) {

  val best = new TreeMap[Double, RTreeNode[T]]()
  val border = new TreeMap[Double, RTreeNode[T]]()

  {
    border.put(root.rectangle.distance(position), root)
  }

  def next() : RTreeNode[T] = {
    while (border.size > 0
       && (best.size == 0 || border.firstKey < best.firstKey)
    ) {
      val expandKey = border.firstKey
      val expandNode = border.remove(expandKey)
      if (expandNode.isLeafLevel) {
        best.put(expandKey, expandNode)
      } else {
        expandNode.foreach(
          (i, leaf) => { },
          (i, node) => {
            border.put(
              node.rectangle.distance(position),
              node
            )
          }
        )
      }
    }
    if (best.size == 0) {
      null
    } else {
      val key = best.firstKey
      val node = best.remove(key)
      node
    }
  }

}
