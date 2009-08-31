package de.measite.v.tree

import java.util.TreeMap

import de.measite.v.data.KVector

case class NearestLeafIterator[T](position: KVector, root: RTreeNode[T]) {

    val best = new TreeMap[Double, RTreeLeaf[T]]()
    val border = new TreeMap[Double, RTreeNode[T]]()

    {
      border.put(root.rectangle.distance(position), root)
    }

    def next() : RTreeLeaf[T] = {
      while (border.size > 0
         && (best.size == 0 || border.firstKey < best.firstKey)
      ) {
        val expandKey = border.firstKey
        val expandNode = border.get(expandKey)
        border.remove(expandKey)
        if (expandNode.isLeafLevel) {
          var i = 0;
          while (i < expandNode.childs && expandNode.child(i) != null) {
              val child = expandNode.child(i).asInstanceOf[RTreeLeaf[T]]
              best.put(
                child.position - position,
                child
              )
          }
        } else {
          var i = 0;
          while (i < expandNode.childs && expandNode.child(i) != null) {
            val child = expandNode.child(i).asInstanceOf[RTreeNode[T]]
            border.put(
              child.rectangle.distance(position),
              child
            )
            i += 1
          }
        }
      }
      if (best.size == 0) {
        null
      } else {
        val key = best.firstKey
        val node = best.get(key)
        best.remove(key)
        node
      }
    }

}
