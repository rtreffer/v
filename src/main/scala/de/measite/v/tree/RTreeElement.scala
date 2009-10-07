package de.measite.v.tree

/**
 * Class for RTree elements.
 */
trait RTreeElement[T] {

  /**
   * Parent of this RTree element.
   */
  var parent : RTreeParent = _
  /**
   * Reference to the RTree root.
   */
  var tree   : RTree[T]    = _

}

