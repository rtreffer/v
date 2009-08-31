package de.measite.v.tree

import de.measite.v.data.KVector

case class RTreeLeaf[T](position: KVector) extends RTreeElement {

  var data: T = _

  def this(position: KVector, d: T) {
    this(position)
    data = d
    this
  }

  def setData(d : T) {
    data = d
  }

  def getData(d : T) {
    data
  }

  def coerce : T = {
    data
  }

}
