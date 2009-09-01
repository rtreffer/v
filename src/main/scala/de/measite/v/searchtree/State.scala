package de.measite.v.searchtree

trait State {

  def next() : Array[State]

  def isTerminal() : boolean

}
