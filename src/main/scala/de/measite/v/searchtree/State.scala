package de.measite.v.searchtree

trait State {

  def score() : Array[Double]

  def next() : Array[State]

  def isTerminal() : boolean

}
