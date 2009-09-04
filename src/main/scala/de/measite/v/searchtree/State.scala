package de.measite.v.searchtree

/**
 * A simple search tree state. Terminal states are solutions,
 * ordering is defined by natural ordering and following (child)
 * states can be obtained via next.
 */
trait State {

  /**
   * Retrieve the following states (usually the child states).
   */
  def next() : Array[State]

  /**
   * Return true if this state is a terminal (e.g. a solution).
   */
  def isTerminal() : boolean

}
