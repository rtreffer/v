package de.measite.v.searchtree

import java.util.TreeSet

/**
 * Search for the first leaf based on the leafs priorities.
 * States have to implement a natural partial ordering and
 * should be collection safe. This means that ever State should
 * override compareTo,hashCode and equals.
 */
object PriorityStragety {

  /**
   * Perform a search, starting with a base state.
   * <ul>
   *   <li>The state implementation must implement compareTo</li>
   *   <li>Child states must be greater or equal to the active state</li>
   * </ul>
   */
  def search(base : State) : State = {
    val tree = new TreeSet[State]()
    tree.add(base)
    search(tree)
  }

  def search(base : Array[State]) : State = {
    val tree = new TreeSet[State]()
    var i = 0
    while(i < base.length) {
      if (base(i) ne null) {
        tree.add(base(i))
      }
      i += 1
    }
    search(tree)
  }

  def search(tree : TreeSet[State]) : State = {
    var result : State = null
    while (tree.size() > 0 && result == null) {
      val state = tree.first();
      tree.remove(state);
      if (state.isTerminal) {
        result = state;
      } else {
        val childs = state.next();
        for (child <- childs) {
          tree.add(child)
        }
      }
    }
    result
  }

}
