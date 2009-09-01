package de.measite.v.searchtree

import java.util.TreeSet
import java.util.Comparator

/**
 * Search for the first leaf based on the leafs priorities
 */
object PriorityStragety {

  def search(base : State) : State = {
    val tree = new TreeSet[State]()
    tree.add(base)
    while (tree.size() > 0) {
      val state = tree.first();
      tree.remove(state);
      val childs = state.next();
      if (state.isTerminal) {
        return state;
      }
      tree.add(childs(0))
      if (childs.length > 1) {
        tree.add(childs(1))
      }
    }
    null
  }

}
