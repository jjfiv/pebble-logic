package edu.umass.vde;

import java.util.Map;
import java.util.Set;

/**
 *
 * @author jfoley
 */
public class Substructure extends Structure {
  private final Set<Integer> nodes;

  public Substructure(String name, int size, Set<Integer> nodes, Set<Relation> relations, Map<String, Integer> constants) {
    super(name, size, relations, constants);
    this.nodes = nodes;
  }
  
  @Override
  public Set<Integer> nodes() {
    return nodes;
  }
  
  @Override
  public boolean inDomain(int id) {
    return nodes.contains(id);
  }
}
