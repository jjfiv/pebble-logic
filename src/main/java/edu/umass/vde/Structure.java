package edu.umass.vde;

import java.util.Map;
import java.util.Set;

/**
 *
 * @author jfoley
 */
public class Structure {
  public final Set<Relation> relations;
  public final Map<String, Integer> constants;
  public final String name;
  public final int size;
  
  public Structure(String name, int size, Set<Relation> relations, Map<String, Integer> constants) {
    this.name = name;
    this.size = size;
    this.relations = relations;
    this.constants = constants;
  }
}
