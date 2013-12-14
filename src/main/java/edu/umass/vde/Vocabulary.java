package edu.umass.vde;

import java.util.Map;
import java.util.Set;

/**
 *
 * @author jfoley
 */
public class Vocabulary {
  public Map<String, Integer> relations;
  public Set<String> constants;
  public String name;
  
  public Vocabulary(String id, Map<String, Integer> relations, Set<String> constants) {
    this.name = id;
    this.relations = relations;
    this.constants = constants;
  }
  
  @Override
  public String toString() {
    return "vocab{" +
            " .name " + name +
            " .relations " + relations +
            " .constants " + constants +
            "}";
  }
}
