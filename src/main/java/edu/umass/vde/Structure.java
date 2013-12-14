package edu.umass.vde;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author jfoley
 */
public class Structure implements IStructure {
  private final Set<Relation> relations;
  private final Map<String, Integer> constants;
  private final String name;
  private final int size;
  
  public Structure(String name, int size, Set<Relation> relations, Map<String, Integer> constants) {
    this.name = name;
    this.size = size;
    this.relations = relations;
    this.constants = constants;
  }
  
  @Override
  public boolean inDomain(int id) {
    return 0 <= id && id < size;
  }

  @Override
  public Set<Integer> nodes() {
    ArrayList<Integer> nl = new ArrayList<Integer>();
    for(int i=0; i<size; i++) {
      nl.add(i);
    }
    return new HashSet(nl);
  }

  @Override
  public Set<Relation> relations() {
    return this.relations;
  }

  @Override
  public Map<String, Integer> constants() {
    return this.constants;
  }

  @Override
  public int size() {
    return this.size;
  }

  @Override
  public String name() {
    return this.name;
  }
  
  @Override
  public String toString() {
    return "structure {" +
            " .name "+this.name +
            " .size "+size+
            " .relations "+relations+
            " .constants "+constants+
            "}";
  }
}
