package edu.umass.vde;

import java.util.Map;
import java.util.Set;

/**
 *
 * @author jfoley
 */
public interface IStructure {
  public Set<Integer> nodes();
  public Set<Relation> relations();
  public Map<String, Integer> constants();
  public int size();
  public String name();
  public boolean inDomain(int id);
}
