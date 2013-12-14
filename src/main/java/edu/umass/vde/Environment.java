package edu.umass.vde;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author jfoley
 */
public class Environment {
  public Map<String, IStructure> structures;
  public Map<String, Vocabulary> vocabularies;
  
  private Environment() {
    structures = new HashMap<String, IStructure>();
    vocabularies = new HashMap<String, Vocabulary>();
  }
  
  private static Environment _env = new Environment();
  public static Environment get() {
    return _env;
  }
  
  public static void assign(IStructure struc) {
    get().structures.put(struc.name(), struc);
  }
  public static void assign(Vocabulary vocab) {
    get().vocabularies.put(vocab.name, vocab);
  }
}
