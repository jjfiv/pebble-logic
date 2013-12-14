package edu.umass.vde;

import java.util.Set;

/**
 *
 * @author jfoley
 */
public class Relation {
  public final String name;
  public final int arity;
  public final Set<Tuple> entries;
  
  public Relation(String name, int arity, Set<Tuple> entries) {
    assert(arity >= 0);
    this.name = name;
    this.arity = arity;
    this.entries = entries;
  }

  public boolean isConstant() {
    return arity == 0;
  }
  
  public Relation withNewEntries(Set<Tuple> newEntries) {
    return new Relation(name, arity, newEntries);
  }
  
  @Override
  public String toString() {
    return name+":"+arity+"="+this.entries.toString();
  }
  
  @Override
  public int hashCode() {
    return name.hashCode() ^ arity;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Relation other = (Relation) obj;
    if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
      return false;
    }
    if (this.arity != other.arity) {
      return false;
    }
    if (this.entries != other.entries && (this.entries == null || !this.entries.equals(other.entries))) {
      return false;
    }
    return true;
  }
            
}
