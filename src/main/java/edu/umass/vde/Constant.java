package edu.umass.vde;

/**
 *
 * @author jfoley
 */
public class Constant {
  public final String name;
  public final int value;
  public Constant(String name, int value) {
    this.name = name;
    this.value = value;
  }
  public boolean isConstant() { return true; }
  public int arity() { return 0; }
  
  @Override
  public int hashCode() {
    return name.hashCode() ^ value;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Constant other = (Constant) obj;
    if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
      return false;
    }
    if (this.value != other.value) {
      return false;
    }
    return true;
  }
}
