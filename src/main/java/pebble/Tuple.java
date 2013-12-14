package pebble;

import java.util.Arrays;

/**
 *
 * @author jfoley
 */
public class Tuple {
  public  final int[] data;
  public final int arity;
  public final int size;
  
  public Tuple(int arity, int size, int value) {
    this.arity = arity;
    this.size = size;
    data = new int[arity];
    for(int i=0; i<arity; i++) {
      data[arity-(i+1)] = (value / (int) Math.pow(size, i)) % size;
    }
  }
  
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[");
    for(int i=0; i<arity; i++) {
      sb.append(data[arity-(i+1)]);
      if(i+1 != arity) sb.append(" ");
    }
    sb.append("]");
    return sb.toString();
  }
  
  public int get(int index) {
    return data[arity-(index+1)];
  }
  
  @Override
  public int hashCode() {
    int hash = 0;
    for(int i=0; i<arity; i++) {
      hash ^= new Integer(data[i]).hashCode();
    }
    return hash;
    
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Tuple other = (Tuple) obj;
    if (!Arrays.equals(this.data, other.data)) {
      return false;
    }
    if (this.arity != other.arity) {
      return false;
    }
    if (this.size != other.size) {
      return false;
    }
    return true;
  }
  
  
}
