/* File: Byte.java
*  Automatically generated from PVS theory Byte (Byte.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 4:2:34 7/9/2010 
*/

import PVS2Java.*;

public class Byte {

  public Byte() {
  }

  public boolean[] Zero() {
    return Prelude.new_boolean(8-1-0+1,    new Lambda<Integer,Boolean>(){
      public Boolean curry(final Integer i) {
        return false;      }
    }
);
  }

  public boolean[] complement(final boolean[] b) {
    return Prelude.new_boolean(8-1-0+1,    new Lambda<Integer,Boolean>(){
      public Boolean curry(final Integer i) {
        return !b[i];      }
    }
);
  }

  // Higher order function complement
  public Lambda<boolean[],boolean[]> complement =   new Lambda<boolean[],boolean[]>(){
    public boolean[] curry(final boolean[] b) {
      return complement(b);    }
  }
;

  public boolean isZero(final boolean[] b) {
    return Prelude.forall(0,8-1,    new Lambda<Integer,Boolean>(){
      public Boolean curry(final Integer i) {
        return !b[i];      }
    }
);
  }

  // Higher order function isZero
  public Lambda<boolean[],Boolean> isZero =   new Lambda<boolean[],Boolean>(){
    public Boolean curry(final boolean[] b) {
      return isZero(b);    }
  }
;

}
