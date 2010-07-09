/* File: nested_updates.java
*  Automatically generated from PVS theory nested_updates (nested_updates.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 4:38:29 7/9/2010 
*/

import PVS2Java.*;

public class nested_updates {

  public nested_updates() {
  }

  static public class R implements Cloneable {
    int x;
    int y;

    public static R[] new_R(int size,Lambda<Integer,R> lambda) {
      R[] arrayR = new R[size];
      for (int i=0;i<size;i++)
        arrayR[i] = lambda.curry(i);
      return arrayR;
    }

    public R(int x,int y) {
      this.x = x;
      this.y = y;
    }

    public R update(int x,int y) {
      this.x = x;
      this.y = y;
      return this;
    }

    public Object clone() {
      try { return super.clone(); }
      catch (CloneNotSupportedException e) {
        return this;
      }
    }
  }

  public R f(final int i) {
    return new R(i+1,i*i);
  }

  // Higher order function f
  public Lambda<Integer,R> f =   new Lambda<Integer,R>(){
    public R curry(final Integer i) {
      return f(i);    }
  }
;

  public R[] g() {
    f[0] = #<why-literal>;
  }

  public R[] h() {
    f[0] = #<why-let>;
  }

}
