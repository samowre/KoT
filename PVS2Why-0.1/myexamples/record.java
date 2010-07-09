/* File: record.java
*  Automatically generated from PVS theory record (record.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 4:22:48 7/9/2010 
*/

import PVS2Java.*;

public class record {

  public record() {
  }

  static public class R implements Cloneable {
    int t;
    float x;
    float y;

    public static R[] new_R(int size,Lambda<Integer,R> lambda) {
      R[] arrayR = new R[size];
      for (int i=0;i<size;i++)
        arrayR[i] = lambda.curry(i);
      return arrayR;
    }

    public R(int t,float x,float y) {
      this.t = t;
      this.x = x;
      this.y = y;
    }

    public R update(int t,float x,float y) {
      this.t = t;
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

  public R a() {
    return new R(3,0,42);
  }

  public R b() {
    return new R(7,1,22);
  }

  public R c() {
    return new R(3,0,42);
  }

  public R d() {
    return a.update(4,41);
  }

}
