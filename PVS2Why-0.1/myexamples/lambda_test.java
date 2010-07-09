/* File: lambda_test.java
*  Automatically generated from PVS theory lambda_test (lambda.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 4:22:47 7/9/2010 
*/

import PVS2Java.*;

public class lambda_test {

  public lambda_test() {
  }

  public int t1(final int i) {
    return i;
  }

  // Higher order function t1
  public Lambda<Integer,Integer> t1 =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer i) {
      return t1(i);    }
  }
;

  public int t2(final int i) {
    return i;
  }

  // Higher order function t2
  public Lambda<Integer,Integer> t2 =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer i) {
      return t2(i);    }
  }
;

  public int[] plus1(final int[] u,
                     final int[] v) {
    return Prelude.new_int(3-1-0+1,    new Lambda<Integer,Integer>(){
      public Integer curry(final Integer i) {
        return u[i]+v[i];      }
    }
);
  }

  // Higher order function plus1
  public Lambda<int[],Lambda<int[],int[]>> plus1 =   new Lambda<int[],Lambda<int[],int[]>>(){
    public Lambda<int[],int[]> curry(final int[] v) {
      return   new Lambda<int[],int[]>(){
    public int[] curry(final int[] u) {
      return plus1(u,v);    }
  }
;    }
  }
;

  public int[] plus2(final int[] u,
                     final int[] v) {
    return Prelude.new_int(3-1-0+1,    new Lambda<Integer,Integer>(){
      public Integer curry(final Integer i) {
        return u[i]+v[i];      }
    }
);
  }

  // Higher order function plus2
  public Lambda<int[],Lambda<int[],int[]>> plus2 =   new Lambda<int[],Lambda<int[],int[]>>(){
    public Lambda<int[],int[]> curry(final int[] v) {
      return   new Lambda<int[],int[]>(){
    public int[] curry(final int[] u) {
      return plus2(u,v);    }
  }
;    }
  }
;

  public Lambda<Integer,Integer> plus(final Lambda<Integer,Integer> f,
                                      final Lambda<Integer,Integer> g) {
    return     new Lambda<Integer,Integer>(){
      public Integer curry(final Integer j) {
        return f.curry(j)+g.curry(j);      }
    }
;
  }

}
