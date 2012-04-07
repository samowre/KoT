/* File: test_aliasing.java 
*  Automatically generated from PVS theory test_aliasing (test_aliasing.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 19:15:1 4/6/2012 
*/

import PVS2Java.*;

public class test_aliasing {

  public test_aliasing() {
  }

  public int tab(final int i) {
    if (i == 0) {
      return 3;
    } else { 
      if (i == 1) {
        return 42;
      } else { 
        if (i == 2) {
          return 7;
        } else { 
          return 7;
        }
      }
    }
  }

  // Higher order function tab
  public Lambda<Integer,Integer> tab =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer i) {
      return tab(i);    }
  }
;

  public int[] plus(final int[] tab1,
                    final int[] tab2) {
    return Prelude.new_int(3-1-0+1,    new Lambda<Integer,Integer>(){
      public Integer curry(final Integer i) {
        return tab1[i]+tab2[i];      }
    }
);
  }

  // Higher order function plus
  public Lambda<int[],Lambda<int[],int[]>> plus =   new Lambda<int[],Lambda<int[],int[]>>(){
    public Lambda<int[],int[]> curry(final int[] tab2) {
      return   new Lambda<int[],int[]>(){
    public int[] curry(final int[] tab1) {
      return plus(tab1,tab2);    }
  }
;    }
  }
;

  public int f(final int i) {
    return i;
  }

  // Higher order function f
  public Lambda<Integer,Integer> f =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer i) {
      return f(i);    }
  }
;

  public int[] g() {
    return plus(pvsrestrict(f),tab);
  }

  public Lambda<Integer,Integer> h(final int i) {
    return     new Lambda<Integer,Integer>(){
      public Integer curry(final Integer j) {
        return i+j;      }
    }
;
  }

  // Higher order function h
  public Lambda<Integer,Lambda<Integer,Integer>> h =   new Lambda<Integer,Lambda<Integer,Integer>>(){
    public Lambda<Integer,Integer> curry(final Integer i) {
      return h(i);    }
  }
;

  public Lambda<Integer,Lambda<Integer,Integer>> hbis() {
    return     new Lambda<Integer,Lambda<Integer,Integer>>(){
      public Lambda<Integer,Integer> curry(final Integer x) {
        return (x == 0 ?     new Lambda<Integer,Integer>(){
      public Integer curry(final Integer x1) {
        return (x1 == 0 ? 1 : h(x).curry(x1));      }
    }
 : h(x));      }
    }
;
  }

  static public class test implements Cloneable {
    int[] f;
    int x;

    public static test[] new_test(int size,Lambda<Integer,test> lambda) {
      test[] arraytest = new test[size];
      for (int i=0;i<size;i++)
        arraytest[i] = lambda.curry(i);
      return arraytest;
    }

    public test(int[] f,int x) {
      this.f = f;
      this.x = x;
    }

    public test update(int[] f,int x) {
      this.f = f;
      this.x = x;
      return this;
    }

    public Object clone() {
      try { return super.clone(); }
      catch (CloneNotSupportedException e) {
        return this;
      }
    }
  }

  public test hj() {
    return new test(g,0);
  }

  public test hk() {
    return hj.update(1);
  }

}
