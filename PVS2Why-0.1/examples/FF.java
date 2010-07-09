/* File: FF.java
*  Automatically generated from PVS theory FF (FF.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 4:2:30 7/9/2010 
*/

import PVS2Java.*;

public class FF {

  public FF() {
  }

  public int factorial(final int n) {
    if (n == 0) {
      return 1;
    } else { 
      return n*factorial(n-1);
    }
  }

  // Higher order function factorial
  public Lambda<Integer,Integer> factorial =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer n) {
      return factorial(n);    }
  }
;

  public Lambda<Integer,Integer> fibonacci(final int i,
                                           final int j) {
    return     new Lambda<Integer,Integer>(){
      public Integer curry(final Integer n) {
        return (n == 0 ? i : (n == 1 ? j : fibonacci(j,i+j).curry(n-1)));      }
    }
;
  }

  // Higher order function fibonacci
  public Lambda<Integer,Lambda<Integer,Lambda<Integer,Integer>>> fibonacci =   new Lambda<Integer,Lambda<Integer,Lambda<Integer,Integer>>>(){
    public Lambda<Integer,Lambda<Integer,Integer>> curry(final Integer j) {
      return   new Lambda<Integer,Lambda<Integer,Integer>>(){
    public Lambda<Integer,Integer> curry(final Integer i) {
      return fibonacci(i,j);    }
  }
;    }
  }
;

  public int sum(final int n,
                 final Lambda<Integer,Integer> f) {
    if (n == 0) {
      return 0;
    } else { 
      return f.curry(n-1)+sum(n-1,f);
    }
  }

  public int sum_fibo(final int n) {
    return sum(n,fibonacci(1,1));
  }

  // Higher order function sum_fibo
  public Lambda<Integer,Integer> sum_fibo =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer n) {
      return sum_fibo(n);    }
  }
;

  public int sum_fact(final int n) {
    return sum(n,factorial);
  }

  // Higher order function sum_fact
  public Lambda<Integer,Integer> sum_fact =   new Lambda<Integer,Integer>(){
    public Integer curry(final Integer n) {
      return sum_fact(n);    }
  }
;

}
