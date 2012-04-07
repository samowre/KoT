/* File: type_test.java 
*  Automatically generated from PVS theory type_test (first_example.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 19:26:37 4/6/2012 
*/

import PVS2Java.*;

public class type_test {

  public type_test() {
  }

  public class my_list {
    public my_list() {}
  }
  
  public class nil extends my_list {
    public nil() {
    }
  }
  
  public class con extends my_list {
    int hd;
    my_list tl;
    public con(int hd,my_list tl) {
      this.hd = hd;
      this.tl = tl;
    }
  }
  
  public int hd(con con) {
    return con.hd;
  }
  
  public my_list tl(con con) {
    return con.tl;
  }
  
  public boolean nilQuestionmark(my_list my_list) {
    return my_list.getClass().getName().equals("nil");
  }
  
  public boolean conQuestionmark(my_list my_list) {
    return my_list.getClass().getName().equals("con");
  }
  
  public Lambda<my_list,Boolean> nilQuestionmark = new Lambda<my_list,Boolean>() {
    public Boolean curry(final my_list my_list) {
      return nilQuestionmark(my_list);
    }
  }
  ;
  public Lambda<my_list,Boolean> conQuestionmark = new Lambda<my_list,Boolean>() {
    public Boolean curry(final my_list my_list) {
      return conQuestionmark(my_list);
    }
  }
  ;

  public int length(final my_list l) {
    if (nilQuestionmark(l)) {
      return 0;
    } else { 
      final int hd = hd((con) l);
      final my_list tl = tl((con) l);
      return length(tl)+1;
    }
  }

  // Higher order function length
  public Lambda<my_list,Integer> length =   new Lambda<my_list,Integer>(){
    public Integer curry(final my_list l) {
      return length(l);    }
  }
;

  public int sum(final my_list l) {
    if (nilQuestionmark(l)) {
      return 0;
    } else { 
      final int hd = hd((con) l);
      final my_list tl = tl((con) l);
      return hd+sum(tl);
    }
  }

  // Higher order function sum
  public Lambda<my_list,Integer> sum =   new Lambda<my_list,Integer>(){
    public Integer curry(final my_list l) {
      return sum(l);    }
  }
;

  public my_list ni() {
    return new nil();
  }

  public my_list l() {
    return new con(0,new con(1,new con(2,new nil())));
  }

  public int n() {
    return sum(l());
  }

}
