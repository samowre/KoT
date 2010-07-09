class test_aliasing_main{

  public static void main(String[] arg) {

    test_aliasing t = new test_aliasing();

    for(int i = 0; i < 3; i++){
	System.out.println("tab("+i+") = "+t.tab(i));
    }
    for(int i = 0; i < 3; i++){
	System.out.println("res("+i+") = "+t.res()[i]);
    }
    

 
  }
}
