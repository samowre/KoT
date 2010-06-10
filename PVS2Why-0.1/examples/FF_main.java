class FF_main {

  public static void main(String[] arg) {
    FF FFTheo = new FF();
    
    for (int n = 0;n <= 10; n++) {
      System.out.println("sum_fibo("+n+") = "+FFTheo.sum_fibo(n));
      System.out.println("sum_fact("+n+") = "+FFTheo.sum_fact(n));
    }
  }
}
