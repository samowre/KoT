class record_main {

  public static String R2string(record.R x){
      return "x: "+x.x+", y:"+x.y+", t: "+x.t;
  }

  public static void main(String[] arg) {

    record tt = new record();

    System.out.println(R2string(tt.a()));
    System.out.println(R2string(tt.b()));
    System.out.println(R2string(tt.c()));

    System.out.println(tt.t());
    System.out.println(tt.u());

  }
}
