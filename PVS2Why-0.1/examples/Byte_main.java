class Byte_main {
  public static void main(String[] arg) {
    Byte ByteTheo = new Byte();

    boolean[] zero     = ByteTheo.Zero();
    boolean[] neg_zero = ByteTheo.complement(zero);

    System.out.print(" 0 = [");
    for (boolean b:zero)
      System.out.print(b?"1":"0");
    System.out.println("]");
    System.out.print("~0 = [");
    for (boolean b:neg_zero)
      System.out.print(b?"1":"0");
    System.out.println("]");
    System.out.println("Is Zero( 0) = "+ByteTheo.isZero(zero));
    System.out.println("Is Zero(~0) = "+ByteTheo.isZero(neg_zero));
  }
	
}
