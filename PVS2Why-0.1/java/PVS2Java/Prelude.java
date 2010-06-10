package PVS2Java;
import java.lang.Math;

public class Prelude {

  public static boolean forall(int lb, int ub,Lambda<Integer,Boolean> lambda) {
    for (int i=0;i <= ub-lb;i++) 
      if (!lambda.curry(i))
	return false;
    return true;
  }

  public static boolean exists(int lb, int ub,Lambda<Integer,Boolean> lambda) {
    for (int i=0;i <= ub-lb;i++) 
      if (lambda.curry(i))
	return true;
    return false;
  }

  public static int[] new_int(int size,Lambda<Integer,Integer> lambda) {
    int[] arrayInt = new int[size];
    for (int i=0;i<size;i++) 
	arrayInt[i] = lambda.curry(i);
    return arrayInt;
  }

  public static Integer[] new_Integer(int size,Lambda<Integer,Integer> lambda) {
    Integer[] arrayInt = new Integer[size];
    for (int i=0;i<size;i++) 
      arrayInt[i] = lambda.curry(i);
    return arrayInt;
  }

  public static boolean[] new_boolean(int size,Lambda<Integer,Boolean> lambda) {
    boolean[] arrayBool = new boolean[size];
    for (int i=0;i<size;i++) 
      arrayBool[i] = lambda.curry(i);
    return arrayBool;
  }

  public static float[] new_float(int size,Lambda<Integer,Float> lambda) {
    float[] arrayFloat = new float[size];
    for (int i=0;i<size;i++) 
      arrayFloat[i] = lambda.curry(i);
    return arrayFloat;
  }
  
  public static float[] update(float[] array, int i, float value) {
    array[i] = value;
    return array;
  }

  public static int[] update(int[] array, int i, int value) {
    array[i] = value;
    return array;
  }

  public static boolean[] update(boolean[] array, int i, boolean value) {
    array[i] = value;
    return array;
  }

  public static boolean implies(boolean a, boolean b) {
    return !a || b;
  }

  public static boolean iff(boolean a, boolean b) {
    return a == b;
  }

  public static boolean when(boolean a, boolean b) {
    return !b || a;
  }
  
  public static double floor(double a) {
    return Math.floor(a);
  }
  
  public static double ceiling(double a) {
    return Math.ceil(a);
  }
  
  public static boolean even(int a) {
    return a % 2 == 0;
  }

  public static boolean odd(int a) {
    return a % 2 == 1;
  }

}
