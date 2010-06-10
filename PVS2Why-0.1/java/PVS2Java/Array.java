package PVS2Java;

import java.util.ArrayList;

public class Array<T> {
  
  public T[] init(T[] array, Lambda<Integer,T> fun) {
    for (int i=0; i < array.length;i++)
      array[i]=fun.curry(i);
    return array;
  }

  @SuppressWarnings("unchecked")
  public T[] new_array(int size,Lambda<Integer,T> lambda) {
    ArrayList<T> array = new ArrayList<T>(size);

    for (int i=0;i<size;i++)
	array.set(i,lambda.curry(i));
    
    return (T[]) array.toArray();
  }
}
