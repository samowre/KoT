/* File: KB3D.java
*  Automatically generated from PVS theory KB3D (KB3D.pvs) 
*  By: PVS2Why-0.1 (11/10/07) 
*  Date: 4:2:32 7/9/2010 
*/

import PVS2Java.*;

public abstract class KB3D {

  float D;
  float H;
  float T;

  public KB3D(float D,float H,float T) {
    this.D = D;
    this.H = H;
    this.T = T;
  }

  static public class Vector3D implements Cloneable {
    float x;
    float y;
    float z;

    public static Vector3D[] new_Vector3D(int size,Lambda<Integer,Vector3D> lambda) {
      Vector3D[] arrayVector3D = new Vector3D[size];
      for (int i=0;i<size;i++)
        arrayVector3D[i] = lambda.curry(i);
      return arrayVector3D;
    }

    public Vector3D(float x,float y,float z) {
      this.x = x;
      this.y = y;
      this.z = z;
    }

    public Vector3D update(float x,float y,float z) {
      this.x = x;
      this.y = y;
      this.z = z;
      return this;
    }

    public Object clone() {
      try { return super.clone(); }
      catch (CloneNotSupportedException e) {
        return this;
      }
    }
  }

  public abstract float sqrt(final float x);

  // Higher order function sqrt
  public Lambda<Float,Float> sqrt =   new Lambda<Float,Float>(){
    public Float curry(final Float x) {
      return sqrt(x);    }
  }
;

  public float sq(final float x) {
    return x*x;
  }

  // Higher order function sq
  public Lambda<Float,Float> sq =   new Lambda<Float,Float>(){
    public Float curry(final Float x) {
      return sq(x);    }
  }
;

  public float sq_dist2D(final Vector3D v) {
    return sq(v.x)+sq(v.y);
  }

  // Higher order function sq_dist2D
  public Lambda<Vector3D,Float> sq_dist2D =   new Lambda<Vector3D,Float>(){
    public Float curry(final Vector3D v) {
      return sq_dist2D(v);    }
  }
;

  public int sign(final float x) {
    if (x == 0) {
      return 0;
    } else { 
      if (x > 0) {
        return 1;
      } else { 
        return -1;
      }
    }
  }

  // Higher order function sign
  public Lambda<Float,Integer> sign =   new Lambda<Float,Integer>(){
    public Integer curry(final Float x) {
      return sign(x);    }
  }
;

  public boolean detection(final Vector3D s,
                           final Vector3D v) {
    if (v.x == 0 && (v.y == 0 && sq_dist2D(s) < sq(D))) {
      return -H < s.z && s.z < H || (v.z > 0 && (s.z < 0 && -H < T*v.z+s.z) || v.z < 0 && (s.z > 0 && H > T*v.z+s.z));
    } else { 
      final float d = 2*s.x*v.x*s.y*v.y+sq(D)*sq_dist2D(v)-(sq(s.x)*sq(v.y)+sq(s.y)*sq(v.x));
      if (d > 0) {
        final float a = sq(v.x)+sq(v.y);
        final float b = s.x*v.x+s.y*v.y;
        if (v.z == 0) {
          return -H < s.z && (s.z < H && ((sq(D) > sq_dist2D(s) || b <= 0) && (d > sq(a*T+b) || a*T+b >= 0)));
        } else { 
          final float t1 = (-(sign(v.z))*H-s.z)/v.z;
          final float t2 = (sign(v.z)*H-s.z)/v.z;
          return (d > sq(a*t2+b) || a*t2+b >= 0) && ((d > sq(a*t1+b) || a*t1+b <= 0) && ((sq(D) > sq_dist2D(s) || b <= 0) && (t2 > 0 && ((d > sq(a*T+b) || a*T+b >= 0) && t1 < T))));
        }
      } else { 
        return false;
      }
    }
  }

  // Higher order function detection
  public Lambda<Vector3D,Lambda<Vector3D,Boolean>> detection =   new Lambda<Vector3D,Lambda<Vector3D,Boolean>>(){
    public Lambda<Vector3D,Boolean> curry(final Vector3D v) {
      return   new Lambda<Vector3D,Boolean>(){
    public Boolean curry(final Vector3D s) {
      return detection(s,v);    }
  }
;    }
  }
;

  public float alpha(final float sx,
                     final float sy) {
    return sq(D)/(sq(sx)+sq(sy));
  }

  // Higher order function alpha
  public Lambda<Float,Lambda<Float,Float>> alpha =   new Lambda<Float,Lambda<Float,Float>>(){
    public Lambda<Float,Float> curry(final Float sy) {
      return   new Lambda<Float,Float>(){
    public Float curry(final Float sx) {
      return alpha(sx,sy);    }
  }
;    }
  }
;

  public float beta(final float sx,
                    final float sy) {
    return D*sqrt(sq(sx)+sq(sy)-sq(D))/(sq(sx)+sq(sy));
  }

  // Higher order function beta
  public Lambda<Float,Lambda<Float,Float>> beta =   new Lambda<Float,Lambda<Float,Float>>(){
    public Lambda<Float,Float> curry(final Float sy) {
      return   new Lambda<Float,Float>(){
    public Float curry(final Float sx) {
      return beta(sx,sy);    }
  }
;    }
  }
;

  public float Q(final float sx,
                 final float sy,
                 final int e) {
    return alpha(sx,sy)*sx+e*beta(sx,sy)*sy;
  }

  // Higher order function Q
  public Lambda<Integer,Lambda<Float,Lambda<Float,Float>>> Q =   new Lambda<Integer,Lambda<Float,Lambda<Float,Float>>>(){
    public Lambda<Float,Lambda<Float,Float>> curry(final Integer e) {
      return   new Lambda<Float,Lambda<Float,Float>>(){
    public Lambda<Float,Float> curry(final Float sy) {
      return   new Lambda<Float,Float>(){
    public Float curry(final Float sx) {
      return Q(sx,sy,e);    }
  }
;    }
  }
;    }
  }
;

  public float contact_time(final float sx,
                            final float sy,
                            final float qx,
                            final float qy,
                            final float vx,
                            final float vy) {
    final float d = vx*(qx-sx)+vy*(qy-sy);
    if (d != 0) {
      return (sq(qx-sx)+sq(qy-sy))/d;
    } else { 
      if (qx == sx && qy == sy) {
        return 0;
      } else { 
        return -1;
      }
    }
  }

  // Higher order function contact_time
  public Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Float>>>>>> contact_time =   new Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Float>>>>>>(){
    public Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Float>>>>> curry(final Float vy) {
      return   new Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Float>>>>>(){
    public Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Float>>>> curry(final Float vx) {
      return   new Lambda<Float,Lambda<Float,Lambda<Float,Lambda<Float,Float>>>>(){
    public Lambda<Float,Lambda<Float,Lambda<Float,Float>>> curry(final Float qy) {
      return   new Lambda<Float,Lambda<Float,Lambda<Float,Float>>>(){
    public Lambda<Float,Lambda<Float,Float>> curry(final Float qx) {
      return   new Lambda<Float,Lambda<Float,Float>>(){
    public Lambda<Float,Float> curry(final Float sy) {
      return   new Lambda<Float,Float>(){
    public Float curry(final Float sx) {
      return contact_time(sx,sy,qx,qy,vx,vy);    }
  }
;    }
  }
;    }
  }
;    }
  }
;    }
  }
;    }
  }
;

  public Vector3D Error() {
    return new Vector3D(0,0,0);
  }

  public Vector3D resolution(final Vector3D s,
                             final Vector3D vo,
                             final Vector3D vi,
                             final int e) {
    if (sq_dist2D(s) < D) {
      return Error();
    } else { 
      final float vx = vo.x-vi.x;
      final float vy = vo.y-vi.y;
      final float qpx = Q(s.x,s.y,e);
      final float qpy = Q(s.y,s.x,-e);
      final float tpq = contact_time(s.x,s.y,qpx,qpy,vx,vy);
      if (tpq > 0) {
        return new Vector3D((qpx-s.x)/tpq+vi.x,(qpy-s.y)/tpq+vi.y,0);
      } else { 
        if (tpq == 0) {
          return new Vector3D((s.x*vy-vx*s.y)*-s.y+vi.x,(s.x*vy-vx*s.y)*s.x+vi.y,0);
        } else { 
          return Error();
        }
      }
    }
  }

  // Higher order function resolution
  public Lambda<Integer,Lambda<Vector3D,Lambda<Vector3D,Lambda<Vector3D,Vector3D>>>> resolution =   new Lambda<Integer,Lambda<Vector3D,Lambda<Vector3D,Lambda<Vector3D,Vector3D>>>>(){
    public Lambda<Vector3D,Lambda<Vector3D,Lambda<Vector3D,Vector3D>>> curry(final Integer e) {
      return   new Lambda<Vector3D,Lambda<Vector3D,Lambda<Vector3D,Vector3D>>>(){
    public Lambda<Vector3D,Lambda<Vector3D,Vector3D>> curry(final Vector3D vi) {
      return   new Lambda<Vector3D,Lambda<Vector3D,Vector3D>>(){
    public Lambda<Vector3D,Vector3D> curry(final Vector3D vo) {
      return   new Lambda<Vector3D,Vector3D>(){
    public Vector3D curry(final Vector3D s) {
      return resolution(s,vo,vi,e);    }
  }
;    }
  }
;    }
  }
;    }
  }
;

}
