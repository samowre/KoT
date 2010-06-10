class KB3D_main {
  
  public static void main(String[] argv) {
    KB3D kb3d = new KB3D(5,1000,5) { // [nm,ft,min]

	public float sqrt(float n) {
	  return (float) Math.sqrt(n);
	}
      };

    KB3D.Vector3D s  = new KB3D.Vector3D(-10,0,0);
    KB3D.Vector3D vo = new KB3D.Vector3D(5,0,0); 
    KB3D.Vector3D vi = new KB3D.Vector3D(-5,0,0); 
    KB3D.Vector3D v  = new KB3D.Vector3D(vo.x-vi.x,vo.y-vi.y,vo.z-vi.z);
    System.out.println("Conflict = "+kb3d.detection(s,v)+", "+
		       "s = ("+s.x+","+s.y+","+s.z+"), "+
		       "vo = ("+vo.x+","+vo.y+","+vo.z+"), "+
		       "vi = ("+vi.x+","+vi.y+","+vi.z+")");
    KB3D.Vector3D nvo1 = kb3d.resolution(s,vo,vi,-1);
    System.out.print("Resolution = ("+nvo1.x+","+nvo1.y+","+nvo1.z+")");
    KB3D.Vector3D nv1  = new KB3D.Vector3D(nvo1.x-vi.x,nvo1.y-vi.y,nvo1.z-vi.z);
    System.out.println(", Conflict = "+kb3d.detection(s,nv1));
    KB3D.Vector3D nvo2 = kb3d.resolution(s,vo,vi,1);
    System.out.print("Resolution = ("+nvo2.x+","+nvo2.y+","+nvo2.z+")");
    KB3D.Vector3D nv2  = new KB3D.Vector3D(nvo2.x-vi.x,nvo2.y-vi.y,nvo2.z-vi.z);
    System.out.println(", Conflict = "+kb3d.detection(s,nv2));
  }
}
