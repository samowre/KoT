# Define PVS_DIR and JAVA_HOME such that they point to the installation
# directories of PVS and Java.

# PVS_DIR=
JAVA_HOME=/usr

all: FF KB3D Byte 

clean:
	rm -f *.class *~

# Examples

FF: FF.pvs
	$(PVS_DIR)/pvs2why -jx FF
	$(JAVA_HOME)/bin/javac -cp PVS2Java.jar:. FF.java FF_main.java	
	$(JAVA_HOME)/bin/java -cp PVS2Java.jar:. FF_main

KB3D: KB3D.pvs
	$(PVS_DIR)/pvs2why -jx KB3D
	$(JAVA_HOME)/bin/javac -cp PVS2Java.jar:. KB3D.java KB3D_main.java
	$(JAVA_HOME)/bin/java -cp PVS2Java.jar:. KB3D_main

Byte: Byte.pvs
	$(PVS_DIR)/pvs2why -jx Byte
	$(JAVA_HOME)/bin/javac -cp PVS2Java.jar:. Byte.java Byte_main.java
	$(JAVA_HOME)/bin/java -cp PVS2Java.jar:. Byte_main

Tree: TreeTheory.pvs
	$(PVS_DIR)/pvs2why -jx TreeTheory
	$(JAVA_HOME)/bin/javac -cp PVS2Java.jar:. TreeTheory.java TreeTheory_main.java
	$(JAVA_HOME)/bin/java -cp PVS2Java.jar:. TreeTheory_main
	 

