# Define PVS_DIR and JAVA_HOME such that they point to the installation
# directories of PVS and Java.

PVS_DIR=~/Documents/STAGE/pvs4.2
JAVA_HOME=/usr

all: first_example test_aliasing reccord

clean:
	mkdir temp 
	mv *_main.java temp
	rm -f *.class *~ *.java *.debug
	mv temp/* .
	rm -r temp

# Examples
first_example: 
	$(PVS_DIR)/pvs2why -j first_example.pvs
	$(JAVA_HOME)/bin/javac -cp $(PVS_DIR)/lib/PVS2Why-0.1/java/PVS2Java.jar:. type_test.java type_test_main.java	
	$(JAVA_HOME)/bin/java -cp $(PVS_DIR)/lib/PVS2Why-0.1/java/PVS2Java.jar:. type_test_main

test_aliasing: 
	$(PVS_DIR)/pvs2why -j test_aliasing.pvs
	$(JAVA_HOME)/bin/javac -cp $(PVS_DIR)/lib/PVS2Why-0.1/java/PVS2Java.jar:. test_aliasing.java test_aliasing_main.java
	$(JAVA_HOME)/bin/java -cp $(PVS_DIR)/lib/PVS2Why-0.1/java/PVS2Java.jar:. test_aliasing_main

reccord: 
	$(PVS_DIR)/pvs2why -j reccord.pvs
	$(JAVA_HOME)/bin/javac -cp $(PVS_DIR)/lib/PVS2Why-0.1/java/PVS2Java.jar:. reccord.java reccord_main.java
	$(JAVA_HOME)/bin/java -cp $(PVS_DIR)/lib/PVS2Why-0.1/java/PVS2Java.jar:. reccord_main


