# Define PVS_DIR and JAVA_HOME such that they point to the installation
# directories of PVS and Java.

PVS2WHY_DIR=../..
#PVS_DIR=/Users/shankar/pvs
JAVA_HOME=/usr

#all: first_example test_aliasing record

ifndef PVS_DIR
ERRMSG=Either define an environment variable PVS_DIR or call make PVS_DIR=<dir>, where <dir> is the directory where PVS resides
$(error Error: $(ERRMSG))
endif

all: first_example
clean:
	mkdir temp 
	mv *_main.java temp
	rm -f *.class *~ *.java *.debug
	mv temp/* .
	rm -r temp

# Examples
first_example: 
	$(PVS_DIR)/pvs2why -j first_example.pvs
	$(JAVA_HOME)/bin/javac -cp $(PVS2WHY_DIR)/PVS2Why-0.1/java/PVS2Java.jar:. type_test.java type_test_main.java	
	$(JAVA_HOME)/bin/java -cp $(PVS2WHY_DIR)/PVS2Why-0.1/java/PVS2Java.jar:. type_test_main

#test_aliasing: 
#	$(PVS_DIR)/pvs2why -j test_aliasing.pvs
#	$(JAVA_HOME)/bin/javac -cp $(PVS2WHY_DIR)/PVS2Why-0.1/java/PVS2Java.jar:. test_aliasing.java test_aliasing_main.java
#	$(JAVA_HOME)/bin/java -cp $(PVS2WHY_DIR)/PVS2Why-0.1/java/PVS2Java.jar:. test_aliasing_main

#record: 
#	$(PVS_DIR)/pvs2why -j record.pvs
#	$(JAVA_HOME)/bin/javac -cp $(PVS2WHY_DIR)/PVS2Why-0.1/java/PVS2Java.jar:. record.java record_main.java
#	$(JAVA_HOME)/bin/java -cp $(PVS2WHY_DIR)/PVS2Why-0.1/java/PVS2Java.jar:. record_main


