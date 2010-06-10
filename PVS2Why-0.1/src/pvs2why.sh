#!/bin/sh
# Script for generating Why (XML and Java) from PVS
# Release: PVS2Why-0.1 (11/10/07)

PVSPATH=CHANGE_TO_PVS_DIR
PVS2WHYPATH=CHANGE_TO_PROOFLITE_DIR

PACK=PVS2Why

#-------------------------------------------------
# Nothing below this line should need modification
#

export PVS2WHYVERSIONMSG="PVS2Why-0.1 (11/10/07)"
export PVS2WHYDEBUG=nil
export PVS2WHYXML=nil
export PVS2WHYJAVA=nil

usage() {
  echo "Usage: pvs2why {<option>}* {<pvsfile>}+"
  echo "Translates PVS functional specifications in <pvsfile> into Why. XML
and Java representations are supported."
  echo "Options:"
  echo " -h|--help                  print this message"
  echo " -l|--lisp <lisp>           PVS lisp version [allegro,cmulisp]"
  echo " -v|--version               print version information"
  echo " -p|--packages <p1>,..,<pn> load packages <p1>,..,<pn>"
  echo " -x|--xml                   generate XML code"
  echo " -j|--java                  generate Java code"
  echo Single one letter options can be combined.
  echo
}

init() {
  if [ -z "$PVS2WHYPACK" ];
  then
    export PVS2WHYPACK=`echo "(\"$PACK\")" | sed -e "s/,/\" \"/g"`
  fi
}

# Pretty print PVS output
print_pvs_output() {
    sed -n -e '/^<pvserror/,/^<\/pvserror/p' \
           -e '/^Typechecking/h' \
           -e '/^Typechecking/ !{
                 /^\*\*\* Typecheck error/ {
                   p;g;p
                 }
                 /^\*\*\* Typecheck error/!H
              }'\
           -e '/^\*\*\*/p' |\
    sed -e "s/^<pvserror msg=\"\(.*\)\">/*** \1 ($FILEDIR\/$FILENAME.pvs)/" \
	-e 's/"//g' -e '/^<\/pvserror>/d' 
}

pvs2why() {
  init
  FILEDIR=`dirname $PVSFILE`
  FILENAME=`basename $PVSFILE .pvs`
  if [ ! -f $FILEDIR/$FILENAME.pvs ];
  then
    echo "File $FILEDIR/$FILENAME.pvs not found"
  else 
    export PVS2WHYFILEDIR=$FILEDIR
    export PVS2WHYFILENAME=$FILENAME
    if [ "$PVS2WHYDEBUG" = "nil" ];
    then
        test -f $FILENAME.debug & rm -f $FILENAME.debug
	$PVSPATH/pvs -raw $PVSLISP -L $PVS2WHYPATH/pvs2why-init.lisp |\
	print_pvs_output
    else 
        echo "Debug file: $FILENAME.debug"
	$PVSPATH/pvs -raw $PVSLISP -L $PVS2WHYPATH/pvs2why-init.lisp |\
	tee $FILENAME.debug | print_pvs_output
    fi
  fi
}

# ... and go!
#

while [ $# -gt 0 ]
do
  case $1 in
      -h|-help|--help)    
	  usage
	  exit 0;;
      -l|-lisp|--lisp)    
	  case $2 in
	      allegro) PVSLISP='-lisp allegro';;
	      cmulisp) PVSLISP='-lisp cmulisp';;
	      *) echo "Only allegro and cmulisp are currently available"
		  exit 1;;
	  esac
	  shift;;
      -v|-version|--version)     
	  echo $PVS2WHYVERSIONMSG
	  exit 0;;
      -p|-packages|--packages) 
	  PACK="$PACK,$2"
	  shift;;
      -d|-debug|--debug)
	  PVS2WHYDEBUG=t;;
      -x|-xml|--xml)
	  PVS2WHYXML=t;;
      -j|-java|--java)
	  PVS2WHYJAVA=t;;
      --*)           
	  usage
	  echo "Error: $1 is not a valid option"
	  exit 1;;
      -*)
	  OPTS=`echo "$1" | sed -e s/-//g -e "s/\(.\)/\1 /g"`
	  for opt in $OPTS
	    do
	    case $opt in
		v) 
		    echo $PVS2WHYVERSIONMSG
		    exit 0;;
		h) 
		    usage
		    exit 0;;

		x)     
		    PVS2WHYXML=t;;
		j)   
		    PVS2WHYJAVA=t;;
		d)
		    PVS2WHYDEBUG=t;;
		*) 
		    usage
		    echo "Error: -$opt is not a valid option"
		    exit 0;;
	    esac
	  done;;  
      *)            
	  PVSFILE="$1"
	  pvs2why
  esac
  shift
done

