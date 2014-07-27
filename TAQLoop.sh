#!/bin/sh
#
# Loop through symbols and dates for a given file, then create an index
# file or run a script if provided on each symbol/date combination csv 
# file.
#
# Arguments
#   -i:         if so, create an index file with the same basename.
#   datafile:   the large data file. Accept csv, zip and gz formats.
#   script:     the file name of the script needed to run on each
#               temporary csv file.
#
# Returns:
#   None
#
# Usage:
#   ./TAQLoop.sh [-i] datafile.gz
#   ./TAQLoop.sh [-i] datafile.gz TAQcsv2RData.sh

##################################################
# Check Input

# Initialize
FLAG_INDEX=false
FLAG_SCRIPT=false

while getopts "i" opt
do
  case $opt in
    i)
      FLAG_INDEX=true;;
    \?)
      echo "Unknown option: -$OPTARG" >&2;;
  esac
done

# Clear all options and reset the command line
shift $(( OPTIND -1 ))

# Parse $1, input file
FILEEXT=$(echo $1 | awk -F. '{ if (NF>1) { print $NF }}')
case $FILEEXT in
  csv)
    INFILE="cat $1 ";;
  zip)
    INFILE="zcat $1 ";;
  gz)
    INFILE="zcat $1 ";;
  *)
    echo "Cannot open $FILEEXT file." 
    exit 1;;
esac
if $FLAG_INDEX
then
  INDEXFILE="$1.ndx"
  echo "Creating index file:" $INDEXFILE
fi

# Parse $2, shell script to run during loop
SCRIPT=$2
if [ $SCRIPT ]
then
  FLAG_SCRIPT=true
  if [ -f $SCRIPT ]
  then
    $SCRIPT="./$SCRIPT"
  fi
  echo "Script to run:" $SCRIPT
fi


##################################################
# AWK Loop

$INFILE | awk '
BEGIN {
  FS="[, ]+"
  OFS=","
}
NR==1 {
  for (i=1; i<=NF; i++) {
    if ($i ~ /DATE/) {col_date=i}
    if ($i ~ /SYMBOL|SYM_ROOT/) {col_symbol=i}
  }
  print "SYMBOL", "DATE", "LINENUM"
  if ("'$FLAG_INDEX'"=="true")
    print "SYMBOL", "DATE", "SKIPLINE", "LINENUM" > "'$INDEXFILE'"
  header=$0
}

NR==2 {
  old_date=$col_date
  old_symbol=$col_symbol
  skip_line=NR-1
  line_num=1
  if ("'$FLAG_SCRIPT'"=="true") {
    TEMPFILE="/tmp/" old_symbol "_" old_date ".csv"
    print header > TEMPFILE
    print $0 >> TEMPFILE
  }
}

NR>2{
  new_date=$col_date
  new_symbol=$col_symbol

  if (new_date==old_date && new_symbol==old_symbol) {
    line_num++
    if ("'$FLAG_SCRIPT'"=="true")
      print $0 >> TEMPFILE
  }
  else {
    print old_symbol, old_date, line_num 
    if ("'$FLAG_INDEX'"=="true")
      print old_symbol, old_date, skip_line, line_num >> "'$INDEXFILE'"
    old_symbol=new_symbol
    old_date=new_date
    skip_line=NR-1
    line_num=1

    ##################################################
    # Do something inside this box
    # file name is $TEMPFILE
    if ("'$FLAG_SCRIPT'"=="true") {
      print "-----| result of '$SCRIPT': |-----"
      system("'$SCRIPT' " TEMPFILE)
      system("rm " TEMPFILE)
      print "-----| end of result |-----"
      TEMPFILE="/tmp/" new_symbol "_" new_date ".csv"
      print header > TEMPFILE
      print $0 >> TEMPFILE
    }
    # end of box
    ##################################################
  }
}
END {
  print new_symbol, new_date, line_num
  if ("'$FLAG_INDEX'"=="true")
    print new_symbol, new_date, skip_line, line_num >> "'$INDEXFILE'"
  ##################################################
  # Do something inside this box
  # file name is $TEMPFILE
  if ("'$FLAG_SCRIPT'"=="true") {
    print "result of '$SCRIPT':"
    system("'$SCRIPT' " TEMPFILE)
    system("rm " TEMPFILE)
    TEMPFILE="/tmp/" new_symbol "_" new_date ".csv"
    print header > TEMPFILE
    print $0 >> TEMPFILE
  }
  # end of box
  ##################################################

  print "-DONE-"
}
'
