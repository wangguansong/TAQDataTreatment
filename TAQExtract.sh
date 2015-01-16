#!/bin/sh
#
# Extract records from gz file for one stock and one day.
#
# Arguments
#   -i indexfile: the index file (optional). If the index file exists
#                 and is named with ".ndx" suffix, it will be used auto-
#                 matically. If no index file available, awk searchs the
#                 stock/date.
#   datafile:     the large data file. Acceptable formats include csv,
#                 zip and gz.
#   SYMBOL:       the symbol of the stock.
#   Date:         the date of the day (YYYYMMDD)
#   outfile.csv:  the name of outcome file (optional, by default it is
#                 SYMBOL-YYYYMMDD.csv). 
# Returns:
#   None
#
# Usage:
#   ./TAQExtract.sh [-i indexfile.ndx] datafile.gz SYMBOL YYYYMMDD [out.csv]

##################################################
# Parse input arguments

# Initialize
FLAG_INDEX=false

while getopts "i:" opt
do
  case $opt in
    i)
      FLAG_INDEX=true
      INDEXFILE=$OPTARG;;
    \?)
      echo "Unknown option: -$OPTARG" >&2;;
  esac
done

# Clear all options and reset the command line
shift $(( OPTIND -1 ))

# Parse $1, data file
INFILE=$1
if [ -f "$INFILE.ndx" ] && [ !$FLAG_INDEX ]
then
  FLAG_INDEX=true
  INDEXFILE="$INFILE.ndx"
fi
FILEEXT=$(echo $INFILE | awk -F. '{ if (NF>1) { print $NF }}')
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

# Parse $2 $3, 
SYMBOL=$2
DATE=$3

# Parse $4
OUTFILE=$4
if [ -z $OUTFILE ]
then
  OUTFILE="${SYMBOL}_${DATE}.csv"
  echo "Extracted to:" $OUTFILE
fi

echo "Extracting stock ($SYMBOL) at date ($DATE) to file ($OUTFILE)"

##################################################

if $FLAG_INDEX
then
  echo "Use index file ($INDEXFILE)"
  $INFILE | head -n 1 > $OUTFILE
  RECORD=$(grep "${SYMBOL},${DATE}," $INDEXFILE)
  SKIP=$(($(echo $RECORD | awk -F, '{print $3}') + 1))
  LINENUM=$(echo $RECORD | awk -F, '{print $4}')
  $INFILE | tail -n +$SKIP | head -n $LINENUM >> $OUTFILE

else
  echo "No index file provided, use awk"
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
  print $0 > "'$OUTFILE'"
}
$col_date=="'$DATE'" && $col_symbol=="'$SYMBOL'" {
  print $0 >> "'$OUTFILE'"
}
END {
print "SYMBOL: '$SYMBOL'"
print "DATE: '$DATE'"
print "Extracted from file: '$3'"
print "In file: '$OUTFILE'"
print "-DONE-"
}'

fi
