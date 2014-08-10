########################################################################
#!/bin/sh
# 
# Split TAQ data file (csv, gz, or zip) into small pieces according to
# stocks and dates, that is each file contains information of one stock
# and one day.
# The small files are transformed into RData files using an R script.
# An index file for the original large data file is produced for future
# "fast" locating information. Stock symbols, trading dates and numbers
# of observations are printed to the standard output along the process.
# This is in fact an awk script.
# This script can work with trade, quote, and millisecond data.
# The splitted small files are in the same directory as the large file.
# 
# Arguments:
#   the large data file
# Returns:
#   None. The splited files will be located at the same directory as the
#   large file.
#   An index file is generated for fast extraction of single symbol/date
#   in the future.
#   To clean up the directory, run the script SortRData.sh which will
#   create sub-directories for each stock and put the RData files in
#   them.
# Dependence:
#   Rscript (bin) and TAQcsv2RData.R (file).
# Usage:
#   sh TAQSplit2RData.sh BIGFILE.csv.gz ; sh SortRData.sh
#
# TODO(Author, 05/16/2013): This script assumes the input is gz file and
#   there is no index (.ndx) file. If there is an index file it will be
#   rewritten.
# TODO: Test for millisecond
########################################################################
# Check Input

INFILE=$1
# TODO(Author, 05/16/2013): check input
INDEXFILE="$1.ndx"
FILEEXT=$(echo $1 | awk -F. '{ if (NF>1) { print $NF }}')
BASENAME=$(basename $INFILE .$FILEEXT)
BASEDIR=$(dirname $INFILE)

echo "# start at: $(date)" 
echo "# start at: $(date)" >> $INDEXFILE

########################################################################
# AWK Loop

zcat $INFILE | awk '
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
  print "SYMBOL", "DATE", "SKIPLINE", "LINENUM" >> "'$INDEXFILE'"
  header=$0
}

NR==2 {
  old_date=$col_date
  old_symbol=$col_symbol
  skip_line=NR-1
  line_num=1
  outfile="'$BASEDIR'/" old_symbol "_" old_date ".csv"
  print header > outfile
  print $0 >> outfile
  }

NR>2{
  new_date=$col_date
  new_symbol=$col_symbol

  if (new_date==old_date && new_symbol==old_symbol) {
    line_num++
    print $0 >> outfile
  }
  else {
    print old_symbol, old_date, line_num 
    print old_symbol, old_date, skip_line, line_num >> "'$INDEXFILE'"
    old_symbol=new_symbol
    old_date=new_date
    skip_line=NR-1
    line_num=1

    system("Rscript TAQcsv2RData.R " outfile)
    system("rm " outfile)
    outfile="'$BASEDIR'/" new_symbol "_" new_date ".csv"
    print header > outfile
    print $0 >> outfile
  }
}

END {
  print new_symbol, new_date, line_num
  print new_symbol, new_date, skip_line, line_num >> "'$INDEXFILE'"
  print $0 >> outfile
  system("Rscript TAQcsv2RData.R " outfile)
  system("rm " outfile)
  print "-DONE-"
}
'
echo "# end at: $(date)" >> $INDEXFILE
echo "# end at: $(date)"
exit 0
