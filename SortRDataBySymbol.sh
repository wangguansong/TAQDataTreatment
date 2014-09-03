########################################################################
#!/bin/sh
# Sort RData files into subdirectories by stock symbols.

BASEDIR=$1
if [ -z $BASEDIR ]; then
  BASEDIR="./"
fi

cd $BASEDIR > /dev/null

SYMBOLLIST=( $(ls *.RData | awk -F[._] '{print $1}' | uniq) )

for SYMBOL in "${SYMBOLLIST[@]}"; do
  echo ${SYMBOL}
  if [ ! -d ${SYMBOL} ]; then
    mkdir ${SYMBOL}
  fi
  mv ${SYMBOL}_*.RData ${SYMBOL}/
done

cd - > /dev/null
