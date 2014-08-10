########################################################################
#!/bin/sh
SYMBOLLIST=( $(ls *.RData | awk -F[._] '{print $1}' | uniq) )

for SYMBOL in "${SYMBOLLIST[@]}"; do
  echo ${SYMBOL}
  if [ ! -d ${SYMBOL} ]; then
    mkdir ${SYMBOL}
  fi
  mv ${SYMBOL}_*.RData ${SYMBOL}/
done
