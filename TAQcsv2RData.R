# read a csv file and save the data in an RData file.
# This is used by calling Rscript with an argument, the name of the csv
# file.
# The RData file and the data frame inside are both named the same as
# the csv file. The SYMBOL, DATE and other fileds may be unnecessary
# which can be ignored to get a smaller file.
#
# TODO: read.csv column types for milli-second data.
#
# Usage:
#   Rscript TAQcsv2RData.R FILENAME.csv

csvfilename <- commandArgs(trailingOnly=T)
basedir <- dirname(csvfilename)
fileid <- sub("^([^.]*).*", "\\1", basename(csvfilename))
outfilename <- paste(basedir, "/", fileid, ".RData", sep="")

header <- as.character(read.csv(csvfilename, nrow=1,
                                header=FALSE, colClasses="character"))

# DATE is readed as integer rather than factor. Using factor has little
# effect on the size of the RData file.

if ("PRICE" %in% header) {  # If "PRICE" column exists, it's trade.
  if ("TIME_M" %in% header) {
    # TODO: specify column types for milli-second data
  } else {
    coltypes <- c("factor", "integer", "character", "numeric",
                 # SYMBOL,   DATE,      TIME,        PRICE,
                  "integer", "integer", "integer", "factor",
                 # SIZE,      G127,      CORR,      COND,      
                  "factor")
                 # EX

  }
} else {
  if ("TIME_M" %in% header) {
    # TODO: specify column types for milli-second data
  } else {
    coltypes <- c("factor", "integer", "character", "numeric",
                 # SYMBOL,   DATE,      TIME,        BID,
                  "numeric", "integer", "integer", "integer",
                 # OFR,       BIDSIZ,    OFRSIZ,    MODE,
                  "factor", "factor")
                 # EX,       MMID
  }
}
temp.df <- read.csv(csvfilename, colClasses=coltypes)

assign(fileid, temp.df)
save(list=fileid, file=outfilename)
