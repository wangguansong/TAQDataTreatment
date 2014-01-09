TAQExtract <- function(filename, symbol, date, outfile=tempfile()) {

  # Extract the trading dataset of one stock in one day from the big
  # file.
  # Dependence: TAQExtract.sh
  #
  # Args:
  #   filename: the path of the big data file
  #   symbol: symbol of the stock
  #   date: date in format "yyyymmdd"
  #
  # Returns:
  #   a data frame, or NA if no record is found.
  
  date <- as.character(date)
  
  # use file TAQExtract.sh
  shell.command <- paste("sh TAQExtract.sh", filename, symbol, date,
                         outfile)
  system(shell.command)
  
  shell.command <- paste("wc -l", outfile)
  nr <- system(shell.command, intern=T)
  nr <- as.numeric(strsplit(nr, split=" ")[[1]][1])

  if (nr==0) {
    return(NA)
  } else {
    stock <- read.csv(outfile, header=T)
    unlink(outfile)       # Clean up
    file.remove(outfile)
    return(stock)
  }
}
