TAQApplyFilter <- function(file.path, no.filter=FALSE) {
  # Load a data file, check its condition, and apply filters
  # Args:
  #   file.path: string, path to the data file.
  #   no.filter: check raw data only, do not apply filters
  # Returns:
  #   No returns.
  object.names <- load(file.path)
  stock <- get(object.names)
  filter.path <- sub("\\.RData", "\\.Filter\\.RData", file.path)
  
  ##### check raw data #####
  flag.updated <- FALSE           # if the raw data is updated
  headers <- names(stock)
  
  # general columns
  if ("SYMBOL" %in% headers && length(unique(stock$SYMBOL))==1) {
    stock <- subset(stock, select=-SYMBOL)
    flag.updated <- TRUE
  }
  if ("DATE" %in% headers && length(unique(stock$DATE))==1) {
    stock <- subset(stock, select=-DATE)
    flag.updated <- TRUE
  }
  if ("TIME" %in% headers && ! is.numeric(stock$TIME)) {
    seconds <- difftime(strptime(stock$TIME, format="%H:%M:%OS"),
                        strptime("00:00:00", format="%H:%M:%OS"),
                        unit="secs")
    stock$TIME <- as.numeric(seconds)
    flag.updated <- TRUE
  }
  if ("EX" %in% headers && is.logical(stock$EX)) {
    stock$EX <- "T"
    flag.updated <- TRUE
  }
  # trade columns
  if ("COND" %in% headers && any(is.na(stock$COND))) {
    stock$COND[is.na(stock$COND)] <- ""
    flag.updated <- TRUE
  }
  # quote columns
  if ("MMID" %in% headers) {
    stock <- subset(stock, select=-MMID)
    flag.updated <- TRUE
  }
  if ("MODE" %in% headers && any(is.na(stock$MODE))) {
    stock$MODE[is.na(stock$MODE)] <- ""
    flag.updated <- TRUE
  }
  
  # update raw data file
  if (flag.updated) {
    assign(object.names, stock)
    save(list=object.names, file=file.path)
  }
  
  ##### apply filters #####
  if (no.filter) return()
  id.normal <- TAQFilter(stock)
  
  if (sum(id.normal)<10) {
    save(list=c("id.normal"), 
         file=filter.path)
    return()
  }
  
  if (is.null(stock$PRICE)) {     # quote data
    id.error.df <- data.frame(
      BID=stock$BID > 2 * median(stock$BID[id.normal]) |
        stock$BID < 0.5 * median(stock$BID[id.normal]),
      OFR=stock$OFR > 2 * median(stock$OFR[id.normal]) |
        stock$OFR < 0.5 * median(stock$OFR[id.normal]),
      MID=(stock$BID + stock$OFR) / 2 >
        2 * median((stock$BID + stock$OFR)[id.normal] / 2) |
        (stock$BID + stock$OFR) / 2 < 
        0.5 * median((stock$BID + stock$OFR)[id.normal] / 2))
    id.error <- id.error.df$BID | id.error.df$OFR | id.error.df$MID
    
    id.bid <- TAQOutLiersBHLS(stock$BID[id.normal & !id.error],
                              c(3, 5, 10), 25)
    id.ofr <- TAQOutLiersBHLS(stock$OFR[id.normal & !id.error],
                              c(3, 5, 10), 25)
    id.mid <- TAQOutLiersBHLS(((stock$OFR + stock$BID) / 2)
                              [id.normal & !id.error],
                              c(3, 5, 10), 25)
    id.outliers.df.a3 <-
      data.frame(BID=id.bid[, 1], OFR=id.ofr[, 1], MID=id.mid[, 1])
    id.outliers.a3 <- (id.outliers.df.a3$BID | id.outliers.df.a3$OFR | 
                         id.outliers.df.a3$MID)
    
    id.outliers.df.a5 <-
      data.frame(BID=id.bid[, 2], OFR=id.ofr[, 2], MID=id.mid[, 2])
    id.outliers.a5 <- (id.outliers.df.a5$BID | id.outliers.df.a5$OFR | 
                         id.outliers.df.a5$MID)
    
    id.outliers.df.a10 <-
      data.frame(BID=id.bid[, 3], OFR=id.ofr[, 3], MID=id.mid[, 3])
    id.outliers.a10 <- (id.outliers.df.a10$BID |
                          id.outliers.df.a10$OFR | id.outliers.df.a10$MID)
    
    save(list=c("id.normal", "id.error.df", "id.error",
                "id.outliers.df.a3", "id.outliers.a3",
                "id.outliers.df.a5", "id.outliers.a5",
                "id.outliers.df.a10", "id.outliers.a10"),
         file=filter.path)
  } else {                        # trade date
    id.error <- stock$PRICE > 2 * median(stock$PRICE[id.normal]) |
      stock$PRICE < 0.5 * median(stock$PRICE[id.normal])
    id.price <- TAQOutLiersBHLS(stock$PRICE[id.normal & !id.error],
                                c(3, 5, 10), 25)
    id.outliers.a3 <- id.price[, 1]
    id.outliers.a5 <- id.price[, 2]
    id.outliers.a10 <- id.price[, 3]
    save(list=c("id.normal", "id.outliers.a3", "id.outliers.a5",
                "id.outliers.a10", "id.error"),
         file=filter.path)
  }
}
