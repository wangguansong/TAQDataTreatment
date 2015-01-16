TAQCreateSamples <- function(file.path, check.file=TRUE,
                             exchange=c("ALL", "N", "TQ1"),
                             filter=c(3, 5, 10),
                             frequency=c(0, 1, 2, 5, 10, 30,
                                         60, 300, 600)) {
  # Given a file.path to a stock/date RData file, check if the data is
  # of correct formats (optional), and then create a list of row numbers
  # of various samples.
  # Element of the list: EX.K.FREQ
  #   EX is one of "ALL", "NYSE", "NASDAQ" (exchange)
  #   K is one of A3, A5, A10 (outlier filter strength, allowed sd)
  #   FREQ is one of F0, F1, F2, F5, F10, F30, F60, F300, F600
  # When a filter RData exists, it will be used and overwritten.

  ##### Check the RData file #####
  object.names <- load(file.path)
  stock <- get(object.names)
  rm(object.names)
  filter.path <- sub("\\.RData", "\\.Filter\\.RData", file.path)
  
  if (check.file) {
    headers <- names(stock)
    # general columns
    if (! is.numeric(stock$TIME)) {
      stop(file.path, " has wrong TIME field format.")
    }
    if (is.unsorted(stock$TIME)) {
      stop(file.path, " has unsorted TIME field.")
    }
    if (is.logical(stock$EX)) {
      stop(file.path, " has wrong EX field format.")
    }
    # trade columns
    if ("COND" %in% headers && any(is.na(stock$COND))) {
      stop(file.path, " has wrong COND field format.")
    }
    # quote columns
    if ("MMID" %in% headers) {
      stop(file.path, " has MMID field.")
    }
    if ("MODE" %in% headers && any(is.na(stock$MODE))) {
      stop(file.path, " has wrong MODE field format.")
    }
  }
  
  new.list <- vector(mode="list",
                     length=length(exchange) * length(filter) *
                       length(frequency))
  paste(rep(exchange, each=length(filter)*length(frequency)), ".A",
        rep(filter, each=length(frequency)), ".F",
        frequency, sep="") -> names(new.list)
  basic.list <- list(NORMAL=NULL, ERROR=NULL, OUTLIER3=NULL,
                    OUTLIER5=NULL, OUTLIER10=NULL)
  
  ##### Create list of filters #####
  # load the filter file if exists
  object.names <- character(0)
  if (file.exists(filter.path)) {
    object.names <- load(filter.path)
  }
  # if there is an old filter.list, try to use it
  if ("filter.list" %in% object.names) {
    # exit if the old list contains all the required filters
    if (all(c(names(new.list), names(basic.list)) %in%
              names(filter.list))) {
      return(invisible())
    }
    # otherwise, copy the old filter.list to new.list
    for (filter.name in names(new.list)) {
      if (filter.name %in% names(filter.list)) {
        new.list[[filter.name]] <- filter.list[[filter.name]]
      }
    }
  }
  # clear the finished ones
  splitted.names <- strsplit(names(new.list), "\\.")
  exchange <- unique(sapply(splitted.names, function(y) y[1]))
  filter <- unique(sapply(splitted.names,
                          function(y) substring(y[2], 2)))
  filter <- as.numeric(filter)
  frequency <- unique(sapply(splitted.names,
                             function(y) substring(y[3], 2)))
  frequency <- as.numeric(frequency)
  
  # create filters
  id.normal <- TAQFilter(stock)
  if ("PRICE" %in% names(stock)) {     # quote data
    price <- stock$PRICE
  } else {                             # trade data
    price <- (stock$BID + stock$OFR) / 2
  }
  if (sum(id.normal)<10) {
    id.error <- FALSE
  } else {
    id.error <- TAQError(price[id.normal])
  }
  if (sum(id.normal[! id.error])<10) {
    id.outliers <- matrix(FALSE, nrow=1, ncol=3)
  } else {
    id.outliers <- TAQOutLiersBHLS(price[id.normal][! id.error],
                                   c(3, 5, 10), 25)
  }
  
  basic.list$NORMAL <- id.normal
  basic.list$ERROR <- rep(FALSE, length(id.normal))
  basic.list$ERROR[id.normal][id.error] <- TRUE
  basic.list$OUTLIER3 <- rep(FALSE, length(id.normal))
  basic.list$OUTLIER3[id.normal][! id.error][id.outliers[, 1]] <- TRUE
  basic.list$OUTLIER5 <- rep(FALSE, length(id.normal))
  basic.list$OUTLIER5[id.normal][! id.error][id.outliers[, 2]] <- TRUE
  basic.list$OUTLIER10 <- rep(FALSE, length(id.normal))
  basic.list$OUTLIER10[id.normal][! id.error][id.outliers[, 3]] <- TRUE
  
  # if any of exchange, filter, frequency is empty, only basic filters
  if (length(exchange)*length(filter)*length(frequency)==0) {
    filter.list <- c(basic.list, new.list)
    save(filter.list, file=filter.path)
    return(invisible())
  }

  for (ex in exchange) {
    # create exchange filter
    if (ex=="ALL") {
      id.exchange <- TRUE
    } else {
      id.exchange <- stock$EX %in%
        substring(ex, 1:nchar(ex), 1:nchar(ex))
    }
    rows.kept <- which(id.normal & id.exchange)
    # no need to continue if available points less than 10
    tick.only <- FALSE
    if (length(rows.kept)<=10) tick.only <- TRUE
    
    for (k in filter) {
      if ("PRICE" %in% names(stock)) {
        id.error <- TAQError(stock$PRICE[rows.kept])
        if (is.logical(id.error) && length(id.error)>0) {
          rows.kept <- rows.kept[! id.error]
        }
        id.outlier <- TAQOutLiersBHLS(stock$PRICE[rows.kept], k)
        if (is.logical(id.outlier) && length(id.outlier)>0) {
          rows.kept <- rows.kept[! id.outlier]
        }
      } else {
        id.error <- TAQError((stock$OFR + stock$BID)[rows.kept] / 2)
        if (is.logical(id.error) && length(id.error)>0) {
          rows.kept <- rows.kept[! id.error]
        }
        id.outlier <- TAQOutLiersBHLS(
          (stock$OFR + stock$BID)[rows.kept] / 2, allowed=k)
        if (is.logical(id.outlier) && length(id.outlier)>0) {
          rows.kept <- rows.kept[! id.outlier]
        }
      }
      
      for (freq in frequency) {
        filter.name <- paste(ex, ".A", k, ".F", freq, sep="")
        if (tick.only && freq>0) {
          new.list[[filter.name]] <- NULL
        } else if (freq==0){
          new.list[[filter.name]] <- rows.kept
        } else {
          time.grid <- 9.5*3600 + seq(from=0, to=6.5*3600, by=freq)
          sample.rows <- findInterval(time.grid, stock$TIME[rows.kept])
          sample.rows[which(sample.rows==0)] <- 1
          sample.rows <- rows.kept[sample.rows]
          new.list[[filter.name]] <- sample.rows
          if (length(sample.rows)==0) {
            new.list[[filter.name]] <- NULL
          }
        }
      }
    }
  }
  filter.list <- c(basic.list, new.list)
  save(filter.list, file=filter.path)
}