TAQCheckRData <- function(file.path, gather.info=TRUE) {
  # 08/12: It turns out the difference in RData size is small (4%), so
  #   why bother. Edit the file to only change the type.
  # Given a file.path to a stock/date RData file, check the data frame,
  # delete certain variables, and change the type of some variables to
  # shrink the size:
  #   - Check SYMBOL and DATE, then delete
  #   - Change TIME from chr to int (or num for millisecond data)
  #   - Change EX from chr to factor
  #   - (Quote) Delete MMID
  #   - (Trade) Change COND from chr to factor
  #   - (Trade) Delete G127 if it's all zero.
  # Optionally, gather basic information of the data frame and save in a
  # RData file names as FILENAME.Result.RData:
  #   - OBSNUM: number of observations
  #   - EXTAB: table of EX
  #   - MODETAB: (Quote) table of MODE or QU_COND
  #   - CORRTAB: (Trade) table of CORR
  #   - CONDTAB: (Trade) table of COND
  #   - G127TAB: (Trade) table of G127, if not all 0


  ##### Check the RData file #####
  object.name <- load(file.path)
  stock <- get(object.name)
  rm(list=object.name)

  headers <- names(stock)
  # delete.col <- integer(0)
  rdata.changed <- FALSE

  # general columns
  if ("SYMBOL" %in% headers || "SYM_ROOT" %in% headers) {
    symbol <- strsplit(object.name, split="_")[[1]][1]
    col.symbol <- which(headers %in% c("SYMBOL", "SYM_ROOT"))
    if (all(stock[, col.symbol]==symbol)) {
      # delete.col <- c(delete.col, col.symbol)
    } else {
      stop(file.path, " has SYMBOL not consistent with file name.")
    }
  }
  if ("DATE" %in% headers) {
    day <- strsplit(object.name, split="_")[[1]][2]
    if (all(stock$DATE==day)) {
      # delete.col <- c(delete.col, which(headers=="DATE"))
    } else {
      stop(file.path, " has DATE not consistent with file name.")
    }
  }

  # Change EX (or BIDEX, ASKEX for millisecond) to factor
  if ("EX" %in% headers) {
    if (! is.factor(stock$EX)) {
      stock$EX <- as.factor(stock$EX)
      rdata.changed <- TRUE
    }
  }
  if ("BIDEX" %in% headers) {
    if (! is.factor(stock$BIDEX)) {
      stock$BIDEX <- as.factor(stock$BIDEX)
      rdata.changed <- TRUE
    }
  }
  if ("ASKEX" %in% headers) {
    if (! is.factor(stock$ASKEX)) {
      stock$ASKEX <- as.factor(stock$ASKEX)
      rdata.changed <- TRUE
    }
  }

  # Change TIME to seconds pass midnight
  col.time <- which(headers %in% c("TIME", "TIME_M"))
  if (! is.numeric(stock[, col.time])) {
    options(digits.secs=3)
    stock[, col.time] <- as.numeric(
      difftime(strptime(stock[, col.time], format="%H:%M:%OS"),
               strptime("00:00:00", format="%H:%M:%OS"),
               unit="secs"))
    rdata.changed <- TRUE
  }

  # Delete MMID (Quote)
  if ("MMID" %in% headers) {
  #   delete.col <- c(delete.col, which(headers=="MMID"))
    if (! is.factor(stock$MMID)) {
      stock$MMID <- as.factor(stock$MMID)
      rdata.changed <- TRUE
    }
  }

  # Change COND or TR_SCOND to factor (Trade)
  if ("COND" %in% headers || "TR_SCOND" %in% headers) {
    col.cond <- which(headers %in% c("COND", "TR_SCOND"))
    if (! is.factor(stock[, col.cond])) {
      stock[, col.cond] <- as.factor(stock[, col.cond])
      rdata.changed <- TRUE
    }
  }

  # Check G127
  # if ("G127" %in% headers) {
  #   if (all(stock$G127==0)) {
  #     delete.col <- c(delete.col, which(headers=="G127"))
  #   }
  # }

  # Delete columns
  # if (length(delete.col)>0) {
  #   stock <- stock[, -delete.col]
  #   rdata.changed <- TRUE
  # }
  if (rdata.changed) {
    assign(object.name, stock)
    save(list=object.name, file=file.path)
  }


  ##### Obtain basic information #####
  #   - market.time: two times of market open and close
  if (gather.info) {
    headers <- names(stock)
    result.path <- sub("\\.RData", "\\.Result\\.RData", file.path)
    obs.num <- nrow(stock)
    save.list <- c("obs.num")
    if ("PRICE" %in% headers) {
      ex.tab <- table(stock$EX)
      col.corr <- which(headers %in% c("CORR", "TR_CORR"))
      col.cond <- which(headers %in% c("COND", "TR_SCOND"))
      corr.tab <- table(stock[, col.corr])
      cond.tab <- table(stock[, col.cond])
      if ("G127" %in% headers) {
        g127.tab <- table(stock$G127)
        save.list <- c(save.list, "g127.tab")
      }
      save.list <- c(save.list, "ex.tab", "corr.tab", "cond.tab")
    } else {
      if ("EX" %in% headers) {
        ex.tab <- table(stock$EX)
        save.list <- c(save.list, "ex.tab")
      } else {
        bidex.tab <- table(stock$BIDEX)
        askex.tab <- table(stock$ASKEX)
        save.list <- c(save.list, "bidex.tab", "askex.tab")
      }
      col.mode <- which(headers %in% c("MODE", "QU_COND"))
      mode.tab <- table(stock[, col.mode])
      save.list <- c(save.list, "mode.tab")
    }
    save(list=save.list, file=result.path)
  }


  ##### Check the RData file #####
}
