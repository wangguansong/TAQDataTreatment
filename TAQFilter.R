# Clean TAQ trade data
# Cleaning Rules:
#   All Data:
# P1: Delete entries with a time stamp outside 9:30am and 4pm window
# P2: Delete entries with a bid, ask or transcation price equal to zero
# P3: Retain entries originating from a single exchange
#   Quote Data:
# Q1: Replace multiple quotes on the same timestamp with median of
#     bid/ask
# Q2: Delete entries with negative spread
# Q3: Delete entries with spread more than 50 times the daily median
#     spread
# Q4: Delete entries for which mid-quote deviate by more than 10 mean
#     absolute deviations from a rolling centered median of 50
#     observations.
# Q5: Check MODE or QU_COND
#   Trade Data:
# T1: Delete entries with corrected trades (!CORR %in% c(0,1,2))
# T2: Delete entries with abnormal Sale Condition.
#     (!COND %in% c("E", "F", " ", "@", "*", ""))
# T3: Replace multiple prices on the same timestamp with median
# T4: Delete entries with prices that are above ask+spread or below
#     bid-spread. If quote data not available, use Q4 while replacing
#     "mid-quote" with "price".
# 
# DEFAULT:
# P=c(1,2) 
#   if trade: trade=c(1, 2)    quote=0
#   if quote: quote=c(2, 3, 5)  trade=0

######################################################################
TAQFilter <- function(stock, P, trade, quote, 
                      P1.value=c(34200, 57600), P3.value="all",
                      Q3.value=50, Q4.value=c(10, 25),
                      Q5.value=c("12", "R", "", "0"),
                      T1.value=c(0,1),
                      T2.value=c("E", "F", "", "@", "*"),
                      T4.value=c(10, 25)) {
  # Filter the raw TAQ high frequency data.
  # Args:
  #   P: a numeric vector of numbers 1, 2 and 3, representing filtering
  #      rules P1, P2 and P3.
  #   trade: a numeric vector of numbers 1, 2, 3 and 4, representing
  #          filtering rules T1, T2, T3 and T4.
  #   quote: a numeric vector of numbers 1, 2, 3, 4 and 5, representing
  #          filtering rules Q1, Q2, Q3, Q4 and Q5.
  #   P1.value: a 2-by-1 vector of the beginning and end time of a day 
  #             in term of seconds after midnight.
  #   P3.value: a vector of charactors. If ex.kept is "all", all markets
  #           are kept; if ex.kept is "most", the market with the most
  #           observations is kept; if ex.kept is a vector of single 
  #           characters, e.g. c("D", "N", "P", "T", "Q"), then those
  #           markets are kept. One can also specify the last case by
  #           concatenating the characters, e.g. "DNPTQ".
  #   T1.value: a numeric vector of kept correction code.
  #   T2.value: a character vector of kept sale condition code.
  #   T4.value: a 2-by-1 vector.
  #   Q3.value: a scalar.
  #   Q4.value: a 2-by-1 vector.
  #   Q5.value: a vector of charactors.
  #
  # Returns:
  #   A logical vector denoting the records to keep.


  if (is.character(stock)) {
    object.names <- load(stock)
    i <- 1
    while (!is.data.frame(get(object.names[i]))) { i <- i+1 }
    stock <- get(object.names[i])
  }
  header <- names(stock)
  col.date <- which(header=="DATE")
  col.time <- which(header=="TIME" | header=="TIME_M")
  col.ex <- which(header=="EX")
  col.bid <- which(header=="BID")
  col.ask <- which(header=="OFR" | header=="ASK")
  col.price <- which(header=="PRICE")
  col.prices <- c(col.bid, col.ask, col.price)
  col.bid.ask <- c(col.bid, col.ask)
  col.cond <- which(header=="COND" | header=="TR_SCOND")
  col.mode <- which(header=="MODE" | header=="QU_COND")
  col.corr <- which(header=="CORR" | header=="TR_CORR")
  #col.bidsize <- which(header=="BIDSIZ")
  #col.asksize <- which(header=="OFRSIZ" | header=="ASKSIZ")
  #col.tradesize <- which(header=="SIZE")

  if (length(col.time)==0 | length(col.prices)==0) {
    return(stock)   #error
  }
  rows.kept <- rep(T, nrow(stock))

  if (!exists("P", mode="numeric")) {
    P <- 1:2
  }
  if (!exists("trade", mode="numeric") &
      !exists("quote", mode="numeric")) {
    if (length(col.price)>0) {
      trade <- 1:2
      quote <- 0
    } else {
      trade <- 0
      quote <- c(2, 3, 5)
    }
  } else {
    if (!exists("trade", mode="numeric")) trade <- 0
    if (!exists("quote", mode="numeric")) quote <- 0
  }

  ##############################
  
  # Format Time to seconds away from start of the day:
  if (is.character(stock[, col.time])) {
    seconds <- difftime(strptime(stock[, col.time], format="%H:%M:%OS"),
                        strptime("00:00:00", format="%H:%M:%OS"),
                        unit="secs")
    seconds <- as.numeric(seconds)
  } else {
    seconds <- stock[, col.time]
  }

  ##############################
  # P1: Delete entries with time stamps outside the 09:30am ~ 04:00pm
  # window.
  if (1 %in% P) {
    rows.kept <- rows.kept & (seconds >= P1.value[1]&
                              seconds <= P1.value[2])
  }
  # P2: Delete entries with a zero or negative bid, ask or transaction
  # price.
  if (2 %in% P) {
    for (i in 1:length(col.prices)) {
      rows.kept <- rows.kept & (stock[, col.prices[i]] > 0)
    }
  }
  # P3: Retain entries originating from selected exchanges.
  # A: AMEX   N: NYSE   B: Boston   P: Arca
  # C: NSX    T/Q: NASDAQ   D: NASD ADF and TRF
  # X: Philadelphia   I: ISE    M: Chicago
  # W: CBOE   Z: BATS   1: Nasdaq aug/sep 2006 only
  if (3 %in% P & length(col.ex)>0 & P3.value!="all") {
    if (P3.value=="most") {
      ex.table <- table(stock[, col.ex])
      ex.kept <- names(ex.table)[which.max(ex.table)]
    } else {
      ex.kept <- P3.value
    }

    if (nchar(ex.kept[1])>1) {
      ex.kept <- substring(ex.kept, 1:nchar(ex.kept), 1:nchar(ex.kept))
    }
    rows.kept <- rows.kept & (stock[, col.ex] %in% ex.kept)
  }

  ##############################
  # Basic filter: T1 (CORR), T2 (COND), Q2 (negative spread), Q5 (MODE)

  # T1: Delete entries with corrected trades
  # CORR: Correction Indicator
  # Filter out invalid records:
  #   cancelled trade: CORR==7, 8, 9, 10, 11
  #   corrected time record: CORR==12
  # Keeping records such that CORR==0, 1, 2
  if (1 %in% trade & length(col.corr)>0) {
    rows.kept <- rows.kept & (stock[, col.corr] %in% T1.value)
  }
  # T2: Delete entries with abnormal sale condition
  # COND: Sale Condition
  # Trading day: 34200 to 57600, or COND="T", "U", "P"
  # Possible "bad" conditions:
  #   W: Nasdaq average price trades
  #   Z: Sold Sale, reported later than it occurred
  if (2 %in% trade & length(col.cond)>0) {
    T2.value <- paste(rep(T2.value, each=length(T2.value)),
                      T2.value, sep="") 
    rows.kept <- rows.kept & (stock[, col.cond] %in% T2.value)
  }
  # Q2: Delete entries with negative spread
  # TAQFilterQ2 <- function(stock, col.bid.ask, return.id=F) {
  if (2 %in% quote & length(col.bid.ask)==2) {
    rows.kept <- rows.kept &
      (stock[, col.bid.ask[2]]-stock[, col.bid.ask[1]] >= 0)
  }
  # Q5: Check MODE or QU_COND
  if (5 %in% quote & length(col.mode)>0 ) {
    rows.kept <- rows.kept & (stock[, col.mode] %in% Q5.value)
  }

  ##############################
  # Out liers filter: T4, Q3, Q4
  if (3 %in% quote | 4 %in% quote | 4 %in% trade) {
    # T4: Delete entries with prices that are above ask+spread or below
    # bid-spread. If quote data not available, use Q4 while replacing
    # "mid-quote" with "price".
    if (4 %in% trade & length(col.price)>0) {
      require("zoo")
      testfunction <- function(x, a=Q4.value[1]) {
        point <- median(1:length(x))
        middle <- median(x[-point])
        abs(x[point]-middle) < a * mean(abs(x[-point]-middle))
      }
      rows.kept <- (rows.kept &
                    rollapply(stock[, col.price], 2*Q4.value[2]+1,
                              testfunction, fill=F))
    }
    # Q3: Delete entries with spread more than 50 times the daily median
    # spread
    if (3 %in% quote & length(col.bid.ask)==2) {
      spread <- stock[, col.bid.ask[2]] - stock[, col.bid.ask[1]]
      rows.kept <- rows.kept & (spread <= Q3.value * median(spread))
    }
    # Q4: Delete entries for which mid-quote deviate by more than 10
    # mean absolute deviations from a rolling centered median of 50
    # observations.
    if (4 %in% quote & length(col.bid.ask)==2) {
      mid.quote <- (stock[, col.bid.ask[1]] +
                    stock[, col.bid.ask[2]]) / 2
      require("zoo")
      testfunction <- function(x, a=Q4.value[1]) {
        point <- median(1:length(x))
        middle <- median(x[-point])
        abs(x[point]-middle) < a * mean(abs(x[-point]-middle))
      }
      rows.kept <- (rows.kept &
                    rollapply(stock[, col.price], 2*Q4.value[2]+1,
                              testfunction, fill=F))
    }
  }
  ##############################
  return(rows.kept)
}