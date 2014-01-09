TAQCondPlot <- function(stock, ex.kept="all", spread.sd.allowed=50,
                        mid.quote.points) {
  # Plot time series of high frequency trading data, highlighting
  # abnormal points.
  # 
  # Args:
  #   stock: a data frame or the path to a RData file (string)
  #   ex.kept: a vector of charactors. If ex.kept is "all", all markets
  #           are kept; if ex.kept is "most", the market with the most
  #           observations is kept; if ex.kept is a vector of single 
  #           characters, e.g. c("D", "N", "P", "T", "Q"), then those
  #           markets are kept. One can also specify the last case by
  #           concatenating the characters, e.g. "DNPTQ".
  #   spread.sd.allowed: a numeric. The function consider a spread too
  #                      large if it excesses this number times the
  #                      median of all spreads.
  #   mid.quote.points: a logical variable. If the mid-quotes are
  #                     plotted as points or line. When not provided,
  #                     they are plotted if the number of good records
  #                     is smaller than 10000.
  #
  # Returns:
  #   A plot is drawn. None is returned.

  if (is.character(stock)) {
    object.names <- load(stock)
    i <- 1
    while (!is.data.frame(get(object.names[i]))) { i <- i+1 }
    stock <- get(object.names[i])
  }

  header <- names(stock)
  col.symbol <- which(header=="SYMBOL" | header=="SYM_ROOT")
  col.date <- which(header=="DATE")
  col.time <- which(header=="TIME" | header=="TIME_M")
  col.ex <- which(header=="EX")
  col.bid <- which(header=="BID")
  col.ask <- which(header=="OFR" | header=="ASK")
  col.price <- which(header=="PRICE")
  col.bid.ask <- c(col.bid, col.ask)
  col.prices <- c(col.bid, col.ask, col.price)
  if (length(col.prices)==0 | length(time)==0)
     return()   # something is wrong, no column of prices or time found
  tq <- ifelse(length(col.price)>0, "trade", "quote")
  
  col.cond <- which(header=="COND" | header=="TR_SCOND")
  col.mode <- which(header=="MODE" | header=="QU_COND")
  col.corr <- which(header=="CORR" | header=="TR_CORR")

  if (is.factor(stock[, col.time]) | is.character(stock[, col.time])) {
    seconds <- difftime(strptime(stock[, col.time], format="%H:%M:%OS"),
                        strptime("00:00:00", format="%H:%M:%OS"),
                        unit="secs")
    seconds <- as.numeric(seconds)
  } else if (is.numeric(stock[, col.time])) {
    seconds <- stock[, col.time]
  }
  

  ######################################################################
  # rows to kept out
  # records when market is closed
  rows.time <- (seconds<34200) | (seconds>57600)

  # pick out the records with are not in the markets chosen by ex.kept
  if (length(col.ex)>0 & (!"all" %in% ex.kept)) {
    if ("most" %in% ex.kept) {
      ex.table <- table(stock[, col.ex])
      ex.kept <- names(ex.table)[which.max(ex.table)]
    }
    if (nchar(ex.kept[1])>1) {
      ex.kept <- substring(ex.kept, 1:nchar(ex.kept), 1:nchar(ex.kept))
    }
    rows.ex <- ! stock[, col.ex] %in% ex.kept
  } else
    rows.ex <- F

  ######################################################################
  # start to plot!
  hours <- seconds / 3600
  xrange <- range(hours[!rows.ex])
  
  X11(width=12)
  
  ######################################################################
  if (tq=="trade") {

    # negative price, almost never happens for trading data
    if (length(col.price)>0) {
      rows.neg <- stock[, col.price]<=0
    } else {
      rows.neg <- F
    }
    # bad trading conditions
    if (length(col.cond)>0) {
      rows.cond <- !(stock[, col.cond] %in%
                     c("E", "F", "@", "*", "", "@F"))
    } else {
      rows.cond <- F
    }
    # with non-zero correction code
    if (length(col.corr)>0) {
      rows.corr <- stock[, col.corr]>1
    } else
      rows.corr <- F
    # "good" records
    rows.kept <- !(rows.time | rows.ex | rows.neg |
                   rows.cond | rows.corr)

    yrange <- range(stock[!rows.neg, col.price])
    # market open/close vertical lines
    plot(rep(9.5, 2), yrange, type="l", lty=2, col="black",
          xlim=xrange, ylim=yrange, xlab="Time (Hours)", ylab="Price")
    lines(rep(16, 2), yrange, type="l", lty=2, col="black")
    title(main=paste("Abnormal Points Plot, Trade",
                     paste(ex.kept, collapse=""),
                     stock[1, col.symbol], stock[1, col.date],
                     sep=", "))
    # pin bad points
    points(hours[rows.time], stock[rows.time, col.price],
           type="p", pch=20, cex=.2, col="blue")
    points(hours[rows.ex], stock[rows.ex, col.price],
           type="p", col="grey", cex=.1)
    points(hours[rows.cond], stock[rows.cond, col.price],
           type="p", col="red")
    points(hours[rows.corr], stock[rows.corr, col.price],
           type="p", col="green", pch=2)
    # pin good points
    points(hours[rows.kept], stock[rows.kept, col.price],
           col="black", pch=20, cex=.2)
    # draw legend. May need to change position accordingly
    legend("topright",
           legend=c("Normal", "Pre/Post Market", "COND", "CORR",
                    "Other EX"),
           pch=c(20, 20, 1, 2, 20),
           col=c("black", "blue", "red", "green", "grey"))

  ######################################################################
  } else if (tq=="quote") {
    
    if (length(col.bid)>0 & length(col.ask)>0) {
      spread <- stock[, col.ask] - stock[, col.bid]
      # negative spread
      rows.neg <- (stock[, col.bid]<=0) | (stock[, col.ask]<=0)
      # too large spread
      rows.spread <- (spread < 0) |
                     (spread > median(spread)*spread.sd.allowed)
    } else {
      return()  # one of bid or ask is missing, something is wrong
    }
    if (length(col.mode)>0) {
      # bad mode
      rows.mode <- ! stock[, col.mode] %in% c("12", "R")
    } else {
      rows.mode <- F
    }
    
    # good points
    rows.kept <- ! (rows.time | rows.ex | rows.neg | rows.spread |
                    rows.mode)
    # Plot
    if (!exists("mid.quote.points", mode="logical")) {
      mid.quote.points <- sum(rows.kept)<10000
    }
    yrange <- range(stock[!(rows.spread | rows.neg | rows.ex),
                          col.bid.ask])
    mid.quote <- rowMeans(stock[, col.bid.ask])
    q <- plot(rep(9.5, 2), yrange, type="l", lty=2, col="black",
              xlim=xrange, ylim=yrange,
              xlab="Time (Hours)", ylab="Price")
    lines(rep(16, 2), yrange, type="l", lty=2, col="black")
    title(main=paste("Abnormal Points Plot, Quote ",
                     stock[1, col.symbol], stock[1, col.date],
                     sep=", "))
    polygon(c(hours[rows.kept], rev(hours[rows.kept])),
            c(stock[rows.kept, col.ask],
              rev(stock[rows.kept, col.bid])),
            col="grey", border=NA)
    if (mid.quote.points) {
      points(hours[rows.kept], mid.quote[rows.kept],
             type="p", pch=20, cex=.05, col="blue")
    } else {
      lines(hours[rows.kept], mid.quote[rows.kept],
            type="l", lty=1, lwd=1, col="blue")
    }
    # pre market
    rows.kept <- (!rows.ex) & (seconds<34200) & 
                 (!(rows.neg|rows.spread|rows.mode))
    polygon(c(hours[rows.kept], rev(hours[rows.kept])),
            c(stock[rows.kept, col.ask],
              rev(stock[rows.kept, col.bid])),
            col="grey", border=NA)
    if (mid.quote.points) {
      points(hours[rows.kept], mid.quote[rows.kept],
             type="p", pch=20, cex=.05, col="blue")
    } else {
      lines(hours[rows.kept], mid.quote[rows.kept],
            type="l", lty=1, lwd=.5, col="blue")
    }
    # post market
    rows.kept <- (!rows.ex) & (seconds>57600) & 
      (!(rows.neg|rows.spread|rows.mode))
    polygon(c(hours[rows.kept], rev(hours[rows.kept])),
            c(stock[rows.kept, col.ask],
              rev(stock[rows.kept, col.bid])),
            col="grey", border=NA)
    lines(hours[rows.kept], mid.quote[rows.kept],
          type="l", lty=1, lwd=1, col="blue")
    
    # abnormal points
    rows.kept <- (!rows.ex) & (rows.neg | rows.spread | rows.mode)


    badpoints <- which(rows.kept)
    for (i in badpoints) {
      lines(rep(hours[i], 2), stock[i, c(col.bid, col.ask)],
            type="l", lty=2, lwd=.7, col="red")
    }
    # draw legend. May need to change position accordingly
    legend("topright",
           legend=c("Mid-quote", "Bid-Ask Spread", "Abnormal Points"),
           pch=c(20, 20, NA),
           lty=c(NA, NA, 2),
           col=c("blue", "gray", "red"))

  }
}
