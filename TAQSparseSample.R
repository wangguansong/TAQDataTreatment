TAQSparseSample <- function (stock, ticks, secs, grid) {
  # Given a data frame of trading records of one stock in one day, 
  #
  # Args:
  #   stock: a data frame or the path to a csv file (string)
  #   ticks: number of ticks of sparse sampling
  #   secs: number of seconds of sparse sampling
  #   grid: a vector representing the sampling grid. It may be in term
  #         of either seconds past midnight or fractions of the trading
  #         day. For example, 10:00am can be either 3600*10 or 0.5/6.5.
  # NOTE: only one of the sparse arguments (ticks, secs, grid) should be
  #       provided. If multiple records exist for the same time stamp,
  #       the latest ones are used.
  # TODO(07/26/2014): This script can cause problem when the sample size
  #       is small, as in early years. It needs more error guards.
  #
  # Returns:
  #   A data frame with two columns: TIMEGRID is the time stamp of the
  #   sparse sample; ROWNUM is the corresponding row number of the
  #   original data frame.
  
  by.ticks <- exists("ticks", mode="numeric")
  by.secs <- exists("secs", mode="numeric")
  by.grid <- exists("grid", mode="numeric")

  if ( !(by.ticks + by.secs + by.grid == 1) )
    return(NA)
#   if (is.character(stock)) {
      # load by path
#   }

  header <- names(stock)
  col.time <- which(header=="TIME" | header=="TIME_M")

  if (is.factor(stock[, col.time]) || is.character(stock[, col.time])) {
    seconds <- as.numeric(difftime(
            strptime(stock[, col.time], format="%H:%M:%OS"),
            strptime("00:00:00", format="%H:%M:%OS"),
            unit="secs"))
  } else {
    seconds <- stock[, col.time]
  }

  if (is.unsorted(seconds)) {
    stock <- stock[order(seconds), ]
    seconds <- seconds[order(seconds)]
  }

  if (by.ticks) {
    rows.kept <- seq(from=1, to=nrow(stock), by=ticks)
    return(data.frame(TIMEGRID=seconds[rows.kept], ROWNUM=rows.kept))
  } else if (by.secs) {
    time.grid <- seq(from=seconds[1], to=seconds[nrow(stock)], by=secs)
    rows.kept <- findInterval(time.grid, seconds)
    if (any(rows.kept==0)) {
      rows.kept[which(rows.kept==0)] <- 1
    }
    return(data.frame(TIMEGRID=time.grid, ROWNUM=rows.kept))
  } else if (by.grid) {
    if (all(grid<=1)) {
      grid <- grid*23400 + 9.5*3600
    }
    rows.kept <- findInterval(grid, seconds)
    if (any(rows.kept==0)) {
      rows.kept[which(rows.kept==0)] <- 1
    }
    return(data.frame(TIMEGRID=grid, ROWNUM=rows.kept))
  }

}
