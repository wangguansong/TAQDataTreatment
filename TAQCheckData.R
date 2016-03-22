# create a data frame for each symbol/date combination, check each
# RData for general informations, fill the data frame.
# Dependence: halfdays.txt
########## Guansong Wang 11/25/2013 ##########

# Loop through each files for testings
data.folder <- "TAQData/"
# taq.rdata$PATH <- sub("/media/Data/data", taq.rdata$PATH)
# check.files.only <- FALSE          # only check if files are good
jobs.id <- seq(nrow(taq.rdata))


# Construct a data frame (taq.file.df) for the paths of each data file.

if (!exists("taq.rdata", mode="list")) {
  
  cat("Creating Rdata files list under folder", data.folder, ":\n")
  RData.list <- list.files(data.folder,
                           pattern="*.RData", recursive=T, full.names=T)
  isRData <- grep("[A-Z]+_[0-9]+\\.RData$", RData.list)
  RData.list <- RData.list[isRData]
  RData.list <- sub("//", "/", RData.list)
  cat("\t Number of Rdata files: ", length(RData.list), "\n")
  
  cat("\t Create Trade/Quote/Millisecond/Haflday tags.\n")
  name.split <- strsplit(RData.list, split="/")
  nr <- length(RData.list)
  nc <- length(name.split[[1]])
  name.split <- unlist(name.split)
  
  taq.type <- name.split[(1:nr)*nc-2]
  file.id <- name.split[(1:nr)*nc]
  file.id <- strsplit(file.id, split="[_\\.]")
  file.id <- unlist(file.id)
  symbol <- file.id[(1:nr)*3-2]
  date <- file.id[(1:nr)*3-1]
  
  taq.rdata <- data.frame(SYMBOL=symbol, DATE=as.numeric(date),
                          PATH=RData.list,
                          TRADE=(taq.type %in% c("trade", "trade_ms")),
                          QUOTE=(taq.type %in% c("quote", "quote_ms")),
                          MS=(taq.type %in% c("trade_ms", "quote_ms")),
                          HALFDAY=FALSE)
  halfdays <- read.table("halfdays.txt")
  halfdays <- halfdays[, 1]
  taq.rdata$HALFDAY[taq.rdata$DATE %in% halfdays] <- TRUE
  taq.rdata$PATH <- as.character(taq.rdata$PATH)
  # the following columns provide extra information and are optional.
  taq.rdata$OBSNUM <- 0             # number of records
  taq.rdata$FILTERFILE <- FALSE     # if exists filter file
  taq.rdata$FILTERNORMAL <- NA      # if exists filter for normal points
  taq.rdata$FILTERERROR <- NA       # if exists filter for obvious error
  taq.rdata$FILTERA3 <- NA          # if exists filter for outlier point
  taq.rdata$FILTERA5 <- NA
  taq.rdata$FILTERA10 <- NA
#   taq.rdata$OKAY <- FALSE
  
  rm(RData.list, date, file.id, isRData, name.split, nc, nr, symbol,
     taq.type)
}

library("doMC")
library("foreach")
registerDoMC(4)

temp.matrix <-
foreach (i = jobs.id, .inorder=FALSE, .combine=rbind) %dopar% {

  rdata.path <- paste(data.folder, taq.rdata$PATH[i], sep="")
  filter.path <- sub("\\.RData", "\\.Filter\\.RData", rdata.path)
  result.path <- sub("\\.RData", "\\.Result\\.RData", rdata.path)

  if (file.exists(result.path)) {
    result.names <-
    tryCatch(
      load(result.path),
      error=function(e) {
        e
      })
    if (inherits(result.names, "error")) {
      print(paste("Error:", i, taq.rdata$SYMBOL[i], taq.rdata$DATE[i]))
      return(c(i, rep(NA, 6)))
    } else {
      print(paste(i, taq.rdata$SYMBOL[i], taq.rdata$DATE[i]))
      return(c(i, basic.filter.info))
    }
  }
  
}

# cbind(vec.obsnum, vec.normal, vec.error, vec.a3, vec.a5, vec.a10) ->
#   taq.rdata[jobs.id, c("OBSNUM", "FILTERNORMAL", "FILTERERROR",
#                        "FILTERA3", "FILTERA5", "FILTERA10")]

# rm(stock, filter.names, filter.path, object.names, rdata.path)
# rm(counter, vec.obsnum, vec.filter, vec.normal, vec.error,
#    vec.a3, vec.a5, vec.a10)

save.image(file="1202.RData)
