TAQOutLiersBHLS <- function(price, allowed=10, k=25) {
  
  if (!is.numeric(price) | length(price)==0 | k<1) {
    return()  #error
  }
  if (k>=length(price)) {
    k <- length(price) - 1
    if (k<=0) {
      return()
    }
  }
  
  N <- length(price)
  rolling.sample <- matrix(NA, nrow=N, ncol=2*k+1)
  for (i in -k:k) {
    rolling.sample[(max(-i, 0)+1):(N-max(i, 0)), i+k+1] <-
      price[(max(i, 0)+1):(N+min(i, 0))]
  }
  
  rolling.median <- apply(rolling.sample, MARGIN=1, FUN=median, na.rm=T)
  rolling.mad <- rowMeans(abs(rolling.sample -
                                rowMeans(rolling.sample, na.rm=T)),
                          na.rm=T)
  id.outliers <- matrix(F, nrow=N, ncol=length(allowed))
  for (i in 1:length(allowed)) {
    id.outliers[, i] <- abs(price - rolling.median) >
                        allowed[i] * rolling.mad
  }
  return(id.outliers)
}