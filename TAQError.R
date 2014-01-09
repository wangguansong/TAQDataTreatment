TAQError <- function(price, over.median=1, under.median=0.5) {
  return(price > median(price) * (1 + over.median) |
           price < median(price) * (1 - under.median))
}