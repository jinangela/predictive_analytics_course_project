outlier.index <- function(x)
{
  lower <- mean(x) - 3*sd(x)
  upper <- mean(x) + 3*sd(x)
  return(which(x < lower | x > upper))
}