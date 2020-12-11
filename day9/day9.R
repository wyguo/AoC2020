## Part 1
library(ICSNP)
input <- read.table("day9/input.txt", quote="\"", comment.char="")
input <- as.numeric(t(input))
k <- 25
for(i in (k+1):length(input)){
  x <- input[i]
  xs <- input[(i-1-k):(i-1)]
  xsum <- as.vector(pair.sum(matrix(xs,ncol=1)))
  if(x %in% xsum){
    next
  } else {
    stop(message(x))
  }
}

## Part 2
x0 <- 88311122
# x0 <- 127

input <- input[which(input < x0)]
for(i in seq_along(input)){
  message(i)
  x.list <- x <- input[i]
  success <- F
  j <- i
  while(!success){
    j <- j+1
    x.list <- c(x.list,input[j])
    success <- sum(x.list) >=x0
  }
  if(sum(x.list)==x0)
    stop(min(x.list)+max(x.list)) else next
}
