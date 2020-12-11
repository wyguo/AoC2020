## Part 1
input <- read.table("day10/input.txt", quote="\"", comment.char="",header = F)
input <- as.numeric(t(input))
table(diff(c(0,sort(input),max(input)+3)))

## Part 2
options(scipen=999)
input <- sort(input)
result <- c(1,rep(NA,length(input)))
names(result) <- c(0,input)

for(i in input){
  result[paste0(i)] <- 
    ifelse(is.na(result[paste0(i-1)]),0,result[paste0(i-1)])+
    ifelse(is.na(result[paste0(i-2)]),0,result[paste0(i-2)])+
    ifelse(is.na(result[paste0(i-3)]),0,result[paste0(i-3)])
}
tail(result,1)

