input <- read.table("day8/input.txt", quote="\"", comment.char="")
input <- data.frame(input)
colnames(input) <- c('boot','value')

#######################################
###---> part1
x <- 0
success <- F
idx <- 1
idx_list <- c()
x_list <- c()
while (!success) {
  message(idx)
  idx_list <- c(idx_list,idx)
  if(input$boot[idx]=='nop') {
    idx <- idx+1
    x <- x
  } else if (input$boot[idx]=='acc') {
    x <- x+input$value[idx]
    idx <- idx+1
  } else if (input$boot[idx]=='jmp') {
    x <- x
    idx <- idx+input$value[idx]
  }
  x_list <- c(x_list,x)
  success <- (idx %in% idx_list) | (idx == nrow(input)+1)
}


##############################################
###---> part2
input0 <- input


## nop to jmp
change_idx <- which(input$boot=='jmp')
change_idx <- which(input$boot=='jmp')
for(i in change_idx){
  # message(i)s
  input <- input0
  input[i,1] <- 'nop'
  x <- 0
  success <- F
  idx <- 1
  idx_list <- c()
  x_list <- c()
  while (!success) {
    # message(idx)
    idx_list <- c(idx_list,idx)
    if(input$boot[idx]=='nop') {
      idx <- idx+1
      x <- x
    } else if (input$boot[idx]=='acc') {
      x <- x+input$value[idx]
      idx <- idx+1
    } else if (input$boot[idx]=='jmp') {
      x <- x
      idx <- idx+input$value[idx]
    }
    x_list <- c(x_list,x)
    success <- (idx %in% idx_list) | (idx == nrow(input)+1)
  }
  if(idx == nrow(input)+1){
    message(i)
    message(x)
    break
  }
}


