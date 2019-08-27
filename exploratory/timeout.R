
n <- 500

output <- matrix(NA,n,2)

for(i in 1:n) {
  x <- sample(1:2e4, 1)
  y <- NULL
  withTimeout({for(z in 1:x) y <- c(y, runif(1))},
              timeout = 0.1, onTimeout = "silent"
             )
  output[i,] <- c(x, length(y))  
}

plot(output, xlim=c(0,max(output)),
             ylim=c(0,max(output)))

#abline(0,1)
