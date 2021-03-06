# Martingale simulation by Matt Asher
# Created for StatisticsBlog.com
potSize = 1000

# Win percent
p = .5264

iter = 1

potHistory = c(potSize, rep(NA, 999))

betSize = 1

while(potHistory[iter] > 0 && iter < 100000) {
  iter = iter + 1
  
  # To increase efficiency, we're going to play a fun little game.
  # Since we don't know how big the history will be, we're going to 
  # increase our vector size in chunks instead of one at a time
  if(iter %% 25000 == 0 ) {
    potHistory = c(potHistory, rep(NA, 25000))
  }
  
  result = runif(1)
  
  if(result > p) {
    potHistory[iter] = potHistory[(iter-1)] + betSize
    
    # reset our bet size
    betSize = 1
  } else {
    potHistory[iter] = potHistory[(iter-1)] - betSize
    
    # Triple down!!!
    betSize = betSize * 3
  }
}

plot.ts(potHistory, col="blue", lwd=3, main="Pot size over time", xlab="Bet number", ylab="Pot size")
abline(h=0, col="gray")

