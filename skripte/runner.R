setwd("E://Projects//R Studio//uiSeminarska2");
source("skripte//RL.R");
#source("skripte//customRLs//tomaz_RL1.R");


run <- function(){
  qmat <- qlearning(c(30, 4, 5), maxtrials=1000)
  simulation(Q=qmat)
} 

#
# 
#

numruns <- 10
sum <- 0

for(i in 1:numruns){
  print(i)
  sum <- sum + run()
}

print(sum/numruns)

