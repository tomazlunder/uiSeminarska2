#
# Lahko eksperimentirate z razlicnimi nastavitvami simuliranega okolja
#

# MAPWIDTH <- 50
# MAPHEIGHT <- 50
# NUMPREYS <- 3
# NUMPREDATORS <- 3


# za podan (nepopoln) primer v datoteki "RL.R"
# predstavlja vektor c(30, 4, 5) najvecje vrednosti istoleznih elementov
# v opisu stanja

#0. DEFAULT 
#source("tomaz//RL.R")
#qmat <- qlearning(c(30, 4, 5), maxtrials=1000)
#Result after 10 simulations [1P:1P] : 304.5

#1. RL1 
source("tomaz//RL1.R")
qmat <- qlearning(c(30, 4, 5, 2, 2), maxtrials=1000)
#Result after 10 simulations [1P:1P] : 224.57


# save(qmat, file="qmat.RData")
# load(file="qmat.RData")

#simulation(Q=qmat)


#RUNNING SIMULATIONS WITH Q MATRIX
numRuns <- 100
avg <- 0

for(i in c(1:numRuns)){
  print(i);
  avg <- avg + simulation(Q=qmat);
}
avg <- avg/numRuns;
print(avg)