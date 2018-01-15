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
#Result after 100 simulations [1P:1P] : 237.5

#1. RL1 
#source("tomaz//RL1.R")
#qmat <- qlearning(c(30, 4, 2, 2), maxtrials=1000)
#Result after 100 simulations [1P:1P] : 305.89 

#1. RL2
#source("tomaz//RL2.R")
#qmat <- qlearning(c(30, 4, 2), maxtrials=1000)
#Result after 1000 simulations [1P:1P] : 302.54

#1. RL3
source("tomaz//RL3.R")
qmat <- qlearning(c(30, 4, 2), maxtrials=1000)
#Result after 1000 simulations [1P:1P] : 


# save(qmat, file="qmat.RData")
# load(file="qmat.RData")

#simulation(Q=qmat)


#RUNNING SIMULATIONS WITH Q MATRIX
numRuns <- 1000
avg <- 0

for(i in c(1:numRuns)){
  result <- simulation(Q=qmat)
  cat("Simulacija[", i,"]: ", result, "\n")
  
  avg <- avg + simulation(Q=qmat)
}
avg <- avg/numRuns;
cat("Povprečje: ", avg, "\n")