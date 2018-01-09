source("RL.R")

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

qmat <- qlearning(c(30, 4, 5), maxtrials=1000)

# save(qmat, file="qmat.RData")
# load(file="qmat.RData")

simulation(Q=qmat)
