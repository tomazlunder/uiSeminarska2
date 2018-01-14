setwd("C://Users//kleme//OneDrive//Dokumenti//uiSeminarska2//skripte");

source("customRLs/Klementina_RL1.R");

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

# Distance, direction, border
#qmat <- qlearning(c(30, 4, 5), maxtrials=1000)

qmat <- qlearning(c(10, 4, 10, 4, 10, 4, 10, 4, 5, 1, 3, 1, 1, 1))

# save(qmat, file="qmat.RData")
# load(file="qmat.RData")

simulation(Q=qmat)