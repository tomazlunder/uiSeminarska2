source("skripte//simulation.R")
source ("skripte//utils.R")

# Argument dimStateSpace je vektor iste dolzine kot opisi stanj, ki jih vraca funkcija getStateDesc. 
# Vsak element vektorja dimStateSpace doloca najvecjo vrednost, ki jo lahko zavzame istolezni element v opisu stanja.


# Mnozica stanj mora biti koncna in diskretna, zato morate upostevati naslednje omejitve:
# - vsa stanja morajo biti opisana z vektorjem enake dolzine
# - vsak element vektorja opisa mora biti pozitivno celo stevilo
#
# Zaradi hitrosti in zanesljivosti ucenja je zazeleno, da je razlicnih stanj cim manj!

#
# V tem primeru (nepopolnem) je v stanje zakodirana:
# - razdalja do najblizjega plenilca (navzgor omejena na 30), 
# - smer v kateri se nahaja ta plenilec in 
# - ali se agent nahaja na robu mape (vrednosti 1-4 ozna?ujejo ustrezne robove, vrednost 5 pa, da agent ni na robu)
#

getStateDesc <- function(simData, preyId)
{
	pos <- getPreyPos(simData, 1)
	
	getPreyInfo(simData,1)

	# ELEMENTI V VEKTORJU
	# 1. Plenilec (index 0)
	# 2. Voda (index 2)
	# 3. Trava (index 4)
	# 4. Gozd (index 6)
	vec <- c(0, 0, 0, 0, 0, 0, 0, 0)
	
	# PLENILEC
	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)
	direction <- res[2]
	vec[0] <- distance
	vec[1] <- direction
	
	# VODA
	res <- getPreyDistAndDirectionToNearestWater(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)
	direction <- res[2]
	vec[2] <- distance
	vec[3] <- direction
	
	# TRAVA
	res <- getPreyDistAndDirectionToNearestGrass(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)
	direction <- res[2]
	vec[4] <- distance
	vec[5] <- direction
	
	# GOZD
	res <- getPreyDistAndDirectionToNearestForest(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)
	direction <- res[2]
	vec[6] <- distance
	vec[7] <- direction
	
	vec

	if (pos[2] == 1)
		border <- 1
	else if (pos[2] == MAPHEIGHT)
		border <- 2
	else if (pos[1] == MAPWIDTH)
		border <- 3
	else if (pos[1] == 1)
		border <- 4
	else
		border <- 5

	c(distance, direction, border)
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

#
# V tem primeru (nepopolnem) je nagrada definirana kot razdalja do najblizjega plenilca.
# Kaznuje se tudi akcija, ki premakne agenta v smeri najblizjega plenilca. 
#

getReward <- function(oldstate, action, newstate)
{
  predator_reward =  oldstate[1] - newstate[1] # Negativno --> agent je v newstate 
  reward <- reward + predator_reward
  
  if (action == 5)  # Jej / pij
    reward <- reward + 2
  
  
	reward <- (newstate[1]-30)

	if (oldstate[3] == action)
		reward <- reward - 10

	reward	
}

