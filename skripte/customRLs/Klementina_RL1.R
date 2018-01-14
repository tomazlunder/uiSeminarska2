source("..//simulation.R")
source ("..//utils.R")

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
	# Border (index 8)
	# HungerLevel (index 9)
	vec <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
	
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
	
	vec[8] <- border
	vec[9] <- preyId
	vec[10] <- simData
	
	vec
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

#
# V tem primeru (nepopolnem) je nagrada definirana kot razdalja do najblizjega plenilca.
# Kaznuje se tudi akcija, ki premakne agenta v smeri najblizjega plenilca. 
#

getReward <- function(oldstate, action, newstate)
{
  preyId <- newstate[9]
  simData <- newstate[10]

  # PREMIK PROTI PREDATORJU  > 0: dlje ; < 0: bližje
  predator_reward = newstate[1] - oldstate[1]
  reward <- reward + predator_reward
  
  # ODDALJENOST OD PREDATORJA
  predator_dist <- newstate[1] - 30
  reward <- reward + predator_dist
  
  # AGENT NI VIDEN
  if(isPreyHidden(simData, preyId))
    reward <- reward + 5
  
  # TEREN
  if(isPreyOnDirt(simData, preyId)) # AGENT NA ZEMLJI
    reward <- reward + 1
  else if(isPreyOnGrass(simData, preyId)) # AGENT NA TRAVI
    reward <- reward - 1
  else if(isPreyInForest(simData, preyId)) # AGENT V GOZDU
    reward <- reward - 2
  else if(isPreyInWater(simData, preyId)) # AGENT V VODI
    reward <- reward - 3
  
  # AGENT NI NA MEJI
  if(newstate[8] == 5)
    reward <- reward + 1
  else if(action < 5) # AGENT NA MEJI IN NE JE/NE PIJE
  {
    if(action == newsate[8])  # Zaleti se v rob - je npr. na severu in spet želi proti severu
      reward <- reward - 1
    else
      reward <- reward + 3
  }
  
  # HRANA / PIJAČA
  if (action == 5) 
  {
    if(!isPreyHungry(simData, preyId)) # ... čeprav agent ni lačen
      reward <- reward - 3
    else  # Agent je lačen
      reward <- reward + 5
  }
  else
  {
    if(isPreyThirsty(simData, preyId) && newstate[2] - oldstate[2] < 0) # AGENT ŽEJEN IN BLIŽJE VODI
      reward <- reward + 3
    else if(isPreyThirsty(simData, preyId) && newstate[2] - oldstate[2] > 0)  # AGENT ŽEJEN IN DLJE OD VODE
      reward <- reward - 3
    
    if(isPreyHungryt(simData, preyId) && newstate[4] - oldstate[4] < 0) # AGENT LAČEN IN BLIŽJE TRAVE
      reward <- reward + 3
    else if(isPreyHungry(simData, preyId) && newstate[4] - oldstate[4] > 0) # AGENT LAČEN IN DLJE OD TRAVE
      reward <- reward - 3
  }
  
  #if(isPreyInForest(simData, preyId))
  #{
  #  if(action == 1 && )
  #}
  
	reward	
}

