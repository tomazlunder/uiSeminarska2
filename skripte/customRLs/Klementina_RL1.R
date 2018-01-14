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
	vec <- c(
	  0, 0, 0, 0, 
	  0, 0, 0, 0, 
	  0, 0, 0, 0,
	  0, 0)
	
	# PLENILEC
	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 10)
	direction <- res[2]
	vec[1] <- distance
	vec[2] <- direction
	
	# VODA
	res <- getPreyDistAndDirectionToNearestWater(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 10)
	direction <- res[2]
	vec[3] <- distance
	vec[4] <- direction
	
	# TRAVA
	res <- getPreyDistAndDirectionToNearestGrass(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 10)
	direction <- res[2]
	vec[5] <- distance
	vec[6] <- direction
	
	# GOZD
	res <- getPreyDistAndDirectionToNearestForest(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 10)
	direction <- res[2]
	vec[7] <- distance
	vec[8] <- direction
	
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
	
	vec[9] <- border
	vec[10] <- preyId
	
	if(isPreyOnDirt(simData, preyId))
	  vec[11] <- 0
	else if(isPreyOnGrass(simData, preyId))
	  vec[11] <- 1
	else if(isPreyInForest(simData, preyId))
	  vec[11] <- 2
	else if(isPreyInWater(simData, preyId))
	  vec[11] <- 3
	
	vec[12] <- if(isPreyHidden(simData, preyId)) 0 else 1
	vec[13] <- if(isPreyThirsty(simData, preyId)) 0 else 1
	vec[14] <- if(isPreyHungry(simData, preyId)) 0 else 1
	
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
  preyId <- newstate[10]

  # PREMIK PROTI PREDATORJU  > 0: dlje ; < 0: bližje
  predator_reward = newstate[2] - oldstate[2]
  reward <- reward + predator_reward
  
  # ODDALJENOST OD PREDATORJA
  predator_dist <- newstate[2] - 10
  reward <- reward + predator_dist
  
  # AGENT NI VIDEN
  if(newstate[12] == 0)
    reward <- reward + 5
  
  # TEREN
  teren <- newstate[11]
  if(teren == 0) # AGENT NA ZEMLJI
    reward <- reward + 1
  else if(teren == 1) # AGENT NA TRAVI
    reward <- reward - 1
  else if(teren == 2) # AGENT V GOZDU
    reward <- reward - 2
  else # AGENT V VODI
    reward <- reward - 3
  
  # AGENT NI NA MEJI
  if(newstate[9] == 5)
    reward <- reward + 1
  else if(action < 5) # AGENT NA MEJI IN NE JE/NE PIJE
  {
    if(action == newsate[9])  # Zaleti se v rob - je npr. na severu in spet želi proti severu
      reward <- reward - 1
    else
      reward <- reward + 3
  }
  
  # HRANA / PIJAČA
  isThirsty = newstate[13] == 0
  isHungry = newstate[14] == 0
  
  if (action == 5) 
  {
    if(!isHungry && !isThirsty) # ... čeprav agent ni lačen / žejen
      reward <- reward - 3
    else  # Agent je lačen / žejen
      reward <- reward + 5
  }
  else
  {
    if(isThirsty && newstate[3] - oldstate[3] < 0 || isHungry && newstate[5] - oldstate[5] < 0) # AGENT ŽEJEN / LAČEN IN BLIŽJE VODI / TRAVI
      reward <- reward + 3
    else if(isThirsty && newstate[3] - oldstate[3] > 0 || isHungry && newstate[5] - oldstate[5] > 0)  # AGENT ŽEJEN / LAČEN IN DLJE OD VODE / TRAVE
      reward <- reward - 3
  }
  
	reward	
}

