source("tomaz//simulation.R")
source ("tomaz//utils.R")

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

	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)

	direction <- res[2]

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

	#[1] razdalja do najbljižjega plenilca,
	#[2] smer najbližjega plenilca,
	#[3] border
	#[4] je lačin in na travi? (1 TRUE, 2 FALSE)
	#[5] je žejen in na vodi? (1 TRUE, 2 FALSE)
	
	isHungryAndCanEat <- if(isPreyHungry(simData, preyId) && isPreyOnGrass(simData, preyId) 1 else 2;
	isThirstyAndCanDrink <- if (isPreyThirsty(simData, preyId) && isPreyOnWater(simData, preyId)) 1 else 2;
	
	

	c(distance, direction, border, isHungryAndCanEat, isThirstyAndCanDrink)
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

#
# V tem primeru (nepopolnem) je nagrada definirana kot razdalja do najblizjega plenilca.
# Kaznuje se tudi akcija, ki premakne agenta v smeri najblizjega plenilca. 
#

getReward <- function(oldstate, action, newstate)
{
  #RAZDALJA DO NAJBLJIZJEGA PLENILCA. Kazen: (0,-29)
	reward <- (newstate[1]-30)

	#PREMIK V SMERI NAJBLJIZJEGLA PLENILCA. Kazen: (0, -29)
	#Kaznuje se relativno na razdaljo do plenilca
	if (oldstate[2] == action)
		reward <- reward - (30 - oldstate[3]);
	
  #AKCIJA, KI ODDALJI AGENTA OD PLENILCA JE NAGRAJENA. Nagrada: (5)
	if (oldstate[1] < newstate[1])
	  reward <- reward + 5;
	
	safeConsumeDistance <- 8;
	
	#Plenilec dlje od safeConsumeDistance, na travi, lačein in je. Nagrada: (5)
	if (oldstate[1] > safeConsumeDistance && oldstate[4] == 1 && action == 5)
	  reward <- reward + 5;
	
	#Plenilec dlje od safeConsumeDistance, v vodi, žejen in pije. Nagrada: (5)
	if (oldstate[1] > safeConsumeDistance && oldstate[5] == 1&& action == 5)
	  reward <- reward + 5;
	
	reward	
}

