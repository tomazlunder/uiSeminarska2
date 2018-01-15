source("tomaz//simulation.R")
source ("tomaz//utils.R")

# Argument dimStateSpace je vektor iste dolzine kot opisi stanj, ki jih vraca funkcija getStateDesc. 
# Vsak element vektorja dimStateSpace doloca najvecjo vrednost, ki jo lahko zavzame istolezni element v opisu stanja.


# Mnozica stanj mora biti koncna in diskretna, zato morate upostevati naslednje omejitve:
# - vsa stanja morajo biti opisana z vektorjem enake dolzine
# - vsak element vektorja opisa mora biti pozitivno celo stevilo
#
# Zaradi hitrosti in zanesljivosti ucenja je zazeleno, da je razlicnih stanj cim manj!


#[1] razdalja do najbljižjega plenilca,
#[2] smer najbližjega plenilca,
#[3] je lačin in na travi? (1 TRUE, 2 FALSE)
#[4] je žejen in na vodi? (1 TRUE, 2 FALSE)
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

	isHungryAndCanEat <- if(isPreyHungry(simData, preyId) && isPreyOnGrass(simData, preyId)) 1 else 2;
	isThirstyAndCanDrink <- if (isPreyThirsty(simData, preyId) && isPreyInWater(simData, preyId)) 1 else 2;
	
	

	c(distance, direction, isHungryAndCanEat, isThirstyAndCanDrink)
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

getReward <- function(oldstate, action, newstate)
{
  #RAZDALJA DO NAJBLIŽJEGA PLENILCA [0, -29].
	reward <- (newstate[1]-30)

	#PREMIK V SMERI NAJBLJIZJEGLA PLENILCA (Kazen odvisna od razdalje) [0, -29].
	if (oldstate[2] == action)
		reward <- reward - (30 - oldstate[3]);
	
  #PREMIK, KI ODDALJI PLEN OD PLENILCA JE NAGRAJEN [5].
	if (oldstate[1] < newstate[1])
	  reward <- reward + 5;
	
	#AGENT, KI JE NA VARNI RAZDALJI, LAČEN, NA TRAVI IN JE, JE NAGRAJEN [5];
	safeConsumeDistance <- 8;
	if (oldstate[1] > safeConsumeDistance && oldstate[3] == 1 && action == 5)
	  reward <- reward + 5;
	
	#AGENT, KI JE NA VARNI RAZDALJI, ŽEJEN, V VODI IN PIJE, JE NAGRAJEN [5].
	if (oldstate[1] > safeConsumeDistance && oldstate[4] == 1 && action == 5)
	  reward <- reward + 5;
	
	reward	
}

