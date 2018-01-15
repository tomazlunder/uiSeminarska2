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
#[3] je lačin in na travi ALI je žejen in na vodi? (1 TRUE, 2 FALSE)
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
  
  isHungryAndCanEat <- (isPreyHungry(simData, preyId) && isPreyOnGrass(simData, preyId))
  isThirstyAndCanDrink <- (isPreyThirsty(simData, preyId) && isPreyInWater(simData, preyId))
  isHungryOrThirstyAndCanConsume <- if (isHungryAndCanEat || isThirstyAndCanDrink) 1 else 2
  
  
  
  c(distance, direction, isHungryOrThirstyAndCanConsume)
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

getReward <- function(oldstate, action, newstate)
{
  #RAZDALJA DO NAJBLJIZJEGA PLENILCA. Kazen: (0,-29)
  reward <- (newstate[1]-30)
  
  #PREMIK V SMERI NAJBLJIZJEGLA PLENILCA. Kazen: (0, -29)
  #Kaznuje se relativno na razdaljo do plenilca
  if (oldstate[2] == action)
    reward <- reward - (30 - oldstate[3]);
  
  #AKCIJA, KI ODDALJI AGENTA OD PLENILCA JE NAGRAJENA. Nagrada: (10)
  if (oldstate[1] < newstate[1])
    reward <- reward + 10;
  
  safeConsumeDistance <- 8;
  
  #Plenilec dlje od safeConsumeDistance, na travi/vodi , lačen/žejen in in je/pije. Nagrada: (50)
  if (oldstate[1] > safeConsumeDistance && oldstate[3] == 1 && action == 5)
    reward <- reward + 50;
  
  #Ko je plenilec zelo blizu agenta (dangerZone) se akcije, ki zmanjšajo razdaljo med njima zelo kaznujejo (100)
  dangerZone <- 5;
  if (oldstate[1] < dangerZone){
    if(newstate[1] < oldstate[1]){
      reward <- reward - 100;
    }
  }
  
  
  reward
}

