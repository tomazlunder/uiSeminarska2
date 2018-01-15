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
#[4] je skrit? (1 TRUE, 2 FALSE)
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
  
  isHidden <- if (isPreyHidden(simData, preyId)) 1 else 2;
  
  c(distance, direction, isHungryOrThirstyAndCanConsume, isHidden)
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

getReward <- function(oldstate, action, newstate)
{
  reward <- 0
  
  #RAZDALJA DO NAJBLJIZJEGA PLENILCA [0,-29].
  reward <- (newstate[1]-30)
  
  #HRANJENJE IN PITJE [10/25].
  #Večja nagrada, če je lačen/žejen in na primernem tile-u
  if(action == 5){
    if(oldstate[3] == 1){
      reward <- reward + 25;
    } else {
      reward <- reward + 10;
    }
  }
  
  #PREDATOR JE BLIZU, AGENT SE SKRIJE [50].
  if(oldstate[1] < 5 && oldstate[4] == 2 && newstate[4] == 1){
    reward <- reward + 50;
  }
  
  #PREDATOR JE BLIZU, AGENT SE MU PRIBLJIŽA [-100].
  if (oldstate[1] < 5){
    if(newstate[1] < oldstate[1]){
      reward <- reward - 100;
    }
  }
  
  #PREDATOR JE BLIZU, AGENT SE ODDALJI [30]
  if (oldstate[1] < 5){
    if(newstate[1] > oldstate[1]){
      reward <- reward + 30;
    }
  }
  
  reward
}

