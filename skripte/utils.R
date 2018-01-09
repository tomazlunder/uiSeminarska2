getPreyInfo <- function(simData, preyId)
{
	simData$preys[[preyId]]
}

getPreyPos <- function(simData, preyId)
{
	simData$preys[[preyId]]$pos
}

isPreyThirsty <- function(simData, preyId)
{
	simData$timestamp - simData$preys[[preyId]]$drankTS > PREY_THIRST_THRESHOLD
}

getPreyThirstLevel <- function(simData, preyId)
{
	simData$timestamp - simData$preys[[preyId]]$drankTS
}

getPreyFatalThirstLevel <- function(simData, preyId)
{
	(simData$timestamp - simData$preys[[preyId]]$drankTS)/PREY_FATAL_THIRST
}

isPreyHungry <- function(simData, preyId)
{
	simData$timestamp - simData$preys[[preyId]]$ateTS > PREY_HUNGER_THRESHOLD
}

getPreyHungerLevel <- function(simData, preyId)
{
	simData$timestamp - simData$preys[[preyId]]$ateTS
}

getPreyFatalHungerLevel <- function(simData, preyId)
{
	(simData$timestamp - simData$preys[[preyId]]$ateTS)/PREY_FATAL_HUNGER
}

isPreyOnGrass <- function(simData, preyId)
{
	simData$preys[[preyId]]$tile == GRASSTILE
}

getPreyDistToNearestGrass <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDistToNearestTile(simData, GRASS, pos)
}

getPreyDirectionToNearestGrass <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDirToNearestTile(simData, GRASS, pos)
}

getAmountOfGrassNorthOfPray <- function(simData, preyId, range=MAPHEIGHT)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, GRASSTILE, pos, NORTH, range)	
}

getAmountOfGrassSouthOfPray <- function(simData, preyId, range=MAPHEIGHT)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, GRASSTILE, pos, SOUTH, range)	
}

getAmountOfGrassEastOfPray <- function(simData, preyId, range=MAPWIDTH)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, GRASSTILE, pos, EAST, range)	
}

getAmountOfGrassWestOfPray <- function(simData, preyId, range=MAPWIDTH)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, GRASSTILE, pos, WEST, range)	
}

isPreyInForest <- function(simData, preyId)
{
	simData$preys[[preyId]]$tile == FORESTTILE
}

getPreyDistToNearestForest <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDistToNearestTile(simData, FOREST, pos)
}

getPreyDirectionToNearestForest <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDirToNearestTile(simData, FOREST, pos)
}

getAmountOfForestNorthOfPray <- function(simData, preyId, range=MAPHEIGHT)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, FORESTTILE, pos, NORTH, range)	
}

getAmountOfForestSouthOfPray <- function(simData, preyId, range=MAPHEIGHT)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, FORESTTILE, pos, SOUTH, range)	
}

getAmountOfForestEastOfPray <- function(simData, preyId, range=MAPWIDTH)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, FORESTTILE, pos, EAST, range)	
}

getAmountOfForestWestOfPray <- function(simData, preyId, range=MAPWIDTH)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, FORESTTILE, pos, WEST, range)	
}

isPreyInWater <- function(simData, preyId)
{
	simData$preys[[preyId]]$tile == WATERTILE
}

getPreyDistToNearestWater <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDistToNearestTile(simData, WATER, pos)
}

getPreyDirectionToNearestWater <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDirToNearestTile(simData, WATER, pos)
}

getAmountOfWaterNorthOfPray <- function(simData, preyId, range=MAPHEIGHT)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, WATERTILE, pos, NORTH, range)	
}

getAmountOfWaterSouthOfPray <- function(simData, preyId, range=MAPHEIGHT)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, WATERTILE, pos, SOUTH, range)	
}

getAmountOfWaterEastOfPray <- function(simData, preyId, range=MAPWIDTH)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, WATERTILE, pos, EAST, range)	
}

getAmountOfWaterWestOfPray <- function(simData, preyId, range=MAPWIDTH)
{
	pos <- getPreyPos(simData, preyId)

	getNumberOfTiles(simData$map, WATERTILE, pos, WEST, range)	
}

isPreyHidden <- function(simData, preyId)
{
	is.infinite(getPreyDistToNearestPredator(simData, preyId))
}

getPreyDistToNearestPredator <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDistToNearestTile(simData, PREDATOR, pos)
}

getPreyDirectionToNearestPredator <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDirToNearestTile(simData, PREDATOR, pos)
}

getPreyDistAndDirectionToNearestPredator <- function(simData, preyId)
{
	pos <- getPreyPos(simData, preyId)
	
	getDistAndDirToNearestTile(simData, PREDATOR, pos)
}

canPreyPerformAction <- function(simData, preyId, action)
{
	rows <- nrow(simData$map)
	cols <- ncol(simData$map)

	pos <- getPreyPos(simData, preyId)

	if (action == CONSUME)
		return(simData$preys[[preyId]]$tile %in% c(WATERTILE, GRASSTILE))

	if (action == EAST)
		pos <- pos + c(1,0)
	else if (action == WEST)
		pos <- pos + c(-1,0)
	else if (action == NORTH)
		pos <- pos + c(0,-1)
	else if (action == SOUTH)
		pos <- pos + c(0,1)
	else
		stop("Unknown action")

	pos[1] >= 1 && pos[1] <= cols && pos[2] >= 1 && pos[2] <= rows && simData$map[pos[2], pos[1]] %in% c(DIRTTILE,GRASSTILE,FORESTTILE,WATERTILE)	
}

getSmallMap <- function(simData, preyId, r)
{
	pos <- getPreyPos(simData, preyId)

	res <- matrix(NA, nrow=2*r+1, ncol=2*r+1)
	
	i1 <- pos[1]-r
	if (i1 > 0)
		x1 <- 1
	else
	{
		x1 <- 2-i1
		i1 <- 1
	}

	i2 <- pos[1]+r
	if (i2 <= ncol(simData$map))
		x2 <- 2*r+1
	else
	{
		x2 <- 2*r+1-i2+ncol(simData$map)
		i2 <- ncol(simData$map)
	}

	j1 <- pos[2]-r
	if (j1 > 0)
		y1 <- 1
	else
	{
		y1 <- 2-j1
		j1 <- 1
	}

	j2 <- pos[2]+r
	if (j2 <= nrow(simData$map))
		y2 <- 2*r+1
	else
	{
		y2 <- 2*r+1-j2+nrow(simData$map)
		j2 <- nrow(simData$map)
	}

	tmp <- res
	tmp[x1:x2,y1:y2] <- simData$map[i1:i2,j1:j2]
	
	sel <- tmp == WATERTILE 
	res[sel] <- 1
	
	sel <- tmp == DIRTTILE 
	res[sel] <- 2
	
	sel <- tmp == GRASSTILE 
	res[sel] <- 3
	
	sel <- tmp == FORESTTILE 
	res[sel] <- 4
	
	for (i in 1:NUMPREYS)
	{
		if (simData$preys[[i]]$status == ALIVE && simData$preys[[i]]$pos[1] >= i1 && simData$preys[[i]]$pos[1] <= i2 && simData$preys[[i]]$pos[2] >= j1 && simData$preys[[i]]$pos[2] <= j2)
			res[x1+simData$preys[[i]]$pos[1] - i1,y1 + simData$preys[[i]]$pos[2] - j1] <- 0 
	}
	
	for (i in 1:NUMPREDATORS)
	{
		if (simData$predators[[i]]$status == ALIVE && simData$predators[[i]]$pos[1] >= i1 && simData$predators[[i]]$pos[1] <= i2 && simData$predators[[i]]$pos[2] >= j1 && simData$predators[[i]]$pos[2] <= j2)
			res[x1+simData$predators[[i]]$pos[1] - i1,y1 + simData$predators[[i]]$pos[2] - j1] <- 5 
	}
	
	res
}

