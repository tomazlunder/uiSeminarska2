library(grid)

initConsts <- function()
{
	assign("GRIDWIDTH", 5, envir = .GlobalEnv)
	assign("GRIDHEIGHT", 5, envir = .GlobalEnv)

	assign("MAPWIDTH", 50, envir = .GlobalEnv)
	assign("MAPHEIGHT", 50, envir = .GlobalEnv)

	assign("WATER_DIRT", -0.3, envir = .GlobalEnv)
	assign("DIRT_GRASS", 0.0, envir = .GlobalEnv)
	assign("GRASS_FOREST", 0.3, envir = .GlobalEnv)

	assign("WATERTILE", rgb(0.0,0.7,1.0), envir = .GlobalEnv)
	assign("DIRTTILE", rgb(0.70,0.42,0.15), envir = .GlobalEnv)
	assign("GRASSTILE", rgb(0.13,0.75,0.13), envir = .GlobalEnv)
	assign("FORESTTILE", rgb(0.13,0.55,0.13), envir = .GlobalEnv)

	assign("PREY_ALIVE", rgb(1.0,1.0,1.0), envir = .GlobalEnv)
	assign("PREY_MEAT", rgb(1.0,0.0,0.0), envir = .GlobalEnv)

	assign("PREDATOR_ALIVE", rgb(0.0,0.0,0.0), envir = .GlobalEnv)
	assign("PREDATOR_CARCASS", rgb(0.7,0.7,0.7), envir = .GlobalEnv)

	assign("NUMPREYS", 1, envir = .GlobalEnv)
	assign("NUMPREDATORS", 1, envir = .GlobalEnv)

	assign("PREDATOR_DIRT_SPEED", 0.5, envir = .GlobalEnv)
	assign("PREDATOR_GRASS_SPEED", 0.4, envir = .GlobalEnv)
	assign("PREDATOR_FOREST_SPEED", 0.3, envir = .GlobalEnv)
	assign("PREDATOR_WATER_SPEED", 0.2, envir = .GlobalEnv)

	assign("PREY_DIRT_SPEED", 1.0, envir = .GlobalEnv)
	assign("PREY_GRASS_SPEED", 0.9, envir = .GlobalEnv)
	assign("PREY_FOREST_SPEED", 0.7, envir = .GlobalEnv)
	assign("PREY_WATER_SPEED", 0.3, envir = .GlobalEnv)

	assign("PREY_FATAL_HUNGER", 8*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREY_EATING_GAIN", 4*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREY_HUNGER_THRESHOLD", 1*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	
	assign("PREY_FATAL_THIRST", 8*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREY_DRINKING_GAIN", 4*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREY_THIRST_THRESHOLD", 4*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	
	assign("PREY_CARCASS_PERIOD", 10, envir = .GlobalEnv)

	assign("PREDATOR_FATAL_HUNGER", 15*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREDATOR_EATING_GAIN", 5*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREDATOR_HUNGER_THRESHOLD", MAPWIDTH + MAPHEIGHT, envir = .GlobalEnv)
	
	assign("PREDATOR_FATAL_THIRST", 8*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREDATOR_DRINKING_GAIN", 4*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	assign("PREDATOR_THIRST_THRESHOLD", 4*(MAPWIDTH + MAPHEIGHT), envir = .GlobalEnv)
	
	assign("PREDATOR_CARCASS_PERIOD", 10, envir = .GlobalEnv)
	assign("PREDATOR_WP_CHANGE_PERIOD", 10, envir = .GlobalEnv)

	assign("ODOR_PREY", 2, envir = .GlobalEnv)
	assign("ODOR_PREDATOR", 2, envir = .GlobalEnv)

	assign("NORTH", 1, envir = .GlobalEnv)
	assign("SOUTH", 2, envir = .GlobalEnv)
	assign("EAST", 3, envir = .GlobalEnv)
	assign("WEST", 4, envir = .GlobalEnv)
	assign("CONSUME", 5, envir = .GlobalEnv)

	assign("ALIVE", 1, envir = .GlobalEnv)
	assign("MEAT", 2, envir = .GlobalEnv)
	assign("CARCASS", 3, envir = .GlobalEnv)
	assign("DELETED", 4, envir = .GlobalEnv)

	assign("PREY", 1, envir = .GlobalEnv)
	assign("PREDATOR", 2, envir = .GlobalEnv)
	assign("WATER", 3, envir = .GlobalEnv)
	assign("GRASS", 4, envir = .GlobalEnv)
	assign("FOREST", 5, envir = .GlobalEnv)
}

initConsts()

perlin_noise <- function(n=5, m=7, N=100, M=100) 
{
	vector_field <- apply(array(rnorm(2*n*m), dim=c(2,n,m) ), 2:3, function(u){u/sqrt(sum(u^2))})
	f <- function(x,y) 
	{
		i <- floor(x)
		j <- floor(y)
		stopifnot(i>=1 || j>=1 || i<n || j<m)
		v1 <- vector_field[,i,j]
		v2 <- vector_field[,i+1,j]
		v3 <- vector_field[,i,j+1]
		v4 <- vector_field[,i+1,j+1]
		u1 <- c(x,y)-c(i,j)
		u2 <- c(x,y)-c(i+1,j)
		u3 <- c(x,y)-c(i,j+1)
		u4 <- c(x,y)-c(i+1,j+1)
		a1 <- sum(v1*u1)
		a2 <- sum(v2*u2)
		a3 <- sum(v3*u3)
		a4 <- sum(v4*u4)
		s <- function(p){3*p^2-2*p^3}
		p <- s(x-i)
		q <- s(y-j)
		b1 <- (1-p)*a1+p*a2
		b2 <- (1-p)*a3+p*a4
		(1-q)*b1+q*b2
  	}
	xs <- seq(from=1, to=n, length=N+1)[-(N+1)]
	ys <- seq(from=1, to=m, length=M+1)[-(M+1)]
	outer(ys, xs, Vectorize(f))
}

prepareMap <- function()
{
	img <- perlin_noise(GRIDWIDTH, GRIDHEIGHT, MAPWIDTH, MAPHEIGHT)

	sel1 <- img < WATER_DIRT
	sel2 <- img >= WATER_DIRT & img < DIRT_GRASS
	sel3 <- img >= DIRT_GRASS & img < GRASS_FOREST
	sel4 <- img >= GRASS_FOREST

	img[sel1] <- WATERTILE
	img[sel2] <- DIRTTILE
	img[sel3] <- GRASSTILE
	img[sel4] <- FORESTTILE
 
	img
}

calcNewPos <- function(isPredator, action, Map, curPos)
{
	rows <- nrow(Map)
	cols <- ncol(Map)


	delta <- c(0,0)

	if (action == EAST)
		delta <- c(1,0)
	else if (action == WEST)
		delta <- c(-1,0)
	else if (action == NORTH)
		delta <- c(0,-1)
	else if (action == SOUTH)
		delta <- c(0,1)

	threshold <- 1
	val <- Map[curPos[2], curPos[1]]
	if (val == DIRTTILE)
		threshold <- ifelse(isPredator, PREDATOR_DIRT_SPEED, PREY_DIRT_SPEED)
	else if (val == GRASSTILE)
		threshold <- ifelse(isPredator, PREDATOR_GRASS_SPEED, PREY_GRASS_SPEED)
	else if (val == FORESTTILE)
		threshold <- ifelse(isPredator, PREDATOR_FOREST_SPEED, PREY_FOREST_SPEED)
	else if (val == WATERTILE)
		threshold <- ifelse(isPredator, PREDATOR_WATER_SPEED, PREY_WATER_SPEED)
	
	if (runif(1) > threshold)
		delta <- c(0,0)

	newPos <- curPos + delta

	if (newPos[1] >= 1 && newPos[1] <= cols && newPos[2] >= 1 && newPos[2] <= rows && Map[newPos[2], newPos[1]] %in% c(DIRTTILE,GRASSTILE,FORESTTILE,WATERTILE))
		newPos
	else
		curPos
}

randomPos <- function(Map)
{
	pos <- c(sample(ncol(Map),1),sample(nrow(Map),1))

	pos
}

prepareSimData <- function()
{
	data <- list()
 
	data$map <- prepareMap()
	data$watertiles <- which(data$map == WATERTILE, arr.ind=T)
	data$foresttiles <- which(data$map == FORESTTILE, arr.ind=T)
	data$timestamp <- 0

	data$preys <- list()

	for (i in 1:NUMPREYS)
	{
		data$preys[[i]] <- list()
		pos <- randomPos(data$map)
		data$preys[[i]]$pos <- pos
		data$preys[[i]]$tile <- data$map[pos[2],pos[1]]
		data$preys[[i]]$status <- ALIVE
		data$preys[[i]]$ateTS <- 0
		data$preys[[i]]$drankTS <- 0
		data$preys[[i]]$carcassTS <- 0
		data$preys[[i]]$action <- CONSUME
	}


	data$predators <- list()

	for (i in 1:NUMPREDATORS)
	{
		data$predators[[i]] <- list()
		pos <- randomPos(data$map)
		data$predators[[i]]$pos <- pos
		data$predators[[i]]$tile <- data$map[pos[2],pos[1]]
		data$predators[[i]]$status <- ALIVE
		data$predators[[i]]$ateTS <- 0
		data$predators[[i]]$drankTS <- 0
		data$predators[[i]]$carcassTS <- 0
		data$predators[[i]]$wp <- pos
		data$predators[[i]]$wpTS <- 0
		data$predators[[i]]$action <- CONSUME
	}

	data$running <- T
	data
}

selectPreyAction <- function(simData, preyId, Q=NULL)
{
	if (is.null(Q))
	{
		action <- sample(c(EAST, WEST, NORTH, SOUTH, CONSUME), 1)
	}
	else
	{
		state <- getStateDesc(simData, preyId)
	
		len <- length(state)
		if (len > 1)
		{
			d <- dim(Q)
			n <- dim2sub(state, d[1:len])
		}
		else
		{
			n <- state[1]
		}

		vals <- apply(Q, len+1, '[', n)

		m <- max(vals)
		candidates <- which(vals==m)
		if (length(candidates) == 1)
			action <- candidates
		else
       		action <- sample(candidates,1)
	}

	action
}

updatePreys <- function(simData, Q=NULL)
{
	for (i in 1:NUMPREYS)
	{
		if (simData$preys[[i]]$status == ALIVE)
		{
			oldpos <- simData$preys[[i]]$pos
			simData$map[oldpos[2],oldpos[1]] <- simData$preys[[i]]$tile

			preyAction <- selectPreyAction(simData, i, Q)
			simData$preys[[i]]$action <- preyAction

			newpos <- calcNewPos(FALSE, preyAction, simData$map, simData$preys[[i]]$pos)
	
			if (preyAction == CONSUME)
			{
				if (simData$map[newpos[2],newpos[1]] == GRASSTILE)
				{
					simData$preys[[i]]$ateTS <- min(simData$preys[[i]]$ateTS + PREY_EATING_GAIN, simData$timestamp)
					simData$map[newpos[2],newpos[1]] <- DIRTTILE
				}
				else if (simData$map[newpos[2],newpos[1]] == WATERTILE)
					simData$preys[[i]]$drankTS <- min(simData$preys[[i]]$drankTS + PREY_DRINKING_GAIN, simData$timestamp)
			}

			simData$preys[[i]]$pos <- newpos
			simData$preys[[i]]$tile <- simData$map[newpos[2],newpos[1]]
	
			if (simData$timestamp - simData$preys[[i]]$ateTS > PREY_FATAL_HUNGER || simData$timestamp - simData$preys[[i]]$drankTS > PREY_FATAL_THIRST)
			{
				simData$preys[[i]]$status <- MEAT
				simData$preys[[i]]$carcassTS <- simData$timestamp
			}
		}
		else if (simData$timestamp - simData$preys[[i]]$carcassTS > PREY_CARCASS_PERIOD)
			simData$preys[[i]]$status <- DELETED

		pos <- simData$preys[[i]]$pos
		if (simData$preys[[i]]$status == ALIVE)
			simData$map[pos[2],pos[1]] <- PREY_ALIVE
		else if (simData$preys[[i]]$status == MEAT)
			simData$map[pos[2],pos[1]] <- PREY_MEAT
		else
			simData$map[pos[2],pos[1]] <- simData$preys[[i]]$tile			
	}

	simData
}

selectPredatorAction <- function(simData, predatorId)
{
	ret <- list()
	ret$action = CONSUME


	minDist <- Inf
	pos <- simData$predators[[predatorId]]$pos
	
	hungerLevel <- simData$timestamp - simData$predators[[predatorId]]$ateTS
	if (hungerLevel < PREDATOR_HUNGER_THRESHOLD)
		hungerLevel <- 0
	else
		hungerLevel <- hungerLevel / PREDATOR_FATAL_HUNGER

	thirstLevel <- simData$timestamp - simData$predators[[predatorId]]$drankTS
	if (thirstLevel < PREDATOR_THIRST_THRESHOLD)
		thirstLevel <- 0
	else
		thirstLevel <- thirstLevel / PREDATOR_FATAL_THIRST


	if (hungerLevel >= thirstLevel)
		target <- PREY
	else
		target <- WATER 

	for (DIR in c(EAST, WEST, NORTH, SOUTH))
	{
		d <- getDistAndPosToNearestTile(simData, target, pos, DIR)
		if (is.finite(d[1]) && d[1] < minDist)
		{
			minDist <- d[1]
			ret$action <- DIR
			if (target == PREY)
				ret$wp <- d[2:3]
		}
	}

	if (minDist == 0 && target == WATER || minDist < 2 && target == PREY)
		ret$action <- CONSUME

	if (is.infinite(minDist))
	{
		wp <- simData$predators[[predatorId]]$wp

		dx <- wp[1] - pos[1]
		dy <- wp[2] - pos[2]

		if (abs(dx) > abs(dy))
		{
			if (dx > 0)
				ret$action <- EAST
			else
				ret$action <- WEST
		}
		else
		{
			if (dy > 0)
				ret$action <- SOUTH
			else
				ret$action <- NORTH
		}
	}

	ret
}

updatePredators <- function(simData)
{
	for (i in 1:NUMPREDATORS)
	{
		if (simData$predators[[i]]$status == ALIVE)
		{
			oldpos <- simData$predators[[i]]$pos
			simData$map[oldpos[2],oldpos[1]] <- simData$predators[[i]]$tile

			res <- selectPredatorAction(simData, i)
			predatorAction <- res$action
			simData$predators[[i]]$action <- predatorAction
			if (!is.null(res$wp) && simData$timestamp - simData$predators[[i]]$wpTS > PREDATOR_WP_CHANGE_PERIOD)
			{
				simData$predators[[i]]$wp <- res$wp
				simData$predators[[i]]$wpTS <- simData$timestamp
			}
			newpos <- calcNewPos(TRUE, predatorAction, simData$map, simData$predators[[i]]$pos)

			for (j in 1:NUMPREYS)
			{
				preypos <- simData$preys[[j]]$pos

				if (abs(preypos[1] - newpos[1]) + abs(preypos[2] - newpos[2]) <= 1)
				{
					if (simData$preys[[j]]$status == ALIVE)
					{
						simData$preys[[j]]$status <- MEAT
						simData$preys[[j]]$carcassTS <- simData$timestamp
						simData$map[preypos[2],preypos[1]] <- PREY_MEAT
						break
					}
					else if (predatorAction == CONSUME && simData$preys[[j]]$status == MEAT)
					{
						simData$predators[[i]]$ateTS <- min(simData$predators[[i]]$ateTS + PREDATOR_EATING_GAIN, simData$timestamp)
						break
					}
				}
			}

			if (predatorAction == CONSUME && simData$map[newpos[2],newpos[1]] == WATERTILE)
				simData$predators[[i]]$drankTS <- min(simData$predators[[i]]$drankTS + PREDATOR_DRINKING_GAIN, simData$timestamp)
				
			simData$predators[[i]]$pos <- newpos
			simData$predators[[i]]$tile <- simData$map[newpos[2],newpos[1]]
			
			wp <- simData$predators[[i]]$wp
			if (abs(wp[1] - newpos[1]) < 2 && abs(wp[2] - newpos[2]) < 2 && simData$timestamp - simData$predators[[i]]$wpTS > PREDATOR_WP_CHANGE_PERIOD)
			{
				simData$predators[[i]]$wp <- randomPos(simData$map)
				simData$predators[[i]]$wpTS <- simData$timestamp
			}

			if (simData$timestamp - simData$predators[[i]]$ateTS > PREDATOR_FATAL_HUNGER || simData$timestamp - simData$predators[[i]]$drankTS > PREDATOR_FATAL_THIRST)
			{
				simData$predators[[i]]$status <- CARCASS
				simData$predators[[i]]$carcassTS <- simData$timestamp
			}
		}
		else if (simData$timestamp - simData$predators[[i]]$carcassTS > PREDATOR_CARCASS_PERIOD)
			simData$predators[[i]]$status <- DELETED

		pos <- simData$predators[[i]]$pos
		if (simData$predators[[i]]$status == ALIVE)
			simData$map[pos[2],pos[1]] <- PREDATOR_ALIVE
		else if (simData$predators[[i]]$status == CARCASS)
			simData$map[pos[2],pos[1]] <- PREDATOR_CARCASS
		else
			simData$map[pos[2],pos[1]] <- simData$predators[[i]]$tile
	}

	simData
}

isSimulationAlive <- function(simData, maxSteps)
{
	if (simData$timestamp > maxSteps)
		return (FALSE)

	val <- FALSE
	for (i in 1:NUMPREYS)
	{
		if (simData$preys[[i]]$status == ALIVE)
		{
			val <- TRUE
			break
		}
	}
	
	if (!val)
		return (FALSE)

	val <- FALSE
	for (i in 1:NUMPREDATORS)
	{
		if (simData$predators[[i]]$status == ALIVE)
		{
			val <- TRUE
			break
		}
	}

	val
}

simulationStep <- function(simData, Q=NULL)
{
	simData$timestamp <- simData$timestamp + 1
	simData$grasstiles <- which(simData$map == GRASSTILE, arr.ind=T)

	simData <- updatePreys(simData, Q)
	simData <- updatePredators(simData)
	simData
}

textPreyInfo <- function(simData)
{
	text <- paste("timestamp:",simData$timestamp)
	grid.text(text, x=unit(5, "native"), y=unit(10, "native"), just="left", gp=gpar(fontsize=12, col="blue"))

	preyInfo <- simData$preys[[1]]

	text <- paste("prey hunger:", simData$timestamp - preyInfo$ateTS," / ",PREY_FATAL_HUNGER)
	grid.text(text, x=unit(5, "native"), y=unit(30, "native"), just="left", gp=gpar(fontsize=12, col="blue"))

	text <- paste("prey thirst:",simData$timestamp - preyInfo$drankTS," / ",PREY_FATAL_THIRST)
	grid.text(text, x=unit(5, "native"), y=unit(50, "native"), just="left", gp=gpar(fontsize=12, col="blue"))

	text <- paste("prey action:",switch(preyInfo$action, NORTH="NORTH", SOUTH="SOUTH", EAST="EAST", WEST="WEST", CONSUME="CONSUME"))
	grid.text(text, x=unit(5, "native"), y=unit(70, "native"), just="left", gp=gpar(fontsize=12, col="blue"))

	predatorInfo <- simData$predators[[1]]

	text <- paste("predator hunger:", simData$timestamp - predatorInfo$ateTS," / ",PREDATOR_FATAL_HUNGER)
	grid.text(text, x=unit(5, "native"), y=unit(90, "native"), just="left", gp=gpar(fontsize=12, col="blue"))

	text <- paste("predator thirst:",simData$timestamp - predatorInfo$drankTS," / ",PREDATOR_FATAL_THIRST)
	grid.text(text, x=unit(5, "native"), y=unit(110, "native"), just="left", gp=gpar(fontsize=12, col="blue"))

	text <- paste("predator action:",switch(predatorInfo$action, NORTH="NORTH", SOUTH="SOUTH", EAST="EAST", WEST="WEST", CONSUME="CONSUME"))
	grid.text(text, x=unit(5, "native"), y=unit(130, "native"), just="left", gp=gpar(fontsize=12, col="blue"))
}

simulationDraw <- function(simData, text=F)
{
	grid.newpage()
	grid.raster(simData$map, interpolate=F)

	if (text)
		textPreyInfo(simData)
}

simulation <- function(Q=NULL, maxSteps=1000, text=F)
{
	simData <- prepareSimData()
	
	while (simData$running)
	{		
		simData <- simulationStep(simData, Q)
		simulationDraw(simData, text)
 
		if (!isSimulationAlive(simData, maxSteps))
			break	
	}

	simData$timestamp
}

getNumberOfTiles <- function(Map, TILE, pos, DIR, dist)
{
	cmat <- col(Map) - pos[2]
	rmat <- row(Map) - pos[1]
	
	acmat <- abs(cmat)
	armat <- abs(rmat)

	if (DIR == NORTH)
		mask <- armat <= dist & armat >= acmat & rmat <= 0
	else if (DIR == SOUTH)
		mask <- armat <= dist & armat >= acmat & rmat >= 0
	else if (DIR == WEST)
		mask <- acmat <= dist & acmat >= armat & cmat <= 0
	else if (DIR == EAST)
		mask <- acmat <= dist & acmat >= armat & cmat >= 0
	else 
		stop("Unknown DIR")

	tab <- table(Map[mask])
	p <- which(names(tab) == TILE)

	if (any(p))
		as.integer(tab[p])
	else
		return(0)
}
	
isVisible <- function(Map,P1,P2)
{
	dx <- P2[1]-P1[1]
	dy <- P2[2]-P1[2]

	if (dx == dy && dx == 0)
		return (TRUE)

	if (abs(dx) > abs(dy)) 
	{
		a <- dy/dx
		b <- P1[2]-a*P1[1]
		x <- P1[1]:P2[1]
		y <- round(a*x+b)
	} 
	else 
	{
		a <- dx/dy
		b <- P1[1]-a*P1[2]
		y <- P1[2]:P2[2]
		x <- round(a*y+b)
	}

	for (i in 1:length(x))
	{
		if (Map[y[i],x[i]] == FORESTTILE)
			return(FALSE)
	}

	TRUE
}


getDistAndPosToNearestTile <- function(simData, TILE, pos, DIR)
{
	if (TILE == WATER)
		mask <- simData$watertiles
	else if (TILE == GRASS)
		mask <- simData$grasstiles
	else if (TILE == FOREST)
		mask <- simData$foresttiles
	else if (TILE == PREY)
	{
		row <- vector()
		col <- vector()

		for (i in 1:NUMPREYS)
		{
			preypos <- simData$preys[[i]]$pos
			if (simData$preys[[i]]$status != DELETED && (isVisible(simData$map, pos, preypos) || (abs(preypos[1] - pos[1]) <= ODOR_PREY && abs(preypos[2] - pos[2]) <= ODOR_PREY)))
			{
				row <- c(row, simData$preys[[i]]$pos[2])
				col <- c(col, simData$preys[[i]]$pos[1])
			}
		}
		mask <- cbind(row, col)
	}
	else if (TILE == PREDATOR)
	{
		row <- vector()
		col <- vector()

		for (i in 1:NUMPREDATORS)
		{
			predatorpos <- simData$predators[[i]]$pos
			if (simData$predators[[i]]$status == ALIVE && 
				(isVisible(simData$map, pos, predatorpos) || (abs(predatorpos[1] - pos[1]) <= ODOR_PREDATOR && abs(predatorpos[2] - pos[2]) <= ODOR_PREDATOR)))
			{
				row <- c(row, simData$predators[[i]]$pos[2])
				col <- c(col, simData$predators[[i]]$pos[1])
			}
		}
		mask <- cbind(row, col)
	}
	else
		stop("Unknown TILE")

	if (nrow(mask) == 0)
	{
		return(c(Inf,pos))
	}

	if (DIR == NORTH)
		tiles <- mask[mask[,"row"] <= pos[2] & abs(mask[,"row"] - pos[2]) >= abs(mask[,"col"] - pos[1]),,drop=F]
	else if (DIR == SOUTH)
		tiles <- mask[mask[,"row"] >= pos[2] & abs(mask[,"row"] - pos[2]) >= abs(mask[,"col"] - pos[1]),,drop=F]
	else if (DIR == WEST)
		tiles <- mask[mask[,"col"] <= pos[1] & abs(mask[,"col"] - pos[1]) >= abs(mask[,"row"] - pos[2]),,drop=F]
	else if (DIR == EAST)
		tiles <- mask[mask[,"col"] >= pos[1] & abs(mask[,"col"] - pos[1]) >= abs(mask[,"row"] - pos[2]),,drop=F]
	else
		stop("Unknown DIR")

	if (nrow(tiles) == 0)
	{
		return(c(Inf,pos))
	}

	dst <- abs(tiles[,"col"] - pos[1]) + abs(tiles[,"row"] - pos[2])
	
	n <- which.min(dst)
	c(dst[n],tiles[n,])
}

getDistToNearestTile <- function(simData, TILE, pos)
{
	minDist <- Inf

	for (DIR in c(EAST, WEST, NORTH, SOUTH))
	{
		d <- getDistAndPosToNearestTile(simData, TILE, pos, DIR)
		if (is.finite(d[1]) && d[1] < minDist)
			minDist <- d[1]
	}

	minDist
}

getPosToNearestTile <- function(simData, TILE, pos)
{
	minDist <- Inf
	pos <- c(Inf, Inf)

	for (DIR in c(EAST, WEST, NORTH, SOUTH))
	{
		d <- getDistAndPosToNearestTile(simData, TILE, pos, DIR)
		if (is.finite(d[1]) && d[1] < minDist)
		{
			minDist <- d[1]
			pos[1] <- d[2]
			pos[2] <- d[3]
		}
	}

	pos
}

getDirToNearestTile <- function(simData, TILE, pos)
{
	minDist <- Inf
	res <- vector()

	for (DIR in c(EAST, WEST, NORTH, SOUTH))
	{
		d <- getDistAndPosToNearestTile(simData, TILE, pos, DIR)
		if (d[1] < minDist)
		{
			minDist <- d[1]
			res <- DIR
		}
		else if (d[1] == minDist)
			res <- c(res, DIR)
	}

	if (length(res) == 1)
		res[1]
	else
		sample(res, 1)
}

getDistAndDirToNearestTile <- function(simData, TILE, pos)
{
	minDist <- Inf
	res <- vector()

	for (DIR in c(EAST, WEST, NORTH, SOUTH))
	{
		d <- getDistAndPosToNearestTile(simData, TILE, pos, DIR)
		if (d[1] < minDist)
		{
			minDist <- d[1]
			res <- DIR
		}
		else if (d[1] == minDist)
			res <- c(res, DIR)
	}

	dir <- res[1]
	if (length(res) > 1)
		dir <- sample(res, 1)

	c(minDist, dir)
}


dim2sub <- function (iarr, dim) 
{
    iarr <- t(iarr)
    pdim <- c(1, cumprod(dim[-length(dim)]))
    iarr <- iarr - 1
    colSums(apply(iarr, 1, "*", pdim)) + 1
}

qlearning <- function(dimStateSpace, gamma = 0.99, maxtrials = 1000, maxsteps = 1000)
{
	dimQ <- c(dimStateSpace, 5)	# 5 actions (north, south, east, west, consume)
	Q <- array(0, dim=dimQ)

	alpha <- 1
	ntrials <- 0
 
	curState <- list()

	while (alpha > 0.1 && ntrials < maxtrials)
	{
		simData <- prepareSimData()
		simData <- simulationStep(simData)

		for (i in 1:NUMPREYS)
			curState[[i]] <- getStateDesc(simData, i)

		while (simData$running)
		{
			simData <- simulationStep(simData)
			
			for (i in 1:NUMPREYS)
			{
				if (simData$preys[[i]]$status == ALIVE)
				{
					nextState <- getStateDesc(simData, i)
					reward <- getReward(curState[[i]], simData$preys[[i]]$action, nextState)

					curStatePos <- dim2sub(c(curState[[i]], simData$preys[[i]]$action), dimQ)
					len <- length(nextState)
					
					if (len > 1)
						n <- dim2sub(nextState, dimStateSpace)
					else
						n <- nextState[1]
	
					vals <- apply(Q, len+1, '[', n)			

              			Q[curStatePos] <- Q[curStatePos] + alpha * (reward + gamma * max(vals, na.rm = T) - Q[curStatePos])
              			curState[[i]] <- nextState
				}
			}

			if (!isSimulationAlive(simData, maxsteps))
				break
		}

		ntrials <- ntrials + 1
		alpha <- alpha * 0.9999

		print(paste("trial",ntrials,"out of max",maxtrials), quote=F)
		flush.console()
	}

	Q
}

