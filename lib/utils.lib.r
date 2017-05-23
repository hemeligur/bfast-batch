# File to hold useful general R functions									 									 #
# If any doubts in how to use, fell free to ask the person by your side now. #
#																			 																			 #
# Created by Hemeligur, the will-bender!									 									 #
#----------------------------------------------------------------------------#
# Functions:																 																 #
#																			 																			 #
# 	* printProgress																												   #
#		* readArgs															 																 #
#   * getArgs                                                                #
#		* printHelp															 																 #
#		* chooseFile														 																 #
#			* file.choose.terminal																								 #
#		* NAposition														 																 #
#		* distNonNAtoNA														  														 #
#		* stepSize															 																 #
#		* generateModisDates										 																 #
#		* readNumericParam											 																 #
#   * line																																	 #
#   * cellFromPointOrPolygon																								 #
##############################################################################

utils.lib = TRUE

# prints the progress passed.
# If the package R.utils is available, uses a ProgressBar.
printProgress = function(done){
	if(class(done) == "numeric"){
		cat(sprintf("%d%%...\n", done))
		# if(suppressWarnings(require(R.utils, quietly=TRUE)) == TRUE){
		# 	bar = ProgressBar()
		# 	setValue(bar, done)
		# 	update(bar, visual=TRUE)
		# }else{
		# 	cat(sprintf("%d%%...\n", done))
		# }
	}
}

# reads the arguments passed through the command-line, evaluating them.
# 'numArgs' is the maximum number of arguments allowed for your script.
# 'must' is a logical value indicating if your script requires an argument to work properly, 
# i.e.: If no args were passed, the program will terminate.
readArgs = function(numArgs, must=FALSE){
	args = getArgs(numArgs, must)
	
  argsPassed = length(args)
    
	#evaluating parameters
	if(argsPassed > 0){
		if(grepl("--help", args[1]) || grepl("-h", args[1])){
			return(parse(text="help=TRUE"))
		}else{
			expr = parse(text=args)
			return(expr)
		}
	}else{
		return(NULL)
	}
}

# reads the arguments passed through the command-line, NOT evaluating them.
# 'numArgs' is the MINIMUM number of arguments for your script.
# 'must' is a logical value indicating if your script requires an argument to work properly, 
# i.e.: If no args were passed, the program will terminate.
getArgs = function(numArgs, must=FALSE){
    args = commandArgs(TRUE)
    argsPassed = length(args)
    
    if(args[1]=='-h' || args[1]=='--help'){
    	return(-1)
    }else if( (argsPassed < numArgs) || (argsPassed == 0 && isTRUE(must)) ){
        cat("Number of arguments is invalid.\n Abort!\n\nTry '--help'.\n")
        quit(status=2)
    }

    return(args)
}


# Prints a help message.
# If msg is passed, just prints it and ignores any other argument.
# Otherwise constructs the message based on the other arguments.
# If there is a --help;-h or any help argument it MUST be the last one.
printHelp = function(msg="", cmdName="command", args=c("arg1","arg2","arg3", "optArg1", "--help"),
	defaultArgs=c("1","2","3"), description=c("sets arg1","sets arg2","sets arg3", "sets optArg1", "show help"),
	type="command-line1", executor="./" #deprecated argument. Keeped for backwards compatibility.
	#, helpArg="-h;--help", helpMsg="Shows this help message."
	){
	if(class(msg) == "character" && nchar(msg) != 0){
		cat(msg);
	}else{
		if(type == "command-line1"){
			argsSignature = paste(args[-length(args)], defaultArgs, sep="='", collapse="' ");
			argsSignature = paste0(argsSignature, "'");
			executor = "./"
			spacing = " "
		}else if(type == "command-line2"){
			argsSignature = paste(defaultArgs, collapse=" ");
			executor = "./"
			spacing = " "
		}else if(type == "function"){
			argsSignature = paste(args[-length(args)], defaultArgs, sep="=", collapse=", ");
			argsSignature = paste0("(", argsSignature, ")");
			executor = ""
			spacing = ""
		}
		
		# if(nchar(helpArg) > 0 && nchar(helpMsg) > 0){
		# 	args = c(args, helpArg);
		# 	description = c(description, helpMsg);
		# }
		description = paste(args, description, sep=": ", collapse="\n\n\t");
		msg = sprintf("Usage:\n%s%s%s%s\n\n\t%s\n", executor, cmdName, spacing, argsSignature, description);
		cat(msg);
	}
}

# Shows a window for the user to choose a file.
# The filter must be in the form ".<extension>", e.g.: ".txt", and can be a vector, or list
# Multi as TRUE if more than one file can be chosen, FALSE otherwise.
# Obrigatory as TRUE if a file must be chosen for the program to continue, and should be terminated otherwise
chooseFile = function(filter="", caption="Choose a file", multi = FALSE, obrigatory=FALSE){
	if(require(tcltk)){
		file = file.choose.gui(filter=filter, caption=caption, multi=multi)
	}else{
		file = file.choose.terminal(filter=filter, caption=caption)
	}
	
	if(!file.exists(file)){
		if(obrigatory == TRUE){
  		#if no file was chosen then terminates execution
			stop("No file has been chosen or file does not exist. Can not continue!\nAbort!\n\n")
		}else{
			warning("No file has been chosen or file does not exist.")
		}
	}

	return(file)
}

file.choose.gui = function(filter="", caption="Choose a file", multi = FALSE){
	library(tcltk)

	filterLabels = gsub("[.]*", "", filter)
	filterLabels = toupper(filterLabels)
	filterLabels = paste(filterLabels, "Files")

	filters = matrix(c(filterLabels, filter), nrow=length(filter), ncol=2, byrow=FALSE)
	file = tk_choose.files(caption=caption, filters=filters, multi=multi)

	if(length(file) == 0) file = ""

	return(file)
}

# Ask the user to type a file name. No GUI.
# Returns the path of the file
file.choose.terminal = function(filter="", caption="Choose a file"){
	message(caption)
	file = ""
	chosen = FALSE
	while(!chosen){
		tryCatch({file <- file.choose()}, error=function(e){message("A file must be chosen. Type 'q' to cancel."); file <- ""})
		if(file == "<-" || file == ".."){
			setwd("..");
		}else if(file == "ls"){
			print(dir())
		}else if(file == "q"){
			message("Input cancelled by user.")
			file = ""
			chosen = TRUE
		}else if(startsWith(file, "-> ") || startsWith(file, "cd ")){
			path = substr(file, 4, nchar(file))
			try(setwd(path))
		}else if(!endsWith(file, filter)){
			message("Invalid file extension.")
		}
	}

	file = suppressWarnings(normalizePath(file))

	return(file)
}

# Finds the positions of the NAs in the data and returns a vector with them
NAposition = function(data){
	na_pos = attr(na.omit(data), "na")[]
	return(na_pos)
}

# Calculates the distances of each element of the data to the closest NA value
distNonNAtoNA = function(data){
	na_pos = NAposition(data)
	res = NULL
	for(i in 1:length(data)){
		dists = sort(abs(i - na_pos))
		res[i] = dists[1]
	}

	return(res)
}

# Calculates the difference of an element to the next in a numeric sequence.
stepSize = function(data){
	minuend = data[-1]
	subtrahend = data[-length(data)]

	stepSz = minuend - subtrahend

	return(stepSz)
}

# Generates a vector with the modis dates since 2000-02-18 with the number of observations since it.
generateModisDates = function(obsNum){
    #############################_dates_##############################################
    start_date = as.Date("2000-02-18");
    end_date = Sys.Date();

    startYear = as.numeric(format(start_date, "%Y"));
    endYear = as.numeric(format(end_date, "%Y"));

    endYears = (startYear+1):endYear
    endYears = as.Date(paste0(endYears, "-01-01"))
    startYears = c(start_date, endYears[-length(endYears)])

    dates = Map(seq, startYears, endYears, 16)
    dates = do.call("c", dates)

    endYearDt = as.Date(endYears[length(endYears)])
    dates = c(dates, seq(from=endYearDt, to=end_date, by=16))

    if(obsNum < length(dates)){
        dates = dates[1:obsNum]
    }

    return(dates)
}

readInput = function(inputMsg=""){
	if(interactive()){
		input = readline(inputMsg)
	}else{
		cat(inputMsg)
		input = readLines(n=1)
	}

	return(input)
}

# Asks the user for input and expects a numeric response
readNumericParam = function(inputMsg, startBound, endBound, outOfBoundsMsg, tries, stopMsg, stopValue){
    param_valid = FALSE
    count = 0
    param = 0

    while(!param_valid){
        param <- readInput(inputMsg)

        param = as.numeric(unlist(param))
        if(is.na(param) || param < startBound || param > endBound ){
            message(outOfBoundsMsg)
            param_valid = FALSE
            count=count+1
        }else{
            param_valid = TRUE
        }

        if(count >= tries){
            message(stopMsg)
            return(stopValue)
        }
    }

    return(param)
}

# Generates a straight line based on a start value, an end value and a desired length
line = function(start, end, length){
	# Just found out this function is basically seq with length.out, so...
	return(seq(from=start, to=end, length.out=length))

	# start = as.numeric(start)
	# end = as.numeric(end)
	# length = as.numeric(length)
	# if(is.na(start) || is.na(end) || is.na(length)){
	# 	stop("NaN argument")
	# }
	
	# length = ceiling(length)

	# if(length == 1 || start == end) stop("Invalid arguments")

	# # an = a1 + (n-1).r
	# r = (end - start)/(length-1)
	# l = seq(start, end, r)

	# return(l)
}

# Get the cell numbers of a raster correspondent to the polygon or point passed
# It is essentially a wrapper to cellFromPolygon and cellFromXY from the raster package,
# with the addition of centroid calculation
# r --> 		The raster of reference
# p --> 		The point or polygon
# type -->	The type of calculation: 1 for cell within polygon closest to the centroid,
#							2 or 3 for polygon and 
# 						4 for point (This is just to facilitate integration with other older modules)
cellFromPointOrPolygon = function(r, p, type){
	switch(type,
		'1' = {
			library(rgeos)
			library(raster)
			centr = gCentroid(p, byid = TRUE)
			centr_cell = cellFromXY(r, centr)
			pol_cells = cellFromPolygon(r, p, weights=F)[[1]]
			if(!(centr_cell %in% pol_cells)){
				r.crop = crop(r, p)
				
				pol_cells.crop = cellFromXY(r.crop, xyFromCell(r, pol_cells))
				centr_cell.crop = cellFromXY(r.crop, xyFromCell(r, centr_cell))
				
				r.crop[] = 0
				r.crop[pol_cells.crop] = 1
				r.crop[centr_cell.crop] = 2
				
				d = gridDistance(r.crop, origin=2, omit=0)
				d[centr_cell.crop] = maxValue(d)+1000
				cell = sample(list(Which(d == minValue(d), cells=TRUE)), 1)[[1]]
				
				cells = cellFromXY(r, xyFromCell(r.crop, cell))
				centroid = centr_cell
			}else{
				cells = centr_cell
				centroid = cell
			}
		},
		'2' = , '3' = {
			cells = cellFromPolygon(r, p, weights=F)[[1]]
			centroid = cellFromPointOrPolygon(r, p, 1)
		},
		'4' = {
			cell = cellFromXY(r, p)
			cells= cell
			centroid = NA
		},
		{
			cells = NA
			centroid = NA
	  	warning("Invalid type. Must be between 1 and 4.")
	  })

	pol_cells = NULL
	pol_cells['cells'] = cells
	pol_cells['centroid'] = centroid

  return(pol_cells)
}

cellExtractionNZoneMask_parallel = function(rastr, shape_mask, type, cores=detectCores()-1){
	library(parallel)
	#####################_Creating cluster_#################################
		cl = makeCluster(cores, outfile="")
	###########_Creating empty zone mask raster_############################
		if(type == 2 || type == 3){
			message("Creating zone mask...")
			zone_mask = raster(rastr)
			zone_mask = crop(extent(shape_mask)+5)
			setValues(zone_mask, 0)
			clusterExport(cl, c("zone_mask"), envir = environment())
		}
	###########_Preparing cluster for cell extraction_######################
		clusterExport(cl, c("cellFromPointOrPolygon"))
		clusterExport(cl, c("type"), envir = environment())
		clusterEvalQ(cl, library(raster))
	###########_Extracting cell values and Zone Mask calculation_###########
		cellsNzone <- parLapplyLB(cl, 1:length(shape_mask), function(pol){
			pol_cells = cellFromPointOrPolygon(rastr, shape_mask[pol,], type)
			if(type == 2 || type == 3){
				tryCatch(zone_mask[pol_cells[[1]]] <- pol, error=function(e){return(NA)})
			}

			res = NULL
			res['pol_cells'] = pol_cells
			if(type == 2 || type == 3){
				res['zone_mask'] = zone_mask
			}else{
				res['zone_mask'] = NA
			}

			print(paste("cellsNzone", pol_cells))

			return(res)
		})

		stopCluster(cl)
		return(cellsNzone)
}