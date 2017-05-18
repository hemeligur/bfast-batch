#!/usr/bin/Rscript

# nonna arguments
# Must be in the following order: timeChange, timeUnits, maskStr, dataRasterStr

# bfast_batch arguments
# Must be in the following order: h, season, startRow, startCol, dates, startIndex, endIndex,
# maskStr, dataRasterStr, outputType

# Unique arguments that need treatment:
# maskStr, dataRasterStr, timeChange, timeUnits, outputType

################################_Loading_libraries_####################################
	print("Loading libraries")
	suppressPackageStartupMessages(library(compiler))
	invisible(setCompilerOptions(optimize=3, suppressAll=TRUE, suppressUndefined=TRUE))
	invisible(enableJIT(3))

	suppressPackageStartupMessages(library(rgdal))
	suppressPackageStartupMessages(library(raster))
	# suppressPackageStartupMessages(library(bfast))
	# suppressPackageStartupMessages(library(doParallel))
	suppressPackageStartupMessages(library(parallel))
################################_Sourcing_code_########################################
	print("Sourcing code")
	source("../lib/utils.lib.r")
	source("nonna_mask.r")
	source("zonal_parallel.r")
	source("bfast_batch.r")
###########################_Creating_log_and_tmp_folders_##############################
	print("Creating log and tmp folders")
	if(!dir.exists("../logs")){
		dir.create("../logs")
	}
	if(!dir.exists("../tmp")){
		dir.create("../tmp")
	}
################################_Cleaning_log_files_###################################
	print("Cleaning log files")
	invisible(file.remove(Sys.glob("../logs/*.log")))
	invisible(file.remove(Sys.glob("../tmp/*.tmp")))
#########################_Reading_args_and_setting_defaults_###########################
	print("Reading args")
	args = getArgs(2, must=TRUE);

	# Setting default argument values
	maskStr = ""
	dataRasterStr = ""
	timeChange = 1
	timeUnits = 365
	outputType = "csv"

	# Setting constants
	bfast_num_cores = 3

	if(args[1] == -1){
		printHelp(cmdName="main.bfast_brasil.r",
			args=c("maskStr", "dataRasterStr", "timeChange", "timeUnits", "outputType", "-h;--help"),
			#"datesFile", "slopeFile",
			defaultArgs=c("","", "1", "365", "csv"),
			#"datesFile.tif", "slopeFile.tif"
			description=c("Endereço da máscara de pontos a processar",
				"Endereço do arquivo com as séries temporais",
				"O tempo em que ocorre mudança.", "A quantidade de dias da unidade de tempo, ex: mês=30, ano=365",
				"Tipo da saída desejada. Tipos disponíveis:
			\t\"csv\"\tGera uma saída em formato csv, contendo as datas, magnitude da tendência, valor da tendência antes e depois, e slope para cada breakpoint.
			\t\"tif-simple\"\tGera uma saída em formato tif, sendo duas imagens: slope e data do último breakpoint.
			\t\"tif-all\"\tGera uma saída em formato tif, sendo cinco imagens: slope, data, magnitude, antes e depois do último breakpoint. (Não ainda implementado)",
			#"Endereço do arquivo de datas", "Endereço do arquivo de slopes",
			"Mostra essa mensagem de ajuda."), executor="./",
			);
		quit();
	}else if(length(args) > 1){
			maskStr = if(!is.na(args[1])) as.character(args[1]) else maskStr
			dataRasterStr = if(!is.na(args[2])) as.character(args[2]) else dataRasterStr
			timeChange = if(!is.na(args[3])) as.character(args[3]) else timeChange
			timeUnits = if(!is.na(args[4])) as.character(args[4]) else timeUnits
			outputType = if(!is.na(args[5])) as.character(args[5]) else outputType
	}else if(length(args) == 0){
		printHelp(msg="\nSem argumentos. Usando padrões...\n");
	}

	if(maskStr == ""){
		maskStr = chooseFile(filter=c(".tif", ".shp", ".csv"), 
			caption="Escolha o arquivo com a máscara de pontos a processar", obrigatory=TRUE)
	}

	if(dataRasterStr == ""){
		dataRasterStr = chooseFile(filter=c(".tif"), 
			caption="Escolha o arquivo com as séries temporais", obrigatory=TRUE)
	}
#######################_nonna, startRow, startCol, h and dates_########################
	print("Calculating nonna, startRow, startCol, h and dates")
	nonna_result = nonna_mask(timeChange=timeChange, timeUnits=timeUnits, maskStr=maskStr, dataRasterStr=dataRasterStr)
	print(paste("Calculated h value:", nonna_result$h))
	dataRasterStr = nonna_result$dataRasterTmpStr
################################_Cluster_preparation_##################################
	print("Cluster preparation")
	cores = detectCores()
	bfast_cores = ifelse(cores>bfast_num_cores, bfast_num_cores, cores)
	numproc = trunc((cores/bfast_cores)*2)
	pts_per_proc = trunc(nonna_result$nonna_sz/numproc)

	cl = makeCluster(numproc)
	clusterExport(cl, c("bfast_batch", "nonna_result", "maskStr", "dataRasterStr", "outputType", 
		"pts_per_proc", "numproc", "bfast_cores"))

	invisible(clusterEvalQ(cl, {
		library(rgdal)
		library(raster)
		library(bfast)
		library(doParallel)

		registerDoParallel(cores = bfast_cores)
	}))

	if(outputType == "csv" && !file.exists("bfast_results.csv")){
		empty_df = data.frame(Row = numeric(), Column = numeric(), Longitude = numeric(), Latitude = numeric(), Date = numeric(), 
							Magnitude = numeric(), Trend_Before = numeric(), Trend_After = numeric(), Slope_Before = numeric(), 
							Slope_After = numeric())
		write.table(empty_df, file = "bfast_results.csv", row.names = FALSE, quote = FALSE, sep = ';')
	}
################################_Firing_parallel_nodes_################################
	print(paste("Firing", numproc, "parallel nodes, each with", pts_per_proc, "points. Totalizing", nonna_result$nonna_sz))
	res = parLapply(cl, 1:numproc, function(x){

		startIndex = 1 + (x-1)*pts_per_proc
		if(x == numproc){
			rest = nonna_result$nonna_sz %% numproc
			endIndex = x*pts_per_proc + rest
		}else{
			endIndex = x*pts_per_proc
		}

		tryCatch(
			bfast_batch(h=nonna_result$h, season=nonna_result$season, startRow=nonna_result$startRow, 
				startCol=nonna_result$startCol, dates=nonna_result$dates, startIndex=startIndex, endIndex=endIndex, 
				maskStr=maskStr, dataRasterStr=dataRasterStr, outputType=outputType),
			error=function(e){
				return(paste0("Error at ", x, " iteration. Indexes: ", startIndex, ", ", endIndex, ".\n Error: ", e))
			})
	})

	stopCluster(cl)
	print("done")