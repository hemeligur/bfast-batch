#!/usr/bin/Rscript

# library(compiler)
# invisible(setCompilerOptions(optimize=3, suppressAll=TRUE, suppressUndefined=TRUE))
# invisible(enableJIT(3))
library(parallel)
# gets the arguments
# Must be in the following order: timeChange, timeUnits, maskStr, dataRasterStr

nonna_mask = function(timeChange = 1, timeUnits = 365, 
	maskStr = "../data/Pastagem/past_raster1_01_wgs84_near_bhrv.tif", 
	dataRasterStr = "../data/pa_br_mod13q1_ndvi_250_2000_2016.tif", shape_proc_method=0, printHelp=FALSE){

	#############################_Arguments_prep_####################################
	print("_Arguments_prep_")
		if(printHelp == TRUE){
			printHelp(cmdName="nonna_mask", args=c("timeChange", "timeUnits", "maskStr", "dataRasterStr", "shape_proc_method", "-h;--help"),
				defaultArgs=c("1","365", "maskStr.tif", "dataRasterStr.tif", "0"),
				description=c("O tempo em que ocorre mudança.", "A quantidade de dias da unidade de tempo, ex: mês=30, ano=365",
					"Endereço da máscara de pontos a processar", "Endereço do arquivo com as séries temporais", "Método de processamento de polígonos",
					"Mostra essa mensagem de ajuda."), type="function",
				);
			return(NA);
		}else{
				timeChange = as.numeric(timeChange)
				timeUnits = as.numeric(timeUnits)
				maskStr = as.character(maskStr)
				dataRasterStr = as.character(dataRasterStr)
		}

		if(!endsWith(x = dataRasterStr, suffix = ".tif")){
			stop("Erro: Argumento séries temporais. Formato de arquivo não suportado. Por favor utilize um GeoTiff.")
		}else{
			dataRaster = raster(dataRasterStr);
		}
	##############################_Preprocessing_input_##############################
	print("_Preprocessing_input_")
		# Inicializa a variável com o valor padrão original
		dataRasterTmpStr = dataRasterStr
		if(!endsWith(maskStr, ".tif")){
		#########################_SHP_############################################
			if(endsWith(maskStr, ".shp")){
				shape_mask = shapefile(maskStr)

			###########_Choosing shapefile process method_##########################
				if(shape_proc_method == 0){
					shape_class = class(shape_mask)[1]
				########################_Polygons_###########################
					if(shape_class == 'SpatialPolygons' || shape_class == 'SpatialPolygonsDataFrame'){
						if(!is.na(Sys.getenv("DISPLAY", unset = NA)) && require(tcltk)){

							shape_proc_method = tk_select.list(
								choices = c("1-Mais próximo do Centróide (Default)", "2-Pixel melhor representante", "3-Média"),
								preselect = 1, multiple = FALSE,
								title = paste0("Você entrou com um arquivo Shapefile.\n",
									"Por favor, escolha o método de processamento dos polígonos:\n"))

							if(shape_proc_method == "") shape_proc_method = 1
						}else{
							shape_proc_method = readNumericParam(inputMsg=paste0(
								"Você entrou com um arquivo Shapefile.\n",
								"Por favor, escolha o método de processamento dos polígonos ",
								"digitando o número correspondente:\n",
								"\n\t1-Mais próximo do Centróide (Default)\n\t2-Pixel melhor representante\n\t3-Média\n> "),
							startBound=1, endBound=2, outOfBoundsMsg="Valor inválido!", tries=tries,
							stopMsg=paste0("Muitas tentativas inválidas. ",
								"Continuando processamento com o valor padrão."),
							stopValue=1)
						}
				########################_Points_############################
					}else if(shape_class == 'SpatialPointsDataFrame' || shape_class == 'SpatialPoints'){
						shape_proc_method = 4
					}
				}
			###########_Reprojecting shapefile_#####################################
				print("Reprojecting shapefile...")
				shape_mask = spTransform(shape_mask, crs(dataRaster))
			###########_Extracting cell values and Zone Mask calculation_###########
				cellsNzone <- cellExtractionNZoneMask_parallel(dataRaster, shape_mask, shape_proc_method)
				cells = cellsNzone['pol_cells']['cells']
				centroids = cellsNzone['pol_cells']['centroid']
				zone_mask = cellsNzone['zone_mask']
			###########_Creating mask and data temp files_##########################
				# Cria uma máscara e brick temporários para o processamento
				maskRast = raster(dataRaster)
				dataRasterTmp = NA
				switch(shape_proc_method,
					'1' = , '4' = {
						maskRast[cells] = 1
					},
					'2' =, '3' = {
						most_representative = function(vals, cells){
							m = mean(vals)
							i = which.min(abs(m-vals))
							cell = cells[i]
							v = vals[i]

							return(c('index' = i, 'value' = v, 'cell' = cell, 'mean' = m))
						}
						result = zonal_parallel(dataRaster, zone_mask, most_representative)
						
						maskRast = crop(maskRast, extent(zone_mask)+6)
						croppedCentroids = cellFromXY(maskRast, xyFromCell(dataRaster, centroids))
						maskRast[croppedCentroids] = 1

						nl = length(result[[1]]$values)
						dataRasterTmp = brick(nrow=nrow(maskRast), ncol=ncol(maskRast), nl=nl)
						idx = ifelse(shape_proc_method==2, 'value', 'mean')
						for (i in 1:length(result)) {
							dataRasterTmp[croppedCentroids[i]] = as.numeric(result[[i]]$values[idx,])
						}
					})
			########################### Saving temp files to disk ##################

				maskStr = paste0(strtrim(maskStr, nchar(maskStr)-3), "tif")
				maskRast = writeRaster(x = maskRast, filename = maskStr, datatype = 'INT4S',
					NAflag = -3000, format = 'GTiff', options = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
					overwrite=TRUE)

				dataRasterStr.split = strsplit(dataRasterStr, "/")
				dataRasterTmpStr = paste0("../tmp/", dataRasterStr.split[[1]][length(dataRasterStr.split[[1]])])
				dataRasterTmpStr = paste0(strtrim(dataRasterTmpStr, nchar(dataRasterTmpStr)-3), "tmp")
				dataRasterTmp = writeRaster(x = dataRasterTmp, filename = dataRasterTmpStr, datatype = 'FLT4S',
					NAflag = -3000, format = 'GTiff', options = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES", "INTERLEAVE=PIXEL"),
					overwrite=TRUE)
		#########################_CSV_############################################
			}else if(endsWith(maskStr, ".csv")){
				maskRast = raster(dataRaster)

				coords = read.csv(file = maskStr)
				lonNames = c("lon", "longitude", "x")
				latNames = c("lat", "latitude", "y")
				coordsNames = tolower(names(coords))
				lonPos = which(!is.na(match(coordsNames, lonNames)))
				latPos = which(!is.na(match(coordsNames, latNames)))

				cells = cellFromXY(maskRast, coords[c(lonPos, latPos)])
				maskRast[cells] = 1;

				maskStr = paste0(strtrim(maskStr, nchar(maskStr)-3), "tif")
				maskRast = writeRaster(x = maskRast, filename = maskStr, datatype = 'INT4S',
					NAflag = -3000, format = 'GTiff', options = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
					overwrite=TRUE)
			}else{
				stop("File format not suported. Use .shp, .csv or.tif")
			}
		}
	##############################_nonna_startRow_and_startCol_######################
	print("_nonna_startRow_and_startCol_")
		mask = raster(maskStr);

		beginCoord = xyFromCell(mask, 1);
		startCol = colFromX(dataRaster, beginCoord[1]);
		startRow = rowFromY(dataRaster, beginCoord[2]);

		nonnaFile = paste0(maskStr, "_nonna.txt")
		# print(paste("nonnaFile:", nonnaFile))
		if(file.exists(nonnaFile)){
			# message("nonnaFile exists. Will read it.")
			nonna <- scan(nonnaFile, sep=',', quiet=TRUE);
		}else{
			# message("nonnaFile doesn't exists. Will create it.")
			nonna = Which(!is.na(mask), cells=TRUE)
			write(nonna, file=nonnaFile, sep=',')
		}

		nonna_sz = length(nonna)
		rm("nonna")
	##############################_h_and_dates_######################################
	print("_h_and_dates_")
		suppressWarnings(ts_info <- GDALinfo(dataRasterStr));
		bands = ts_info['bands']

		period = 16
		h = (timeChange*timeUnits)/period/bands;
	
		dates = generateModisDates(bands)
	#############################_output_############################################
	print("_output_")

		nonna_output = list(nonna_sz=nonna_sz, h=h, startRow=startRow, startCol=startCol, dates=dates,
			dataRasterTmpStr=dataRasterTmpStr)
		return(nonna_output)
		# cat(nonna_sz, h, startRow, startCol, 
		# 	paste0("c(", paste0("'", paste(dates, collapse="','"), "'"), ")"), "\n");
}