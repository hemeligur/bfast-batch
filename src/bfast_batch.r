#!/usr/bin/Rscript

source("../lib/utils.lib.r");

bfast_batch = function(h=0.06, season='harmonic', startRow, startCol, dates, startIndex=1, 
	endIndex, maskStr, dataRasterStr, outputType='csv', 
	datesFile = "../output/bfast_mod13q1_last_break_dates_250_2000_2016.tif",
	slopeFile = "../output/bfast_mod13q1_last_slopes_250_2000_2016.tif", blockSize = 5000, printHelp=FALSE){

	###########################_Arguments_prep_#####################################################
		if(printHelp == TRUE){
			printHelp(cmdName="bfast_brasil.r",
				args=c("h", "season", "startRow", "startCol", "dates", "startIndex", "endIndex",
				 "maskStr", "dataRasterStr", "outputType", "-h;--help"),
				#"datesFile", "slopeFile",
				defaultArgs=c("0.06","harmonic", "1", "1", "dates=c('2000-01-01')", "1", "15",
					"mask.tif", "data_time-series.tif", "csv"),
				#"datesFile.tif", "slopeFile.tif"
				description=c("h = Minimal segment size.",
				"O argumento season a ser passado para o BFAST. O padrão é 'harmonic'. Outras opções são 'dummy' e 'none'",
				"Linha inicial", "Coluna inicial", "Datas desde 2000-02-18 até o tamanho da série temporal.",
				"Índice inicial", "Índice final",
				"Endereço da máscara de pastagem", "Endereço do arquivo com as séries temporais",
				"Tipo da saída desejada. Tipos disponíveis:
				\t\"csv\"\tGera uma saída em formato csv, contendo as datas, magnitude da tendência, valor da tendência antes e depois, e slope para cada breakpoint.
				\t\"tif-simple\"\tGera uma saída em formato tif, sendo duas imagens: slope e data do último breakpoint.
				\t\"tif-all\"\tGera uma saída em formato tif, sendo cinco imagens: slope, data, magnitude, antes e depois do último breakpoint.",
				#"Endereço do arquivo de datas", "Endereço do arquivo de slopes",
				"Mostra essa mensagem de ajuda."), executor="./",
			);
			return();
		}else if(length(args) > 1){
				h = as.numeric(h)
				season = as.character(season)
				startRow = as.numeric(startRow)
				startCol = as.numeric(startCol)
				dates = as.Date(dates)
				startIndex = as.numeric(startIndex)
				endIndex = as.numeric(endIndex)
				maskStr = as.character(maskStr)
				dataRasterStr = as.character(dataRasterStr)
				outputType = as.character(outputType)
		}

		logFile = paste0("../logs/bfast_", startIndex, "-", endIndex, ".log")

		cat("startIndex:", startIndex, "\n", file=logFile, append = TRUE)
		cat("endIndex:", endIndex, "\n", file=logFile, append = TRUE)

		print(maskStr)
		pastureMask = raster(maskStr);

		nonna = scan(paste0(maskStr,"_nonna.txt"), sep=',', quiet=TRUE);
		nonna = nonna[startIndex:endIndex];

		rows = rowFromCell(pastureMask, nonna)-1;
		cols = colFromCell(pastureMask, nonna)-1;
		points = Map(c, rows, cols);
		cat("Points:", length(points), "\n", file=logFile, append = TRUE)
	###########################_Output_configuration_####################################
		cat("_Output_configuration_", outputType, "\n", file=logFile, append = TRUE)
		if(outputType == "tif-simple"){
			datesFile = paste(datesFile, startIndex, endIndex, sep="_")
			slopeFile = paste(slopeFile, startIndex, endIndex, sep="_")
			dateRasterOutput = raster(pastureMask);
			slopeRasterOutput = raster(pastureMask);
			dateRasterOutput[] = 0;
			slopeRasterOutput[] = 0;

			dateRasterOutput = writeRaster(dateRasterOutput, datesFile, datatype='INT4S', NAflag=-3000, format='GTiff',
				options=c('COMPRESS=NONE', 'TILED=YES', 'BIGTIFF=YES'), overwrite=TRUE)
			slopeRasterOutput = writeRaster(slopeRasterOutput, slopeFile, datatype='FLT4S', NAflag=-3000, format='GTiff',
				options=c('COMPRESS=NONE', 'TILED=YES', 'BIGTIFF=YES'), overwrite=TRUE)

			blockStart = 1;
			datesBlk = integer(blockSize);
			slopesBlk = integer(blockSize);
		}else if(outputType == "tif-all"){
			print("Warning: Option \"tif-all\" not fully implemented. Unexpected behavior to be expected ;)")
			
			dateRasterOutput = raster(ts_data_firstlayer);
			magnitudeRasterOutput = raster(ts_data_firstlayer);
			beforeRasterOutput = raster(ts_data_firstlayer);
			afterRasterOutput = raster(ts_data_firstlayer);
			dateRasterOutput[] = 0;
			magnitudeRasterOutput[] = 0;
			beforeRasterOutput[] = 0;
			afterRasterOutput[] = 0;

			dateRasterOutput = writeRaster(dateRasterOutput, "datas_breakpoints_bfast.tif",
			    datatype='INT4S', NAflag=-3000, format='GTiff',
			    options=c('COMPRESS=LZW', 'TILED=YES', 'BIGTIFF=YES'), overwrite=TRUE)
			magnitudeRasterOutput = writeRaster(magnitudeRasterOutput, "magnitude_breakpoints_bfast.tif",
			    datatype='FLT4S', NAflag=-3000, format='GTiff',
			    options=c('COMPRESS=LZW', 'TILED=YES', 'BIGTIFF=YES'), overwrite=TRUE)
			beforeRasterOutput = writeRaster(beforeRasterOutput, "antes_breakpoints_bfast.tif",
			    datatype='FLT4S', NAflag=-3000, format='GTiff',
			    options=c('COMPRESS=LZW', 'TILED=YES', 'BIGTIFF=YES'), overwrite=TRUE)
			afterRasterOutput = writeRaster(afterRasterOutput, "depois_breakpoints_bfast.tif",
			    datatype='FLT4S', NAflag=-3000, format='GTiff',
			    options=c('COMPRESS=LZW', 'TILED=YES', 'BIGTIFF=YES'), overwrite=TRUE)
		}else if(outputType == "csv"){
			rows = cols = lons = lats = numeric(blockSize)
			mags = trendsBfor = trendsAftr = slopeBfor = slopeAftr = numeric(blockSize)
			mags[] = trendsBfor[] = trendsAftr[] = slopeBfor[] = slopeAftr[] = -9999
			breakDates = character(blockSize)
			breaksCount = 0;
		}
	###########################_Processing_loop_####################################################
		cat("_Processing_loop_\n", file=logFile, append = TRUE)
		cellCount = 0;
		for (rowcol in points){
			tryCatch({
			###################### Running BFAST #######################################################
				cat("Running bfast\n", file=logFile, append = TRUE)
				row = rowcol[1];
				col = rowcol[2];

				lon = xFromCol(pastureMask, col)
				lat = yFromRow(pastureMask, row)

				capture.output(ts <- readGDAL(dataRasterStr, offset=c(row+startRow, col+startCol), region.dim=c(1,1)))
				Yt <- bfastts(unlist(ts@data), dates, type="16-day");
				Yt[is.na(Yt)] = 0;
				#Runs BFAST
				# time = system.time(
				cat("\n", Yt, "\n", file=logFile, append = TRUE)
				invisible(capture.output(fit <- suppressWarnings(bfast(Yt, h=h, season=season, max.iter=1, hpc='foreach'))))
				# )
				breaks = fit$output[[1]]$Vt.bp;
				trendCmp = fit$output[[1]]$Tt;
				cat("[", length(breaks), "] Breaks: ", breaks, "\n", file=logFile, append = TRUE)
				cat("trendCmp: [", length(trendCmp), "]\n", file=logFile, append = TRUE)
				cat("bfast runned\n", file=logFile, append = TRUE)
			#######################_Processing_output_##################################################
				if(outputType != "csv"){
					cat("output not csv.\n", file=logFile, append = TRUE)

					lastBreak = if(!fit$nobp$Vt) breaks[length(breaks)] else 1;
					lastBreakDate = dates[lastBreak];
					lastBreakJulianDate = format(lastBreakDate, "%Y%j")
					lastSegment = trendCmp[lastBreak:length(trendCmp)];

					#The slope is calculated using the point at one third of the segment's length and the one at 2 thirds;
					# |---------#*********#----------| Use the # points
					# This is made to avoid taking points at the edge that might be out of the line (y=ax+b) due to the confidence of the breakpoint
					slope = (lastSegment[2*floor(length(lastSegment)/3)] - lastSegment[floor(length(lastSegment)/3)])/(2*floor(length(lastSegment)/3) - floor(length(lastSegment)/3));

					if(((cellCount%%blockSize) == 0) && (cellCount/blockSize >= 1)){
						print(cellCount);
						dateRasterOutput = update(dateRasterOutput, v=datesBlk, cell=nonna[blockStart:(blockStart+blockSize-1)])
						slopeRasterOutput = update(slopeRasterOutput, v=slopesBlk, cell=nonna[blockStart:(blockStart+blockSize-1)])
						blockStart=cellCount+1;
					}

					datesBlk[(cellCount%%blockSize)+1] = as.numeric(lastBreakJulianDate);
					slopesBlk[(cellCount%%blockSize)+1] = slope #as.numeric(sprintf("%.10f", slope))
				}
				else{
					cat("outputType is CSV\n", file=logFile, append = TRUE)
					if(breaksCount >= blockSize){
						cat("breaksCount >= blockSize\n", file=logFile, append = TRUE)
						cat(breaksCount, "\n", file=logFile, append = TRUE)
						cat(cellCount, "\n", file=logFile, append = TRUE)
						result_df = data.frame(Row = rows, Column = cols, Longitude = lons, Latitude = lats, Date = breakDates, 
							Magnitude = mags, Trend_Before = trendsBfor, Trend_After = trendsAftr, Slope_Before = slopeBfor, 
							Slope_After = slopeAftr, stringsAsFactors = FALSE)

						write.table(x = result_df, file = "bfast_results.csv", row.names = FALSE, quote = FALSE, sep = ";", col.names = FALSE, append = TRUE)
						rm("result_df")
						breaksCount = 0;
					}

					for (i in seq_len(length(breaks))) {
						rows[i+breaksCount] = row
						cols[i+breaksCount] = col
						lons[i+breaksCount] = lon
						lats[i+breaksCount] = lat
						cat("row, col, lon, lat\n", file=logFile, append = TRUE)

						trendsBfor[i+breaksCount] = tryCatch(fit$Mags[i,1], error=function(e){return(NA)})
						trendsAftr[i+breaksCount] = tryCatch(fit$Mags[i,2], error=function(e){return(NA)})
						mags[i+breaksCount] = tryCatch(fit$Mags[i,3], error=function(e){return(NA)})
						cat("mags\n", file=logFile, append = TRUE)

						breakDates[i+breaksCount] = format(dates[breaks[i]], "%Y%j")[1]
						cat("dates\n", file=logFile, append = TRUE)

						bforObs = ifelse(i == 1, 1, breaks[i-1])
						aftrObs = ifelse(i == length(breaks), length(trendCmp), breaks[i+1])
						trendCmpBfor = trendCmp[bforObs:breaks[i]]
						trendCmpAftr = trendCmp[breaks[i]:aftrObs]
						cat("trendCmp\n", file=logFile, append = TRUE)

						slopeBfor[i+breaksCount] =
						((trendCmpBfor[2*floor(length(trendCmpBfor)/3)] - trendCmpBfor[floor(length(trendCmpBfor)/3)])/
						(2*floor(length(trendCmpBfor)/3) - floor(length(trendCmpBfor)/3)))[1]
						slopeAftr[i+breaksCount] =
						((trendCmpAftr[2*floor(length(trendCmpAftr)/3)] - trendCmpAftr[floor(length(trendCmpAftr)/3)])/
						(2*floor(length(trendCmpAftr)/3) - floor(length(trendCmpAftr)/3)))[1]
						cat("slope\n", file=logFile, append = TRUE)
					}
					breaksCount = breaksCount + length(breaks)
					cat("CSV lines writen. Going to next point.", "\n", file=logFile, append = TRUE)
				}

				cellCount=cellCount+1;
			},
			interrupt=function(x){
				print("Captured user interrupt. Terminating");
			});
		}
	###########################_Finish_writing_last_data_###########################################
		if(outputType != "csv"){
			print(cellCount)
			dateRasterOutput = update(dateRasterOutput, v=datesBlk[1:(cellCount%%blockSize)], cell=nonna[blockStart:length(nonna)])
			slopeRasterOutput = update(slopeRasterOutput, v=slopesBlk[1:(cellCount%%blockSize)], cell=nonna[blockStart:length(nonna)])

			print(dateRasterOutput)
			print(slopeRasterOutput)
		}else{
			print(breaksCount);
			print(cellCount);
			result_df = data.frame(Row = rows[1:breaksCount], Column = cols[1:breaksCount],
				Longitude = lons[1:breaksCount], Latitude = lats[1:breaksCount], Date = breakDates[1:breaksCount],
				Magnitude = mags[1:breaksCount], Trend_Before = trendsBfor[1:breaksCount],
				Trend_After = trendsAftr[1:breaksCount], Slope_Before = slopeBfor[1:breaksCount],
				Slope_After = slopeAftr[1:breaksCount], stringsAsFactors = FALSE)

			write.table(x = result_df, file = "bfast_results.csv", row.names = FALSE, quote = FALSE, sep = ";", col.names = FALSE, append = TRUE)
		}
}