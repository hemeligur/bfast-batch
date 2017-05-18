zone_parallel = function(rastr, zone_mask, zone, fun, startRowCol=NA, cores=detectCores()-1, clfun=NA, export=TRUE, stop=TRUE){
	library(raster)
	# library(parallel)

	fun = match.fun(fun)

	if(any(is.na(startRowCol))){
		beginCoord = xyFromCell(zone_mask, 1)
		startRowCol = rowColFromCell(rastr, cellFromXY(rastr, beginCoord))-1
	}
	# if(is.na(clfun)){
	# 	clfun = makeCluster(cores, outfile="")
	# 	export = TRUE
	# }
	cells = Which(zone_mask == zone, cells=TRUE)
	rowCols = rowColFromCell(zone_mask, cells)
	
	# if(export){
	# 	clusterExport(clfun, c("fun", "rastr", "startRowCol", "rowCols"), envir = environment())
	# }
	vals <- lapply(X=1:nrow(rowCols), FUN=function(i){
		idx = rowCols[i,]+startRowCol
		data = rastr[idx]
		res = as.vector(data)
		return(res)
	})

	rm("rowCols")

	df.vals = as.data.frame(vals, col.names=1:length(vals))
	rm("vals")

	fun_vals <- lapply(X=1:nrow(df.vals), FUN=function(px, df.vals, cells){
		return(fun(as.numeric(df.vals[px,]), cells))
	}, df.vals=df.vals, cells=cells)

	rm(list=c("df.vals", "cells"))

	# if(stop){
	# 	stopCluster(clfun)
	# }

	zone_res = NULL
	zone_res['zone'] = zone
	zone_res['values'] = list(as.data.frame(fun_vals, col.names=1:length(fun_vals)))

	return(zone_res)
}

zonal_parallel = function(rastr, zone_mask, fun, cores=detectCores()-1){
	library(raster)
	library(parallel)

	# beginCoord = xyFromCell(zone_mask, 1)
	# rastrStartCell = cellFromXY(rastr, beginCoord)
	# startRowCol = rowColFromCell(rastr, rastrStartCell)
	startRowCol=NA

	cl = makeCluster(2*cores/3, outfile="")
	# clfun = makeCluster(cores/3, outfile="")
	clusterExport(cl, c("rastr", "zone_mask", "startRowCol", "fun", "zone_parallel"
		#, "clfun"
		), envir = environment())
	# clusterExport(clfun, c("fun", "rastr", "startRowCol"), envir = environment())

	zones = unique(zone_mask)

	tryCatch(
		res <- parLapplyLB(cl, 1:length(zones), function(zone){
			print(zone)
			return(zone_parallel(rastr, zone_mask, zone, fun, startRowCol, cores=cores/3,
				export=FALSE, stop=FALSE,
				clfun=NA))
		}),
	error = function(e){print(e);stopCluster(cl)})

	print('fim')

	stopCluster(cl)

	return(res)

}

# shp_sample = shapefile("../data/Pastagem/SIAD/SIAD_15_16_shape_sample.shp")
# br_ndvi = brick("../data/pa_br_mod13q1_ndvi_250_2000_2016.tif")
# br_ndvi_layer1 = raster("../data/pa_br_mod13q1_ndvi_250_2000_2016.tif")

# system.time(cells <- cellFromPolygon(br_ndvi_layer1, shp_sample, weights=F))
# zone_mask = raster(br_ndvi_layer1)

# cl = makeCluster(detectCores() -1)
# clusterExport(cl, c("zone_mask", "cells"))

# system.time(
# 	res <- parLapply(cl, 1:length(cells), function(pol){
# 		tryCatch(zone_mask[cells[[pol]]] <- pol, error=function(e){return(NA)})
# 	})
# )

# stopCluster(cl)

# # for(pol in 1:length(cells)){
# # 	for(cell in cells[[pol]]){
# # 		zone_mask[cell]=pol
# # 	}
# # }