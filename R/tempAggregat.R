#' @title tempAggregat
#' @details This function aggregates, matching to destination dates, tiff layers and sums them to a single layer.
#' dependent on package "raster" & "rgdal"
#' @param beginzeitsp, integer, year of start 
#' @param endzeitsp, integer, year of end
#' @param dates_path, filepath to sorted and listet data with date info
#' @param aggrdata, filepath to stored data which shall be temporally aggregated
#' @param edit, character, shortnote for data [MAX 3 LETTERS]
#' @param outfilepath, path to store temporal aggregated tif files
#'
#' @return geo-tiff data
#'
#' @export tempAggregat
#' @author A. Santowski & L. Giese
#' #'@examples
#' \dontrun{
#' tempAggregat=function(beginnzeitsp,endzeitsp,dates_path,aggrdata,outfilepath,edit="kap")
#' }
tempAggregat=function(beginnzeitsp,endzeitsp,dates_path,aggrdata,outfilepath,edit="kap"){
  
  dates = substr(basename(list.files(path = dates_path, full.names = T)), 10, 16)
  
  for(j in seq(beginnzeitsp,endzeitsp)){
    print(j)
    
    lc = substr(dates[substr(dates, 1, 4) == j], 5, 7)
    l = as.numeric(lc)
    
    foreach(i=seq(1:(length(l)-1)),
            .export=c(beginnzeitsp, endzeitsp, dates_path, aggrdata, outfilepath, edit),
            .packages = c("raster", "rgdal")) %dopar%{
              print(i)
              if ((i+1 > (length(l)-1))){
                stacklist=raster::stack(aggrdata[which(j == seq(beginnzeitsp,endzeitsp))])
                temp_agg = sum(stacklist[[(l[length(l)]+1):nlayers(stacklist)]])
                raster::writeRaster(temp_agg, paste0(outfilepath, "MSWEP_",edit,"_", j, lc[i+1], "_temporal_aggregated"),format="GTiff",overwrite=T)
              } else {
                stacklist=raster::stack(aggrdata[which(j == seq(beginnzeitsp,endzeitsp))])
                temp_agg=sum(stacklist[[l[i]:(l[i+1]-1)]])
                raster::writeRaster(temp_agg, paste0(outfilepath, "MSWEP_",edit,"_", j, lc[i], "_temporal_aggregated"),format="GTiff",overwrite=T)
                
              }
            }
  }
  
}

