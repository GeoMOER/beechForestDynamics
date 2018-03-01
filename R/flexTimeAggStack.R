#' @title flexTimeAggStack
#' @details This function aggregates, matching to destination dates, tiff layers and sums them to a single layer.
#' dependent on package "raster" & "rgdal"
#' @param beginzeitsp, integer, year of start
#' @param endzeitsp, integer, year of end
#' @param dates, sorted date information
#' @param aggrdata, filepath to stored data which shall be temporally aggregated
#' @param edit, character, shortnote for data [MAX 3 LETTERS]
#' @param outfilepath, path to store temporal aggregated tif files
#' @param a integer, first pos of annual date
#' @param b integer, last pos of annual date
#' @param c integer, first pos of daily date
#' @param d integer, last pos of daily date
#'
#' @return geo-tiff data
#'
#' @export flexTimeAggStack
#' @author A. Santowski & L. Giese
#' #'@examples
#' \dontrun{
#' flexTimeAggStack=function(beginnzeitsp,endzeitsp,dates,aggrdata,outfilepath,edit="kap")
#' }
flexTimeAggStack=function(beginnzeitsp,endzeitsp,dates,aggrdata,outfilepath,edit="kap",a=1,b=4,c=5,d=7){

  for(j in seq(beginnzeitsp,endzeitsp)){
    print(j)

    lc = substr(dates[substr(dates, a, b) == j], c, d)
    l = as.numeric(lc)
    stacklist=raster::stack(aggrdata[which(j == seq(beginnzeitsp,endzeitsp))])

    foreach(i=seq(1:(length(l))),
            .export=c("stacklist", "outfilepath", "edit"),
            .packages = c("raster", "rgdal")) %dopar%{
              print(i)
              if ((i+1 > (length(l)))){
                temp_agg = sum(stacklist[[l[i]:nlayers(stacklist)]])
              } else {
                temp_agg=sum(stacklist[[l[i]:(l[i+1]-1)]])
              }
              raster::writeRaster(temp_agg,
                                  paste0(outfilepath, "MSWEP_",edit,"_", j, lc[i], "_temporal_aggregated"),
                                  format="GTiff",overwrite=T)

            }
  }

}

