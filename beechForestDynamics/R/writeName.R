#' Write name function
#' @title writeName
#' @aliases writeName
#' @author Santowski, A. & C. Weber
#' @description Creates new folder structure and file-names for output files constructed in other functions.
#' 
#' @param indir List of input files 
#' @param newdir output directory
#' @param prefix Set a new prefix to file-name (default = NA) - prefix or suffix must be set
#' @param suffix Set a new suffix to filename (default = NA) - prefix or suffix must be set

writeName=function(indir,newdir,prefix=NA,suffix=NA){
  newname=list()
  if (is.na(prefix)){
    lapply(indir,function(i){
      tmp=base::regexpr("\\.[^\\.]*$", i)
      tmp1=base::substr(i,1,tmp-1)
      tmp2=base::dirname(i)
      tmp3=base::paste0(tmp2,"/",newdir,"/",basename(tmp1),"_",suffix,".tif")
      newname=base::append(newname,tmp3)
    })
  }else{
    lapply(indir,function(i){
      tmp=base::regexpr("\\.[^\\.]*$", i)
      tmp=base::regexpr("\\.[^\\.]*$", i)
      tmp1=base::substr(i,1,tmp-1)
      tmp2=base::dirname(i)
      tmp3=base::paste0(tmp2,"/",newdir,"/",prefix,"_",basename(tmp1),".tif")
      newname=base::append(newname,tmp3)
    })
    }
  }
  return(newname)
}