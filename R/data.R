
#' List all files from a directory
#' @import data.table
#' @import tools
#' @param path directory
#' @export
getData <- function(path = ""){
  
  ext <- filename <- file <- NULL

  dt <- data.table(
    file.info(list.files(path, full.names = T))
  ) 
  dt[,`:=`(
    filename = rownames(file.info(list.files(path, full.names = T)))
  )]
  dt[,file := basename(filename)]
  dt[,ext := file_ext(file)]
  dt <- dt[,c("filename", "file",
              setdiff(names(dt), c("filename", "file"))
              ), with = FALSE]
  
  return(dt)
  
}
