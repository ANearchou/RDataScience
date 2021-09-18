#' Use data.table::fwrite to write and zipped a file
#' @import data.table
#' @import parallel
#' @param data file to export
#' @param filename path to be exported to
#' @param seven_zip 7z installation path
#' @param sep column seperator
#' @family read_write
#' @export
write7z <- function(data, filename, seven_zip = sevenzip_ath, sep = ","){

    file <- paste0(getwd(), "/", gsub(".7z", 
        "", basename(filename), ignore.case = T))
    
    data.table::fwrite(data, file, sep = sep)
    gc()
    system(paste0(seven_zip, " a -aoa -mx7 -m0=LZMA2 -mmt", 
        parallel::detectCores(), " \"", filename, "\" \"", 
        file, "\""), ignore.stdout = TRUE)
    file.remove(file)
    return(TRUE)


}

#' Use data.table::fread to read a zipped file
#' @import data.table
#' @param file file to read
#' @param seven_zip 7z installation path
#' @family read_write
#' @export
read7z <- function(file, seven_zip = sevenzip_ath){
    txtfile <- paste0(getwd(), "/", gsub(".7z", 
        "", basename(file), ignore.case = T))
    cmd <- paste0(seven_zip, " x \"", file, "\" -aoa -o\"", 
        getwd(), "\"")
    system(cmd, ignore.stdout = TRUE)
    dt <- data.table::fread(txtfile)
    file.remove(txtfile)
    return(dt)
}


#' Use feather::write_feather to write and zipped a file
#' @import feather
#' @import parallel
#' @param data file to export
#' @param filename path to be exported to
#' @param seven_zip 7z installation path
#' @family read_write
#' @export
write7zfeather <- function(data, filename, seven_zip = sevenzip_ath){

    file <- paste0(getwd(), "/", gsub(".7z", 
        "", basename(filename), ignore.case = T))
    
    feather::write_feather(data, file)
    gc()
    system(paste0(seven_zip, " a -aoa -mx7 -m0=LZMA2 -mmt", 
        parallel::detectCores(), " \"", filename, "\" \"", 
        file, "\""), ignore.stdout = TRUE)
    file.remove(file)
    return(TRUE)

}

#' Use feather::read_feather to read a zipped file
#' @import data.table
#' @import feather
#' @param file file to read
#' @param seven_zip 7z installation path
#' @family read_write
#' @export
read7zfeather <- function(file, seven_zip = sevenzip_ath){
    featherfile <- paste0(getwd(), "/", gsub(".7z", 
        "", basename(file), ignore.case = T))
    cmd <- paste0(seven_zip, " x \"", file, "\" -aoa -o\"", 
        getwd(), "\"")
    system(cmd, ignore.stdout = TRUE)
    dt <- data.table::data.table(feather::read_feather(featherfile))
    file.remove(featherfile)
    return(dt)
}
