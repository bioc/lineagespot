
#' isVcf
#'
#' @param vcf_fls 
#' A character vector of paths to VCF files
#' 
#' @param vcf_folder 
#' A path to a folder containing all VCF files
#' that will be integrated into a single table
#' 
#' @param gff3_path 
#' Path to GFF3 file containing SARS-CoV-2 gene coordinates.
#' 
#' @param vcf_exists 
#' A parameter indicating the number of VCF files 
#' stored in the VCF list.
#'
#' @description
#' Identify VCF files from a group of files.
#'
#' @return
#' A list of two elements;
#' * VCF list; A list where only VCF files are stored.
#' 
#' * number of VCF files; A parameter indicating the number of VCF files 
#' stored in the VCF list.
#' 
#'
#' @examples
#'
#' isVcf_info <- isVcf(
#'     vcf_folder = system.file("extdata", "vcf-files",
#'                              package = "lineagespot"
#'     ),
#'     gff3_path = system.file("extdata",
#'                           "NC_045512.2_annot.gff3",
#'                           package = "lineagespot"
#'     )
#' )
#' print(isVcf_info)
#' 

isVcf <- function(vcf_fls = NULL,
                  vcf_folder = NULL,
                  gff3_path= NULL,
                  vcf_exists = 0){
    
    if (is.null(vcf_fls) && is.null(vcf_folder)){
        stop('No VCF was given')
    }
    
    else if (!is.null(vcf_fls) && !is.null(vcf_folder)){
        stop('Please give only one source for VCF.')
    }
    
    else if (is.null(vcf_fls) | is.null(vcf_folder)){ 
        if (is.null(vcf_fls)){
            if (length(vcf_folder) == 1){
                if(endsWith(vcf_folder,'.vcf')){
                    stop('Please provide one path to a folder of VCF files.')
                }
                else{
                    vcf_l <- list.files(vcf_folder,pattern = "vcf",
                    full.names = TRUE)
                }
                
            }
            else{
                stop('Please give only one path to a folder 
                    containing all VCF files ')
            }
                
            
        }
        else{
            vcf_l <- vcf_fls
            if (length(which(endsWith(vcf_l,'.vcf'))) == 0) {
                stop('Vcf_ls paramater does not contain any path to VCF')
            }
        }
        
        num_vcf <- as.character(length(vcf_l))
        
        return(c((vcf_l),(num_vcf)))
        }
  
}


#' isGff3
#'
#' @description
#' Identify whether a file is in GFF3 format.
#'
#' @param file 
#' Path to GFF3 file.
#'
#' @return
#' result; TRUE if the input file is in GFF3 format, FALSE if not.
#' 
#'
#' @examples
#' 
#' gff3_path <- system.file("extdata","NC_045512.2_annot.gff3",
#'                          package = "lineagespot")
#' isGff3(gff3_path)
#' 

isGff3 <- function(file){
    
    if (is.null(file)) {
        stop("Please provide a valid GFF3 file containing gene coordinates")
    }
    if (length(file) >1) {
        stop("Please give path of only one gff3.")
    }
    res <- str_detect(file,'.gff3')
    return(res)
}


#' input_check
#' 
#' @description 
#' Check the validity of input parameters from lineagespot function.
#'
#' @param vcf_fls 
#' A character vector of paths to VCF files.
#' 
#' @param vcf_folder 
#' A path to a folder containing all VCF files
#' that will be integrated into a single table.
#' 
#' @param gff3_path 
#' Path to GFF3 file containing SARS-CoV-2 gene coordinates.
#' 
#' @importFrom utils tail
#' 
#' @return
#' Return a character vector of paths to VCF files.
#' 
#' 
#'
#' @examples
#' 
#' 
#' vcflist <- input_check(
#'   vcf_folder = system.file("extdata", "vcf-files",
#'                            package = "lineagespot"
#'   ),
#'   gff3_path = system.file("extdata",
#'                           "NC_045512.2_annot.gff3",
#'                           package = "lineagespot")
#' )

input_check <- function(
    
    vcf_fls = NULL,
    vcf_folder = NULL,
    gff3_path= NULL){
    
    
    isVcf_info <- isVcf(vcf_fls,vcf_folder,gff3_path)
    vcf_li <- isVcf_info[1:tail(isVcf_info,n=1)]
    vcf_exists <- as.integer(tail(isVcf_info,n=1))
   

    if (vcf_exists == 0){
        stop("No VCF is found. Please insert valid input files.")
    }
    
    if (isGff3(gff3_path) == FALSE){
        stop('No valid gff3 was given. Please insert path of valid gff3.')
    }
    
    
    return (vcf_li)
    
}










