
#' list_vcf
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
#' @description
#' Identify VCF files from a group of files.
#'
#' @return
#' * VCF list; A list where only VCF files are stored.
#' 
#'
#' @examples
#'
#' list_vcf_info <- list_vcf(
#'     vcf_folder = system.file("extdata", "vcf-files",
#'                              package = "lineagespot"
#'     ),
#'     gff3_path = system.file("extdata",
#'                           "NC_045512.2_annot.gff3",
#'                           package = "lineagespot"
#'     )
#' )
#' print(list_vcf_info)
#' 

list_vcf <- function(vcf_fls = NULL,
                  vcf_folder = NULL,
                  gff3_path= NULL){
    
    if (is.null(vcf_fls) & is.null(vcf_folder)){
        stop('No VCF was given')
    }
    
    else if (!is.null(vcf_fls) & !is.null(vcf_folder)){
        stop(c(
          "Please give only one source for VCF. ",
          "Other a vector of paths to VCF files",
          "or a folder containing VCF files"
        ))
    }
    
    else {
      
      if (is.null(vcf_fls)){
        
        vcf_fls <- list.files(
            vcf_folder,
            pattern = "vcf",
            full.names = TRUE
          )

          return( vcf_fls )
      
        
      } else {
        
        vcf_fls <- vcf_fls[which(str_detect(vcf_fls, "vcf"))]
        
        if (length(vcf_fls) == 0) {
          
          stop('No VCF files have been provided.')
          
        }
        
        
        return(vcf_fls)
      }
      
      
    }
  
}


#' list_gff3
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
#' list_gff3(gff3_path)
#' 

list_gff3 <- function(file){
    
    if (is.null(file)) {
        stop(c(
          "Please provide a valid GFF3 file",
          " containing SARS-CoV-2 genes' coordinates.\n",
          "\tSee ", system.file("extdata",
                              "NC_045512.2_annot.gff3",
                              package = "lineagespot")
        ))
    }
  
    if (length(file) > 1) {
        stop("Please give path of only one GFF3 file.")
    }
  
    res <- str_detect(file, 'gff3')
    
    return(res)
}


#' list_input
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
#' @return
#' Return a character vector of paths to VCF files.
#' 
#' 
#'
#' @examples
#' 
#' 
#' vcflist <- list_input(
#'   vcf_folder = system.file("extdata", "vcf-files",
#'                            package = "lineagespot"
#'   ),
#'   gff3_path = system.file("extdata",
#'                           "NC_045512.2_annot.gff3",
#'                           package = "lineagespot")
#' )

list_input <- function(
    
    vcf_fls = NULL,
    vcf_folder = NULL,
    gff3_path= NULL){
    
    
    vcf_fls <- list_vcf(vcf_fls, vcf_folder, gff3_path)
   

    if (length(vcf_fls) == 0){
        stop("No VCF is found. Please insert valid input files.")
    }
    
    if (list_gff3(gff3_path) == FALSE){
        stop('No valid gff3 was given. Please insert path of valid gff3.')
    }
    
    
    return(vcf_fls)
    
}









