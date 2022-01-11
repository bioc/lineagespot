
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
        
        if (length(vcf_folder) == 1){
          
          # if(endsWith(vcf_folder,'.vcf')){
          #   
          #   stop('Please provide one path to a folder of VCF files.')
          #   
          # } else {
          #   
          #   vcf_l <- list.files(vcf_folder,pattern = "vcf",
          #                       full.names = TRUE)
          # }
          
            vcf_l <- list.files(
              vcf_folder,
              pattern = "vcf",
              full.names = TRUE
            )
            
            return( vcf_l )
          
        } else {
          
          stop(c(
            "Please provide a path to a folder containing VCF files."
          ))
        }
        
        
      } else {
        
        vcf_fls <- vcf_fls[which(str_detect(vcf_fls, "vcf"))]
        
        if (length(vcf_fls) == 0) {
          
          stop('No VCF files have been provided.')
          
        }
      }
      
      # num_vcf <- length(vcf_l)
      
      
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
    
    
    vcf_fls <- isVcf(vcf_fls, vcf_folder, gff3_path)
    # vcf_li <- isVcf_info[1:tail(isVcf_info,n=1)]
    # vcf_exists <- as.integer(tail(isVcf_info,n=1))
   

    if (length(vcf_fls) == 0){
        stop("No VCF is found. Please insert valid input files.")
    }
    
    if (isGff3(gff3_path) == FALSE){
        stop('No valid gff3 was given. Please insert path of valid gff3.')
    }
    
    
    return(vcf_fls)
    
}










