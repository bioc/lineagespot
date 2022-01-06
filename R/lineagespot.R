#'
#' lineagespot
#'
#' @description
#' Identify SARS-CoV-2 related mutations based on a single (or a list) of
#' variant(s) file(s)
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
#' @param ref_folder
#' A path to a folder containing lineage reports
#'
#' @param voc
#' A character vector containing the names of the lineages of interest
#'
#' @param AF_threshold
#' A parameter indicating the AF threshold for identifying variants per sample
#'
#' @import data.table
#' @importFrom stringr str_detect str_split str_squish str_remove_all
#' @importFrom stringr str_replace_all str_detect str_to_upper str_remove
#'             str_length
#' @importFrom httr GET content
#' @importFrom VariantAnnotation readVcf info fixed
#' @importFrom SummarizedExperiment assays
#' @importFrom MatrixGenerics rowRanges
#'
#' @return
#' A list of three elements;
#' * Variants' table; A data table containing all variants that are
#' included in the input VCF files
#'
#' * Lineage hits; A data table containing identified hits between the input
#' variants and outbreak.info's lineage reports
#'
#' * Lineage report; A data table with computed metrics about
#' the prevalence of the lineage of interest per sample.
#'
#'
#'
#' @export
#'
#' @examples
#'
#' results <- lineagespot(
#'     vcf_folder = system.file("extdata", "vcf-files",
#'         package = "lineagespot"
#'     ),
#'     gff3_path = system.file("extdata",
#'         "NC_045512.2_annot.gff3",
#'         package = "lineagespot"
#'     ),
#'     ref_folder = system.file("extdata", "ref",
#'         package = "lineagespot"
#'     )
#' )
#'
#' head(results$lineage.report)
#' 

lineagespot <- function(vcf_fls = NULL,
                        vcf_folder = NULL,
                        gff3_path = NULL,
                        ref_folder = NULL,
                        voc = c("B.1.617.2", "B.1.1.7", "B.1.351", "P.1"),
                        AF_threshold = 0.8
) {
    input_check(vcf_fls,vcf_folder,gff3_path,ref_folder)
    
    vcf_table <- merge_vcf(
        vcf_fls = vcf_fls,
        vcf_folder = vcf_folder,
        gff3_path = gff3_path
    )
    
    
    
    
    hits_table <- lineagespot_hits(
        vcf_table = vcf_table,
        ref_folder = ref_folder,
        voc = voc
    )
    
    
    
    
    lineage_report <- uniq_variants(
        hits_table = hits_table,
        AF_threshold = AF_threshold
    )
    
    
    out <- list(
        "variants.table" = vcf_table,
        "lineage.hits" = hits_table,
        "lineage.report" = lineage_report
    )
    
    
    return(out)
    
}

# Removes R CMD check NOTE regarding global variables
utils::globalVariables(
    c(
        "DP", ".", "AD_alt", "POS", "Gene_Name", "AA_alt",
        "start_pos", "end_pos", "gene_name", "lineage",
        "AF", ""
    )
)




#' isVcf
#'
#' @description
#' Identify VCF files from a group of files.
#'
#' @param file 
#' A path to a folder containing all VCF files
#' that will be integrated into a single table (or A character vector of paths
#' to VCF files.)
#'
#' @return
#' A list of two elements;
#' * VCF list; A list where only VCF files are stored.
#' 
#' * number of VCF files; A parameter indicating the number of VCF files 
#' stored in the VCF list.
#' 
#' @export
#'
#' @examples
#'
#' vcf_exists <- isVcf(system.file("extdata", "vcf-files",
#'                                 package = "lineagespot"))
#' print(vcf_exists)
#' 

isVcf <- function(file){
  
  if (length(file) ==1) {
    vcf_l <- list.files(file)
  }
  else{
    vcf_l <- file
  }
  
  vcf_l <- vcf_l[which(str_detect(vcf_l,'.vcf'))]
  num_vcf <- length(vcf_l)
  
  
  return(num_vcf)
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
#' @export
#'
#' @examples
#' 
#' gff3_path <- system.file("extdata","NC_045512.2_annot.gff3",
#'                          package = "lineagespot")
#' isGff3(gff3_path)
#' 

isGff3 <- function(file){
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
#' @param ref_folder 
#' A path to a folder containing lineage reports
#' 
#' @param vcf_exists 
#' A parameter indicating the number of VCF files 
#' stored in the VCF list.
#'
#'
#' @return
#' Return TRUE if all input parameters are valid.
#' 
#' 
#' @export
#'
#' @examples
#' 
#' 
#' input_check(
#'   vcf_folder = system.file("extdata", "vcf-files",
#'                            package = "lineagespot"
#'   ),
#'   gff3_path = system.file("extdata",
#'                           "NC_045512.2_annot.gff3",
#'                           package = "lineagespot"
#'   ),
#'   ref = system.file("extdata", "ref",
#'                     package = "lineagespot"
#'   ))
#' 

input_check <- function(
  
  vcf_fls = NULL,
  vcf_folder = NULL,
  gff3_path= NULL,
  ref_folder = NULL,
  vcf_exists = 0){
  
  
  if (length(gff3_path) >1) {
    stop("Please give path of only one gff3.")
  }
  
  if (is.null(vcf_fls) && is.null(vcf_folder)){
    stop('No VCF was given')
  }
  
  else if (!is.null(vcf_fls) && !is.null(vcf_folder)){
    stop('Please give only one source for VCF.')
  }
  
  else if (is.null(vcf_fls) | is.null(vcf_folder)){ 
    if (is.null(vcf_fls)){
      files_l <- vcf_folder
    }
    else{
      files_l <- vcf_fls
    }
    
    vcf_exists <- isVcf(files_l)
    
    if (vcf_exists == 0){
      stop("No VCF is found. Please insert valid input files.")
    }
    
    
  } 
  
  if (isGff3(gff3_path) == FALSE){
    stop('No valid gff3 was given. Please insert path of valid gff3.')
  }
  return (TRUE)
  
}