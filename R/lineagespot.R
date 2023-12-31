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
#' @importFrom utils globalVariables
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
globalVariables(
    c(
        "DP", ".", "AD_alt", "POS", "Gene_Name", "AA_alt",
        "start_pos", "end_pos", "gene_name", "lineage",
        "AF", ""
    )
)






