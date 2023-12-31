% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_checking.R
\name{list_vcf}
\alias{list_vcf}
\title{list_vcf}
\usage{
list_vcf(vcf_fls = NULL, vcf_folder = NULL, gff3_path = NULL)
}
\arguments{
\item{vcf_fls}{A character vector of paths to VCF files}

\item{vcf_folder}{A path to a folder containing all VCF files
that will be integrated into a single table}

\item{gff3_path}{Path to GFF3 file containing SARS-CoV-2 gene coordinates.}
}
\value{
\itemize{
\item VCF list; A list where only VCF files are stored.
}
}
\description{
Identify VCF files from a group of files.
}
\examples{

list_vcf_info <- list_vcf(
  vcf_folder = system.file("extdata", "vcf-files",
    package = "lineagespot"
  ),
  gff3_path = system.file("extdata",
    "NC_045512.2_annot.gff3",
    package = "lineagespot"
  )
)
print(list_vcf_info)
}
