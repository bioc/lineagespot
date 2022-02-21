# lineagespot


Lineagespot is a framework written in [R](https://www.r-project.org/), and aims to identify SARS-CoV-2 related mutations based on a single (or a list) of variant(s) file(s) (i.e., [variant calling format](https://gatk.broadinstitute.org/hc/en-us/articles/360035531692-VCF-Variant-Call-Format)). 


## Installation

```r
# install.packages("devtools")
devtools::install_github("BiodataAnalysisGroup/lineagespot")
```

## Raw data analysis

The processing steps of the raw fastq files can be found [here](inst/scripts/raw-data-analysis.md).

## Citation

If you use the tool, please cite the following work:

Pechlivanis, N., Tsagiopoulou, M., Maniou, M.C. et al. _Detecting SARS-CoV-2 lineages and mutational load in municipal wastewater and a use-case in the metropolitan area of Thessaloniki, Greece._ Sci Rep 12, 2659 (2022). https://doi.org/10.1038/s41598-022-06625-6
