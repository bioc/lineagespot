library(lineagespot)
test_that(' Lineagespot hits and uniq variants functions run properly',{
  
    lineage_hits_table <- lineagespot_hits(
        vcf_table = merge_vcf(
            vcf_folder = system.file("extdata", "vcf-files",
                                     package = "lineagespot"
            ),
            gff3_path = system.file("extdata",
                                    "NC_045512.2_annot.gff3",
                                    package = "lineagespot"
            )
        ),
        ref_folder = system.file("extdata", "ref",
            package = "lineagespot")
    )
    
    expect_type( uniq_variants(hits_table = lineage_hits_table),'list')
    expect_type(lineage_hits_table,'list')
})