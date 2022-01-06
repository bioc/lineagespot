
test_that('Merge vcf returns a table',{
    expect_type(merge_vcf(
        vcf_folder = system.file("extdata",
                                 "vcf-files",
                                 package = "lineagespot"
        ),
        gff3_path = system.file("extdata",
                                "NC_045512.2_annot.gff3",
                                package = "lineagespot"
        )
    ),'list')
})


