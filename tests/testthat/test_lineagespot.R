
test_that('Test lineagespot runs properly',{
    expect_vector(lineagespot(
        vcf_folder = system.file("extdata", "vcf-files",
                                 package = "lineagespot"
        ),
        gff3_path = system.file("extdata",
                                "NC_045512.2_annot.gff3",
                                package = "lineagespot"
        ),
        ref_folder = system.file("extdata", "ref",
                                 package = "lineagespot"
        )
    ))
})