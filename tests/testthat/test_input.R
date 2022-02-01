

test_that('Input file is vcf',{
    expect_vector(list_vcf(
        vcf_folder = system.file("extdata", "vcf-files",package = "lineagespot"
    ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                                package = "lineagespot"
    )
    ))
  
    expect_equal(length(list_vcf(
        vcf_folder = system.file("extdata", "vcf-files",package = "lineagespot"
        ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                              package = "lineagespot"
        )
        )),3)
    
    expect_equal(length(list_vcf(
        vcf_folder = system.file("scripts", "vcf-files",package = "lineagespot"
        ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                              package = "lineagespot"
        )
        )),0)
    
    
    
})

test_that('Input file is gff3',{
    expect_true(is_gff3(system.file("extdata","NC_045512.2_annot.gff3",
                                    package = "lineagespot")))
    expect_false(is_gff3(system.file("extdata","vcf-files",
                                    "SampleA_freebayes_ann.vcf",
                                    package = "lineagespot")))
    expect_false(is_gff3(("/path/to/fake/gff3/file.csv")))
})

test_that('Input check runs properly',{
    expect_error( list_input(vcf_fls = system.file("extdata", "vcf-files",
                                                   "SampleA_freebayes_ann.vcf",
                                                   package = "lineagespot"
                            ),
                            vcf_folder = system.file("extdata", "vcf-files",
                                package = "lineagespot"
                            ),
                            gff3_path = system.file("extdata",
                                "NC_045512.2_annot.gff3",
                                package = "lineagespot"
                            )
                            ))

    expect_error( list_input(vcf_fls = NULL,

                            vcf_folder = NULL,

                            gff3_path = system.file("extdata",
                                                    "NC_045512.2_annot.gff3",
                                                    package = "lineagespot"
                            )))

    expect_error( list_input(vcf_folder = system.file("extdata", "vcf-files",
                                                     package = "lineagespot"
                            ),
                            gff3_path = system.file("extdata", "vcf-files",
                                                    "SampleA_freebayes_ann.vcf",
                                                    package = "lineagespot"
                            )))


    expect_vector( list_input(vcf_folder = system.file("extdata", "vcf-files",
                                                    package = "lineagespot"
                            ),
                            gff3_path = system.file("extdata",
                                                    "NC_045512.2_annot.gff3",
                                                    package = "lineagespot"
                            )))

})