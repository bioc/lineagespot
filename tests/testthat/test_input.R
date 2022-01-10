

test_that('Input file is vcf',{
    expect_vector(isVcf(
        vcf_folder = system.file("extdata", "vcf-files",package = "lineagespot"
    ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                                package = "lineagespot"
    )
    ))
  
    expect_equal(length(isVcf(
        vcf_folder = system.file("extdata", "vcf-files",package = "lineagespot"
        ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                              package = "lineagespot"
        )
        ))-1,3)
    
    expect_equal(as.integer(isVcf(
        vcf_folder = system.file("scripts", "vcf-files",package = "lineagespot"
        ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                              package = "lineagespot"
        )
        )),0)
    
    expect_error(isVcf(
        vcf_fls = system.file("extdata", "vcf-files",package = "lineagespot"
        ),
        gff3_path = system.file("extdata","NC_045512.2_annot.gff3",
                              package = "lineagespot"
        )))
    
})

test_that('Input file is gff3',{
    expect_true(isGff3(system.file("extdata","NC_045512.2_annot.gff3",
                                    package = "lineagespot")))
    expect_false(isGff3(system.file("extdata","vcf-files",
                                    "SampleA_freebayes_ann.vcf",
                                    package = "lineagespot")))
})

test_that('Input check runs properly',{
    expect_error(input_check(vcf_fls = system.file("extdata", "vcf-files",
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

    expect_error(input_check(vcf_fls = NULL,

                            vcf_folder = NULL,

                            gff3_path = system.file("extdata",
                                                    "NC_045512.2_annot.gff3",
                                                    package = "lineagespot"
                            )))

    expect_error(input_check(vcf_folder = system.file("extdata", "vcf-files",
                                                     package = "lineagespot"
                            ),
                            gff3_path = system.file("extdata", "vcf-files",
                                                    "SampleA_freebayes_ann.vcf",
                                                    package = "lineagespot"
                            )))


    expect_vector(input_check(vcf_folder = system.file("extdata", "vcf-files",
                                                    package = "lineagespot"
                            ),
                            gff3_path = system.file("extdata",
                                                    "NC_045512.2_annot.gff3",
                                                    package = "lineagespot"
                            )))

})