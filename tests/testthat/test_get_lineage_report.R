test_that('get lineage report runs properly',{
    expect_false(get_lineage_report(lineages = c("B.1.1.7", "B.1.617.2")))
})
