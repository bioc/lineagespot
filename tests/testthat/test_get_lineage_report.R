


test_that('get lineage report runs properly',{
    result <- get_lineage_report(lineages = c("B.1.1.7", "B.1.617.2"))
                                 
    if (result == FALSE){
        expect_false(result)
    }
    else{
        expect_vector(result)
    }
    
})
