test_that("crochet constructor works", {

    expect_error(crochet())
    expect_error(crochet(subset_vector = "derp"))
    expect_error(crochet(subset_matrix = "derp"))
    expect_error(crochet(subset_vector = "derp", subset_matrix = "derp"))
    expect_error(crochet(subset_vector = function() {}, subset_matrix = "derp"))
    expect_error(crochet(subset_vector = "derp", subset_matrix = function() {}))
    expect_error(crochet(subset_vector = function() {}, subset_matrix = function() {}))
    expect_error(crochet(subset_vector = function(x) {}, subset_matrix = function(x) {}))
    expect_error(crochet(subset_vector = function(x, i) {}, subset_matrix = function(x, i) {}))

    expect_type(crochet(subset_vector = function(x, i) {}, subset_matrix = function(x, i, j) {}), "closure")

})
