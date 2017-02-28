context("extract")

test_that("extract function works", {

    expect_error(extract())
    expect_error(extract(extract_vector = "derp"))
    expect_error(extract(extract_matrix = "derp"))
    expect_error(extract(extract_vector = "derp", extract_matrix = "derp"))
    expect_error(extract(extract_vector = function() {}, extract_matrix = "derp"))
    expect_error(extract(extract_vector = "derp", extract_matrix = function() {}))
    expect_error(extract(extract_vector = function() {}, extract_matrix = function() {}))
    expect_error(extract(extract_vector = function(x) {}, extract_matrix = function(x) {}))
    expect_error(extract(extract_vector = function(x, i) {}, extract_matrix = function(x, i) {}))

    expect_type(extract(extract_vector = function(x, i) {}, extract_matrix = function(x, i, j) {}), "closure")

})
