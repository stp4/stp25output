context("test-projekt")

test_that("Projekt works", {
  x <- Projekt(NULL)
  expect_equal(get_my_options()$output, "text")
  expect_true(is.character(x))
  
})
