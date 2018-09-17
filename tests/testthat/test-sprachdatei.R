context("test-sprachdatei")



test_that("Names2Language", {
  expect_true(is.list(Sprachdatei()))
  expect_equal(Sprachdatei("de")$source,
               "Quelle")
  expect_equal(Sprachdatei("en")$source, "Source")
  
  
  expect_equal(Names2Language(c("Pr..Chisq.", "F.value")),
               c(Pr..Chisq. = "p" ,  F.value = "F"))
})
