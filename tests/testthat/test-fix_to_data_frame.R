context("test-fix_to_data_frame.R")

test_that("matrix", {
  x <- c(10, 45, 14,  11 , 22 ,  1,  2, 0 , NA)
  mx <- matrix(x,
               ncol = 3,
               byrow = TRUE,
               dimnames = list(c("a", "b" , "c"),
                               c("A", "B", "C")))
  res <- fix_to_data_frame(mx)
  
  expect_true(is.data.frame(res) )
  expect_false(any(sapply(res, class) == "factor") )
  expect_equal(dim(res), c(3, 4))
})

test_that("table", {
  x2 <- with(airquality, table(OzHi = Ozone > 80, Month))
  res <- fix_to_data_frame(x2)

  expect_true(is.data.frame(res) )
  expect_false(any(sapply(res, class) == "factor") )
  expect_equal(dim(res), c(2, 6))
})



test_that("ftable", {
  airquality$Tmp <-  cut(airquality$Temp, quantile(airquality$Temp))
  x3 <- with(airquality, table(OzHi = Ozone > 80, Tmp,  Month))
  res <- fix_to_data_frame(x3)
  expect_true(is.data.frame(res) )
  expect_false(any(sapply(res, class) == "factor"))
  expect_equal(dim(res), c(8, 7))
  expect_equal(names(res),
               c(
                 "OzHi",
                 "Tmp" ,
                 "Month_5",
                 "Month_6",
                 "Month_7",
                 "Month_8",
                 "Month_9"
               ))
  
})
