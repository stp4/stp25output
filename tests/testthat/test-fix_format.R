context("test-fix_format")

test_that("auto format works", {
  df2 <- data.frame(
    term = c("A", "B", "C", "D"),
    Estimate = c(23.5, .14, 5.6, 2.9876),
    df1 = c(3.3, 35., 7.8, 2.1),
    df = c(3, 35, 7, 2),
    N = c(33, 35, 78, 21),
    
    F.value = c(2.73, 12.444, 14.576, 30.412),
    pvalue = c(0.73, 0.044, 0.056, 0.042) 
    
    
  )
  
  
  
  expect_equal(
    fix_format(df2),
    tibble::tibble(
      term = c("A", "B", "C", "D"),
      Estimate = c("23.50", "0.14", "5.60" , "2.99"),
      df1      = c("3.3", "35.0", "7.8" , "2.1"),
      df       = c("3", "35", "7", "2"),
      N        = c("33" , "35" , "78", "21"),
      F.value  = c("2.73", "12.44" , "14.58", "30.41"),
      pvalue   = c(".730" , ".044", ".056", ".042") 
    )
  )
})



test_that("exclude and digits works", {
  df <- data.frame(
    Item = c("a", "b"),
    x = 1:2,
    x2 = c(1.2, 2.3),
    beta =  c(.22, .13),
    est = c(2.4234, .03),
    p.value = c(0.02456, 0.0398),
    stringsAsFactors = FALSE
  )
  
  #stp25rndr::Format2(df , digits = c(NA, 0, 1, 2, 3, 4))
  
  
  expect_equal(
    fix_format(df,  exclude = 2)[-2],
    tibble::tibble(
      Item   = c("a" , "b"),
      x      = c(1 , 2),
      x2     = c("1.20" , "2.30"),
      beta   = c("0.22", "0.13"),
      est    = c("2.42" , "0.03"),
      p.value = c(".025", ".040") 
    )[-2]
  )
  
  expect_equivalent(
  fix_format(df,  digits = c(NA, 0, 1, 2, 3, 4))$p.value,
  
  c("0.0246", "0.0398"))
  
})
