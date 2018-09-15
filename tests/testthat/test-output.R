context("test-output")

test_that("Output works", {
  df <- data.frame(
    term = c("A", "B", "C", "D"),
    n = c(23, 14, 56, 2),
    m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA)
  )
  
  df1 <-
    stp25APA2::prepare_output(df,
                              caption = "Überschrift",
                              note = "Anmerkung",
                              N = 256)
 
  expect_output(Output(df1, output = "text"),  "term")
  
  expect_output(Output(df1, output = "html"),  "<table")

  expect_output(Output(df1, output = "markdown"),  "<table")
  
})


test_that("tbl_header works", {

df1 <- data.frame(
  term = c("A", "B", "C", "D"),
  n = c(23, 14, 56, 2),
  m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA)
)

df2 <- data.frame(
  term = c("A", "B", "C", "D"),
  G1_k_n = c(23, 14, 56, 2),
  G1_k_m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
  G2_n = c(33, 35, 78, 21),
  G2_m = c("4.9 (2.7)", "4.7 (2.5)", "4.1 (5.6)", "4.2 (5.6)")
  
)

expect_equal(stp25output:::tbl_header(df1)$header,
c( "term" ,"n",    "m" ) )
expect_equal(stp25output:::tbl_header(df2)$cgroup,
c("",   "G1", "G2"))

})





test_that("Output header", {
  df2 <- data.frame(
    term = c("A", "B", "C", "D"),
    G1_k_n = c(23, 14, 56, 2),
    G1_k_m = c("4.7 (2.4)", "4.1 (2.3)", "8.9 (3.6)", NA),
    G2_n = c(33, 35, 78, 21),
    G2_m = c("4.9 (2.7)", "4.7 (2.5)", "4.1 (5.6)", "4.2 (5.6)")
    
  )
  
  expect_output(Output(df2, output = "text"),  "term")
  
  expect_output(Output(df2, output = "html"),  "<table")
  
  expect_output(Output(df2, output = "markdown"),  "<table")
  
})





test_that("xtabs header", {

  require(stp25data)
  require(stp25APA2)
  
hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
hkarz$Tzell<- cut(hkarz$tzell, 3, c("low", "med", "hig"))
 
expect_output(APA2(xtabs(~ gruppe+LAI+Tzell, hkarz),
                   caption="APA_Xtabs: 2x2x3 Tabelle", 
                   test=FALSE, 
                   output="markdown"),
              "<table")

})

