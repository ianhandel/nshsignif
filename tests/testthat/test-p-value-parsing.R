context("test-p-value-parsing")

library(nshsignif)

numbers <- c(
  "0.01", "0.05", "0.10",
  "0.009999", "0.0100001", "0.049999"
)


test_that("numbers and alpha", {
  expect_equal(parse_pvalue(numbers, 0.05), c("S", "NS", "NS",
                                              "S", "S", "S"))
  expect_equal(parse_pvalue(numbers, 0.01), c("NS", "NS", "NS",
                                              "S", "NS", "NS"))
  expect_equal(parse_pvalue(numbers, 0.90), c("S", "S", "S",
                                              "S", "S", "S"))
})
