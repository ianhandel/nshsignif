context("test-p-value-parsing")

library(nshsignif)

numbers <- c(
  "0.01", "0.05", "0.10",
  "0.009999", "0.0100001", "0.049999"
)

p_and_nums <- c("p < 0.04", "p =   0.04", "p > 0.04",
                   "P<0.04", "P=0.04", "P>0.04")

multi_nums <- c("0.05 to 0.06", "0.03 to 0.06")

test_that("numbers and alpha", {
  expect_equal(parse_pvalue(numbers, 0.05), c("S", "NS", "NS",
                                              "S", "S", "S"))
  expect_equal(parse_pvalue(numbers, 0.01), c("NS", "NS", "NS",
                                              "S", "NS", "NS"))
  expect_equal(parse_pvalue(numbers, 0.90), c("S", "S", "S",
                                              "S", "S", "S"))
})

test_that("numbers and P", {
  expect_equal(parse_pvalue(p_and_nums, 0.04), c("S", "NS", "NS",
                                                 "S", "NS", "NS"))
})

test_that("multiple numbers", {
  expect_equal(parse_pvalue(multi_nums, 0.05), c("NS", "S"))
})

test_that("Missing and NA", {
  expect_equal(parse_pvalue(c("NA", NA, "missing"), 0.05), c("not-tested",
                                                      "not-tested",
                                                      "not-tested"))
})
