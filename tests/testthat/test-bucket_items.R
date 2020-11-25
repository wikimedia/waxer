test_that("bucket size 1", {
  buckets <- wx_bucket_items(month.name, 1)
  expect_length(buckets, 12)
})

test_that("one bucket", {
  buckets <- wx_bucket_items(month.name, 12)
  expect_length(buckets, 1)
})

test_that("equally-sized buckets", {
  buckets <- wx_bucket_items(month.name, 4)
  expect_true(all(vapply(buckets, length, 0L) == c(4, 4, 4)))
})

test_that("remainders", {
  buckets <- wx_bucket_items(month.name, 5)
  expect_true(all(vapply(buckets, length, 0L) == c(5, 5, 2)))
})
