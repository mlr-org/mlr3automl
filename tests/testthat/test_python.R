test_that("check_python_packages works", {
  res = check_python_packages(c("package_1", "package_2"))
  expect_equal(res, "Package package_1,package_2 not available.")

  res = check_python_packages("fastai")
  expect_true(res)
})
