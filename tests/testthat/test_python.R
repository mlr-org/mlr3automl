test_that("check_python_packages works", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  res = check_python_packages(c("package_1", "package_2"))
  expect_equal(res, "Package package_1,package_2 not available.")

  res = check_python_packages("fastai")
  expect_true(res)
})

test_that("check_python_packages caches results per session", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  key = calculate_hash(c("package_1", "package_2"), NULL)
  on.exit(rm(list = key, envir = python_package_cache), add = TRUE)

  res = check_python_packages(c("package_1", "package_2"))
  expect_equal(res, python_package_cache[[key]])

  # a seeded cache entry short-circuits the probe
  python_package_cache[[key]] = "cached"
  expect_equal(check_python_packages(c("package_1", "package_2")), "cached")
})
