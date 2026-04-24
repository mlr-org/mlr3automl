test_that("auto sugar function works", {
  obj = auto("catboost")
  expect_class(obj, "AutoCatboost")

  expect_class(auto("ranger"), "AutoRanger")
  expect_class(auto("xgboost"), "AutoXgboost")
})

test_that("auto returns dictionary when key is missing", {
  dict = auto()
  expect_class(dict, "DictionaryAuto")
})
