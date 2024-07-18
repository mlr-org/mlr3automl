test_that("single svm works", {
  rush_plan(n_workers = 2)
  lgr::get_logger("mlr3automl")$set_threshold("debug")

  task = tsk("penguins")
  learner = lrn("classif.automl_svm",
    small_data_size = 100,
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 6)
  )

  expect_class(learner$train(task), "LearnerClassifAutoSVM")
})
