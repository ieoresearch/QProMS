box::use(
  testthat[expect_false, expect_true, test_that],
)

test_that("uploads use an explicit configurable size limit", {
  main_source <- readLines("app/main.R", warn = FALSE)
  home_source <- readLines("app/view/home.R", warn = FALSE)

  expect_false(any(grepl("10000\\*1024\\^2", main_source)))
  expect_true(any(grepl("QPROMS_MAX_UPLOAD_MB", main_source, fixed = TRUE)))
  expect_true(any(grepl("qproms.maxUploadSizeMb", c(main_source, home_source), fixed = TRUE)))
  expect_true(any(grepl("validate_uploaded_file", home_source, fixed = TRUE)))
})

test_that("restored sessions are loaded only after validation", {
  logic_source <- readLines("app/logic/R6Class_QProMS.R", warn = FALSE)
  home_source <- readLines("app/view/home.R", warn = FALSE)

  expect_true(any(grepl('list.load(input_path, type = "rds")', logic_source, fixed = TRUE)))
  expect_true(any(grepl("validate_parameters_list", logic_source, fixed = TRUE)))
  expect_true(any(grepl("unknown entries are not allowed", logic_source, fixed = TRUE)))
  expect_true(any(grepl("functions, environments, and external pointers", logic_source, fixed = TRUE)))
  expect_true(any(grepl("Invalid session file", home_source, fixed = TRUE)))
})
