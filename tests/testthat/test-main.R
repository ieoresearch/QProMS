box::use(
  testthat[expect_false, expect_true, test_that],
)

test_that("QProMS state is created per Shiny session", {
  main_source <- readLines("app/main.R", warn = FALSE)

  expect_false(any(grepl("^object <- R6Class_QProMS\\$QProMS\\$new\\(\\)", main_source)))
  expect_true(any(grepl("session_state <- R6Class_QProMS\\$QProMS\\$new\\(\\)", main_source)))
})

test_that("report generation does not use shared app-tree artifacts", {
  download_source <- readLines("app/view/download.R", warn = FALSE)
  report_source <- readLines("app/logic/Report_QProMS.qmd", warn = FALSE)

  expect_false(any(grepl("QProMS_session_internal\\.rds", c(download_source, report_source))))
  expect_false(any(grepl("Report_QProMS\\.html", download_source)))
  expect_true(any(grepl("session_file <- tempfile", download_source, fixed = TRUE)))
  expect_true(any(grepl("params\\$session_file", report_source)))
})
