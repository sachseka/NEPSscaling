context("create_error_msg")

test_that("create_error_msg: school = TRUE", {
  msg <- paste0(
    "* Path '", "", "' may not contain CohortProfile.\n",
    "* File format: '", "", "' might be wrong"
  )
  expect_equal(create_error_msg(filepath = "", filetype = "", school = TRUE),
               msg)
})

test_that("create_error_msg: school = FALSE", {
  msg <-  paste0(
    "* Path '", "test", "' may not lead to competence files.\n",
    "* File format: '", "case", "' might be wrong"
  )
  expect_equal(create_error_msg(filepath = "test", filetype = "case", school = FALSE),
               msg)
})
