library(testthat)
library(microbiomeMQC)

test_that("microbiomeMQC works correctly", {
  # Correct path to the test data CSV file
  input <- system.file("extdata", "MQC_testdata.csv", package = "microbiomeMQC")
  output <- tempfile(fileext = ".csv")

  # Ensure that the file exists
  expect_true(file.exists(input), info = "Test data file should exist")

  # Run the function and capture warnings
  result <- tryCatch(
    {
      microbiomeMQC(input, "species", output)
      NULL
    },
    warning = function(w) {
      w
    },
    error = function(e) {
      e
    }
  )

  # Print warnings if any
  if (!is.null(result) && inherits(result, "warning")) {
    print(result)
  }

  # Check if the output file is created
  expect_true(file.exists(output))
})
