# Unit tests for convert_array_fields() (R/ndaTransformations.R): recoding of
# MongoDB list columns and bracket/vector-notation strings to the numeric
# codes defined in NDA field notes ("1=(0.9, 0.7); 2=(0.1, 0.3)").

array_elements <- data.frame(
  name = "task_pattern",
  notes = "1=(0.9, 0.7); 2=(0.1, 0.3)",
  stringsAsFactors = FALSE
)

test_that("convert_array_fields() recodes bracket-notation strings to NDA codes", {
  df <- data.frame(
    src_subject_id = c("SUB001", "SUB002", "SUB003"),
    task_pattern = c("[0.9, 0.7]", "c(0.1, 0.3)", NA),
    stringsAsFactors = FALSE
  )

  out <- convert_array_fields(df, array_elements)

  expect_equal(out$task_pattern, c("1", "2", NA))
})

test_that("convert_array_fields() recodes MongoDB list columns to NDA codes", {
  df <- data.frame(src_subject_id = c("SUB001", "SUB002", "SUB003"))
  df$task_pattern <- list(c(0.9, 0.7), c(0.1, 0.3), NULL)

  out <- convert_array_fields(df, array_elements)

  expect_equal(out$task_pattern, c("1", "2", NA))
})

test_that("convert_array_fields() matches regardless of whitespace and notation", {
  df <- data.frame(
    task_pattern = c("[0.9,0.7]", "c( 0.1 , 0.3 )"),
    stringsAsFactors = FALSE
  )

  out <- convert_array_fields(df, array_elements)

  expect_equal(out$task_pattern, c("1", "2"))
})

test_that("convert_array_fields() leaves non-array fields and values untouched", {
  df <- data.frame(
    task_pattern = c("1", "2"),       # already coded, no bracket notation
    other_field = c("[1, 2]", "[3, 4]"),  # array-like but not in the structure
    stringsAsFactors = FALSE
  )

  out <- convert_array_fields(df, array_elements)

  expect_identical(out, df)
})

test_that("convert_array_fields() errors when array data has no mapping in the notes", {
  no_mapping <- data.frame(name = "task_pattern", notes = "", stringsAsFactors = FALSE)
  df <- data.frame(task_pattern = "[0.9, 0.7]", stringsAsFactors = FALSE)

  expect_error(
    convert_array_fields(df, no_mapping),
    "cannot be automatically recoded"
  )
})

test_that("convert_array_fields() errors when array values match no defined pattern", {
  df <- data.frame(task_pattern = "[5, 5]", stringsAsFactors = FALSE)

  expect_error(
    convert_array_fields(df, array_elements),
    "could not be matched"
  )
})
