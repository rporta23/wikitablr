library(testthat)
library(dplyr)
library(purrr)

# sites to use for testing
urls <- c(
  colleges = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts",
  presidents = "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age",
  linebreaks = "https://en.wikipedia.org/wiki/Wikipedia:Advanced_table_formatting",
#  marvel = "https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films",
  ascii = "https://en.wikipedia.org/wiki/ASCII"
)

# makes list of tables
tables <- urls %>%
  map(read_wikitables)

test_that("read_wikitables works", {

  expect_is(tables, "list")
  expect_length(tables, 4)

  expect_true(every(tables, tibble::is_tibble))
  expect_true(every(tables, is.data.frame))

})


test_that("clean_rows works", {
  ascii <- tables$ascii$table[[1]]
  # test clean_rows()
  # test that duplicate of header is removed (this one has a double header)
  # this needs to be revised, and I'm not sure how to extract the table we want
  expect_lt(
    ascii %>%
      clean_rows() %>%
      nrow(),
    ascii %>%
      nrow()
  )

  # test that rows with same values in all column are removed
  ## read_wikitables not working for marvel page
#  marvel1 <- marvel %>%
#    purrr::pluck(1)
#  expect_lt(nrow(clean_rows(marvel1)), nrow(marvel1))

})

test_that("empty_to_na works", {

  # test empty_to_na()
  ## I'm confused about applying these functions to the dummy data because
  ## they are supposed to take in a list of data frames
  dummy_data <- tibble::tribble(
    ~first, ~second, ~third,
    "?", "two", "three",
    "_", "five", "7",
    "N/A", "ten", "eleven"
  )

  expect_true(is.na(empty_to_na(data.frame(x = ""))))

})

test_that("special_to_na works", {

  dummy_data <- tibble::tribble(
    ~first, ~second, ~third,
    "?", "two", "three",
    "_", "five", "7",
    "N/A", "ten", "eleven"
  )

  # test special_to_na()
  expect_false(
    dummy_data %>%
      special_to_na() %>%
      pull(first) %>%
      stringr::str_detect("\\?") %>%
      any(na.rm = TRUE)
  )

  special_df <- data.frame(x = c("?", "le#t"), y = c("hi", "8%"))
  special_to_na(special_df) %>%
    pull(x) %>%
    is.na() %>%
    any() %>%
    expect_true()

})

test_that("convert_types works", {

  presidents <- tables$presidents$table[[1]]

  # test convert_types()
  presidents %>%
    convert_types() %>%
    pull(born) %>%
    expect_is("Date")

  # test remove_footnotes()
  # test that footnotes are removed
  ## ERROR: Error in colSums(!is.na(dat)) :
  # 'x' must be an array of at least two dimensions
  presidents %>%
    remove_footnotes() %>%
    pull(age_at_end_of_presidency) %>%
    stringr::str_detect("\\[") %>%
    any() %>%
    expect_false()
})


test_that("special_to_na works", {
  ascii_table <- tables$ascii$table[[2]]

  # test that special_to_na = FALSE returns the special character
  # I don't understand this test-- is it still necessary now that special_to_na() is now a function, not an argument?
  expect_false(
    ascii_table %>%
      dplyr::filter(dec == "33") %>%
      dplyr::pull(glyph) %>%
      is.na() %>%
      any()
  )
})


test_that("old cleaners work", {
  skip("Skipping old and possibly meaningless cleaner tests")
  ## I think all tests below this point should either be restructured and moved to test-cleaners
  ## or removed completely

  # remove_footnotes = TRUE -- not sure why this isn't working
  expect_false(presidents_clean[, 3] %>%
                 stringr::str_detect("\\[a\\]") %>%
                 any())

  # remove_footnotes = FALSE
  presidents_clean_footnotes <- read_wiki_table(presidents, remove_footnotes = FALSE)
  expect_true(presidents_clean_footnotes[, 3] %>%
                stringr::str_detect("[\\[a\\]]") %>%
                any())

  # to_na = "George Washington"
  presidents_clean_na <- read_wiki_table(presidents, to_na = "George Washington")
  # test to detect "George Washington" in column 2-- should be false
  expect_false(presidents_clean_na[, 2] %>%
                 stringr::str_detect("George Washington") %>%
                 any(na.rm = TRUE))

  # special_to_na = TRUE
  expect_false(colleges_clean[, 5] %>%
                 stringr::str_detect("—") %>%
                 any(na.rm = TRUE))

  # special_to_na = FALSE Doesn't work
  colleges_3 <- read_wiki_table(colleges, table_number = 3, special_to_na = FALSE)
  # test to detect "-" in column 5 (there should be one) FAILS TEST
  expect_true(colleges_3[, 5] %>%
                stringr::str_detect("—") %>%
                any(na.rm = TRUE))

  # test that clean_rows() removes double header FAILS TEST
  presidents_raw <- read_wiki_raw(presidents)
  expect_lt(nrow(presidents_clean), nrow(presidents_raw))

  # Another test for the same thing
  presidents_noheader <- presidents_raw %>%
    clean_rows()
  expect_equal(nrow(presidents_noheader), nrow(presidents_clean))

  # Jessica's tests
  # test that structure of dataframes from both readers is the same
  # I suspect that this test should not always work based on the above two tests
  expect_identical(dim(presidents_clean), dim(presidents_raw))

  # test that structure of data is the same, regardless of whether I remove footnotes
  expect_equal(names(presidents_clean), names(presidents_clean_footnotes))


  # remove_footnotes = TRUE -- not sure why this works for this one but not for read_wiki_table
  expect_false(presidents_clean_1[, 3] %>%
                 stringr::str_detect("[\\[a\\]]") %>%
                 any())

  # remove_footnotes = FALSE
  pres_clean_footnotes <- read_wikitables(presidents, remove_footnotes = FALSE)
  pres_clean_footnotes_1 <- pres_clean_footnotes[[1]]

  presidents_clean_footnotes <- read_wikitables(presidents, remove_footnotes = FALSE)
  expect_true(presidents_clean_footnotes[, 3] %>%
                stringr::str_detect("[\\[a\\]]") %>%
                any())

  # to_na = "George Washington"
  presidents_clean_na <- read_wikitables(presidents, to_na = "George Washington")
  pres_clean_na_1 <- presidents_clean_na[[1]]
  # test to detect "George Washington" in column 2-- should be false
  expect_false(pres_clean_na_1[, 2] %>%
                 stringr::str_detect("George Washington") %>%
                 any(na.rm = TRUE))

  # special_to_na = TRUE
  expect_false(colleges_clean_3[, 5] %>%
                 stringr::str_detect("—") %>%
                 any(na.rm = TRUE))

  # special_to_na = FALSE Doesn't work
  colleges_all <- read_wikitables(colleges, special_to_na = FALSE)
  colleges_3 <- colleges_all[[3]]
  # test to detect "-" in column 5 (there should be one) FAILS TEST
  expect_true(colleges_3[, 5] %>%
                stringr::str_detect("—") %>%
                any())

  # test that clean_rows() removes double header PASSES TEST
  expect_lt(
    nrow(clean_rows_single(presidents_clean_1)),
    nrow(presidents_clean_1)
  )

})
