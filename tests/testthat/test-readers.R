library(testthat)
library(dplyr)
library(purrr)

# webpages for tests
urls <- c(
  colleges = "https://en.wikipedia.org/wiki/List_of_colleges_and_universities_in_Massachusetts",
  presidents = "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_age",
  linebreaks = "https://en.wikipedia.org/wiki/Wikipedia:Advanced_table_formatting"
)

test_that("read_wikinodes works", {
  # Reads first presidents table with all default parameters
  raw <- urls %>%
    map(read_wikinodes)

  # test that each item is a list
  expect_is(raw, "list")
  expect_length(raw, 3)
  expect_true(
    raw %>%
      map_chr(class) %>%
      `==`("xml_nodeset") %>%
      all()
  )
  expect_true(
    raw %>%
      map(~map_chr(., class)) %>%
      unlist() %>%
      `==`("xml_node") %>%
      all()
  )

})

test_that("read_wikitables works", {
  raw <- urls %>%
    map(read_wikitables)

  # test that each item is a data frame
  expect_is(raw, "list")
  expect_length(raw, 3)
  map(raw, expect_s3_class, "tbl_df")
  map(raw, expect_s3_class, "data.frame")

  raw %>%
    pluck("presidents") %>%
    pull(table) %>%
    map(expect_s3_class, "tbl_df")

  ## linebreak option
  p1_default <- raw %>%
    pluck("presidents") %>%
    pull(table) %>%
    pluck(1)

  # test to detect "," in column 4
  expect_true(
    p1_default %>%
      pull(age_at_start_of_presidency) %>%
      stringr::str_detect(",") %>%
      any()
  )

  # test to detect "," in column 5
  expect_true(
    p1_default %>%
      pull(age_at_end_of_presidency) %>%
      stringr::str_detect(",") %>%
      any()
  )

  # test that linebreaks in cells are replaced with "/" if replace_linebreak = "/ "
  p1_slash <- urls %>%
    pluck("presidents") %>%
    read_wikitables(replace_linebreak = "/ ") %>%
    pull(table) %>%
    pluck(1)

  # test to detect "/" in column 4
  expect_true(
    p1_slash %>%
      pull(age_at_start_of_presidency) %>%
      stringr::str_detect("/ ") %>%
      any()
  )

  # test to detect "/" in column 5
  expect_true(
    p1_slash %>%
      pull(age_at_end_of_presidency) %>%
      stringr::str_detect("/ ") %>%
      any()
  )

  # replace_linebreak = "/ "
  linebreaks_table_2 <- urls %>%
    pluck("linebreaks") %>%
    read_wikitables(replace_linebreak = "/ ") %>%
    pull(table) %>%
    pluck(2)

  # test to detect "/" in column 1
  expect_true(
    linebreaks_table_2 %>%
      pull(1) %>%
      stringr::str_detect("/") %>%
      any()
  )

})


