#' @name clean_wiki_names
#' @title clean_wiki_names
#' @importFrom dplyr %>%
#' @param wiki_table A dataframe for which the column names will be cleaned
#' @param ... arguments passed to \code{\link[janitor]{clean_names}}
#' @return a cleaned dataframe
#' @export
#' @examples
#' url <- "https://en.wikipedia.org/wiki/ASCII"
#' raw <- read_wikitables(url)
#'
#' # clean

clean_wiki_names <- function(wiki_table, ...) {
  #removes all columns without a name
  wiki_table <- wiki_table[!is.na(names(wiki_table))]
  # remove footnotes (which are in brackets) from column names
  names(wiki_table) <- stringr::str_remove_all(names(wiki_table), "\\[.*]")
  # remove "(s)" from column names
  names(wiki_table) <- stringr::str_remove_all(names(wiki_table), "\\(s\\)")
  # remove special characters from column names
  names(wiki_table) <- stringr::str_replace_all(names(wiki_table), "[^a-zA-Z0-9 ]", "_")
  # convert to snake case
  wiki_table <- janitor::clean_names(wiki_table, ...)

  return(wiki_table)
}


#' @name empty_to_na
#' @title empty_to_na
#' @description Convert empty strings and special characters to NA
#' @export
#' @param wiki_table A dataframe
#' @return A dataframe
#' @examples
#' empty_to_na(data.frame(x = ""))

empty_to_na <- function(wiki_table){
  wiki_table %>%
    dplyr::mutate_if(
      is.character, list(~dplyr::na_if(., ""))
    )
}

#' @rdname empty_to_na
#' @export
#' @examples
#' special_to_na(data.frame(x = c("?", "le#t"), y = c("hi", "8%")))

special_to_na <- function(wiki_table) {
  wiki_table %>%
    dplyr::mutate_if(
      is.character,
      stringr::str_replace_all,
      pattern = "\\A[^a-zA-Z0-9]{1}$", replacement = "<special_char/>"
    ) %>%
    dplyr::mutate_if(
      is.character,
      list(~dplyr::na_if(., "<special_char/>"))
    )
}


#' @name remove_footnotes
#' @title remove_footnotes
#' @param wiki_table a dataframe
#' @param ... arguments passed to \code{\link{empty_to_na}}
#' @export
#' @return a cleaned dataframe

remove_footnotes <- function(wiki_table, ...) {
  wiki_table %>%
    purrr::map(~stringr::str_remove_all(.x, "\\[.*]")) %>%
    empty_to_na(...) %>%
    janitor::remove_empty(which = "cols")
}


#' @name clean_rows
#' @title clean_rows
#' @param wiki_table A dataframe for which the rows will be cleaned
#' @export
#' @return Cleaned dataframe

clean_rows <- function(wiki_table) {
  #removes all columns without a name
  wiki_table <- wiki_table[!is.na(names(wiki_table))]

  #removes rows with the same value across all columns
  #line of code pulled from Psidom's stack overflow answer here:
  #https://stackoverflow.com/questions/44398252/remove-rows-with-the-same-value-across-all-columns
  wiki_table <- wiki_table[rowSums(wiki_table[-1] != wiki_table[[2]], na.rm = TRUE) != 0,]

  wiki_table <- wiki_table %>%
    dplyr::mutate_all(as.character)

    tryCatch({i <- 1

    while(i <= nrow(wiki_table)){

      #remove repeats of header in rows
      suppressWarnings(
        if(colnames(wiki_table) == wiki_table[i,]){
          wiki_table <- wiki_table[-c(i),]
          i <- i-1
        }
      )
      i <- i+1
    }}, error = function(e) cat("Error when checking for header duplicates in rows: ",e$message, "\n")
  )

  return(wiki_table)
}


#' @name convert_types
#' @title convert_types
#' @export
#' @param wiki_table A dataframe for which the rows will be cleaned
#' @return Cleaned dataframe

convert_types <- function(wiki_table) {
  suppressWarnings(
    wiki_table <- wiki_table %>%
      dplyr::mutate_all(as.character)%>%
      dplyr::mutate_if(~all(!is.na(lubridate::dmy(.x))), lubridate::dmy)%>%
      dplyr::mutate_if(~all(!is.na(lubridate::mdy(.x))), lubridate::mdy)%>%
      dplyr::mutate_if(~all(!is.na(lubridate::ymd(.x))), lubridate::ymd)%>%
      dplyr::mutate_if(~class(.x) == "character" &&
                         all(!stringr::str_detect(.x, "[a-zA-Z]"), na.rm = TRUE), readr::parse_number))

  return(wiki_table)

}

