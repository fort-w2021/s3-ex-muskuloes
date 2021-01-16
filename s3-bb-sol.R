library("checkmate")
new_bb <- function(text = character(),
                   class_name = character()) {
  
  text_ul <- unlist(as.relistable(text))

  # replace ((leading consonants) (1-2 vowels) (trailing consonants)) with
  # ((leading consonants) (vowels) b (same vowels again) (trailing consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  value <- gsub(
    pattern = match,
    replacement = "\\1\\2b\\2\\3",
    x = text_ul
  )

  if ("factor" %in% class(text)) {
    ord <- match(text_ul, levels(text_ul))
    levels <- levels(reorder(unique(value), ord))
    attr(value, "skeleton") <- NULL
    return(structure(
      ord,
      levels = levels,
      class = c("bb", class_name)
    ))
  }

  if ("list" %in% class(text)) {
    value <- relist(value, attr(text_ul, "skeleton"))
    value <- rapply(value, function(v) {
      class(v) <- c("bb", class(v))
      v
    }, how = "replace")
    return(structure(value, class = c("bb", class_name)))
  }

  attr(value, "skeleton") <- NULL
  structure(value, class = c("bb", class_name))
}

# validator
validate_bb <- function(text) {
  checkmate::assert(
    checkmate::check_character(text),
    checkmate::check_list(text, types = c("character", "list")),
    checkmate::check_factor(text)
  )

  text
}
# Helper
bb <- function(text) {
  validate_bb(new_bb(text, class_name = class(text)))
}
