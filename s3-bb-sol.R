library("checkmate")

# bb constructor
new_bb <- function(text = character()) {
  # replace ((leading consonants) (1-2 vowels) (trailing consonants)) with
  # ((leading consonants) (vowels) b (same vowels again) (trailing consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  value <- gsub(
    pattern = match,
    replacement = "\\1\\2b\\2\\3",
    x = text
  )
  structure(value, class=c("bb", class(text)))
}
# validator
validate_bb <- function(text) {
  checkmate::assert_class(object, "bb")
  text
}
# generic bb method
bb <- function(text, ...) {
  UseMethod("bb")
}
# default bb method
bb.default <- function(text) {
  checkmate::assert(
    checkmate::check_character(text),
    checkmate::check_list(text, types = c("character", "list")),
    checkmate::check_factor(text)
  )

  validate_bb(new_bb(text))
}
# bb method for factors
bb.factor <- function(text) {
  levels(text) <- unclass(bb.default(levels(text)))
  class(text) <- c("bb", class(text))
  text
}
# bb method for lists
bb.list <- function(text) {
  obj <- lapply(text, bb)
  class(result) <- c("bb", class(obj))
  obj
}

