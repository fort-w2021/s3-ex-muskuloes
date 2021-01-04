new_bb <- function(text) {
  # replace ((leading consonants) (1-2 vowels) (trailing consonants)) with
  # ((leading consonants) (vowels) b (same vowels again) (trailing consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
}
texttest <- "Bedeutet nach jedem Vokal oder Diphtong die Konsonanten..."
new_bb(texttest)
