# =============================================================================
# helpers.R
#
# Shared utilities sourced by other scripts in the pipeline.
# =============================================================================

# Normalize a person's first or last name for cross-source merging.
#
# Steps (applied in order):
#   1. transliterate to ASCII ("José" -> "Jose"; curly quotes -> straight) so
#      accented spellings merge across sources instead of losing the letter
#   2. lowercase
#   3. strip quoted nicknames    -> "John \"JJ\" Smith" -> "john   smith"
#   4. strip parenthetical aliases (balanced) -> "John (Jack) Smith"
#   5. strip unbalanced trailing parens -> "John (Jack Smith" -> "john "
#   6. (optional) strip name suffix from end: jr, sr, ii, iii (with optional ".")
#   7. replace any remaining non-letter characters with a space
#   8. collapse runs of whitespace and trim
#
# `strip_suffix = TRUE` is intended for last names; first names rarely carry
# generational suffixes and stripping them there would clip valid given names.
clean_person_name <- function(x, strip_suffix = FALSE) {
  out <- x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all('"[^"]*"', " ") |>
    stringr::str_replace_all("\\([^)]*\\)", " ") |>
    stringr::str_replace_all("\\(.*$", " ")

  if (strip_suffix) {
    out <- stringr::str_remove(out, "\\s+(jr|sr|ii|iii)\\.?$")
  }

  out |>
    stringr::str_replace_all("[^a-z]", " ") |>
    stringr::str_squish()
}
