#' @title Count references (in text on clipboard)
#' @description Counts the number of references (identified by the \code{@}
#' symbol) in a character vector, which is read from the clipboard by default.
#' @param x Character vector, Default: readClipboard()
#' @return Object of class \code{table}
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  text <- c("Although sex differences in emotional adjustment are sometimes",
#'            "found in older children [@silkAdolescentsEmotionRegulation2003;",
#'            "@vanlissaRoleFathersMothers2018a], a meta-analysis found little",
#'            "evidence for such sex differences in young children",
#'            "[@else-questGenderDifferencesTemperament2006a].")
#'  count_references(text)
#'  }
#' }
#' @rdname count_references
#' @export
count_references <- function(x = readClipboard()){
  ref_lines <- x[grepl("@", x)]

  all_refs <- unlist(lapply(ref_lines, function(a_line){
    words <- strsplit(a_line, "\\s+")[[1]]
    ref_words <- words[grepl("@", words)]
    gsub("(\\[|\\]|\\.|\\;)", "", ref_words)
  }))
  tab_refs <- table(all_refs)
  tab_refs[order(tab_refs)]
}
