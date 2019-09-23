#```{r chunk1, eval=opts_current$get("label") %in% eval_chunks}
#print(opts_current$get("label"))

#' Run chunk and save to file, load file if it exists
#'
#' For an Rmarkdown chunk, check if a file exists containing the results,
#' and if it doesn't, run an expression, and save the resulting objects.
#' Useful when writing Rmarkdown
#' reports with time-intensive analyses; these can be run once and then saved.
#' @param expr An expression to be evaluated if the filename does not exist.
#' Multiple expressions should be enclosed in curly braces. Objects created by
#' this expression are stored in a file called \code{filename}.
#' @author Caspar J. van Lissa
#' @export

save_chunk <- function(run_expression) {
  expr_text <- deparse(quote(run_expression))
  expr_nchar <- nchar(expr_text) # Computing a checksum is overkill and requires external package
  #file_name <- paste0(opts_current$get()$root.dir, "/", opts_current$get()$label, "_", expr_nchar, ".RData")
  file_name <- paste0(opts_current$get()$label, "_", expr_nchar, ".RData")
  if(!file.exists(file_name)){
    old_envir <- ls(envir = parent.frame())
    invisible(eval(expr = run_expression, envir = parent.frame()))
    new_objects <- ls(envir = parent.frame())[which(!ls(envir = parent.frame()) %in% old_envir)]
    save(list = new_objects, file = file_name, envir = parent.frame())
  } else {
    cat("Loaded from ", file_name)
    load(file_name, envir = parent.frame())
  }
}
