#' @title Print results of different types of analyses
#' @description Takes an object, and prints the results as an APA table.
#' @param x Object to be printed
#' @param keepCols Character vector, indicating the columns to retain.
#' Default: c("label", "est_sig", "confint").
#' @param digits Integet. Number of digits to round to when formatting;
#' Default: 2.
#' @param ... Other arguments passed to and from other functions.
#' @return data.frame
#' @rdname printResults
#' @export
printResults <- function(x, keepCols = c("label",
                                         "est_sig", "confint"), digits = 2, ...){
  UseMethod("printResults", x)
}

#' @method printResults rma
#' @export
printResults.rma <-  function(x, keepCols = c("label",
                                              "est_sig", "confint"), digits = 2, ...){

  results <- do.call(cbind, x[c("b", "se", "zval", "pval", "ci.lb", "ci.ub")])
  results <- data.frame(label = rownames(results), results)
  names(results)[2] <- c("est")
  value_columns <- c("est", "se", "zval", "pval", "ci.lb", "ci.ub")
  value_columns <- value_columns[which(value_columns %in%
                                         colnames(results))]
  add_cis <- TRUE
  results$est_sig <- est_sig(results, digits)

  results$confint <- conf_int(results, digits)

  results[, value_columns] <- lapply(results[, value_columns],
                                     formatC, digits = digits, format = "f")
  if (!is.null(keepCols))
    results <- results[, keepCols]
  rownames(results) <- NULL
  results
}
