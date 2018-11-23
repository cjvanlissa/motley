#' @title Update citekeys using fuzzy string matching
#' @description When a new version of Zotero is released, sometimes, citekeys
#' are changed, thereby breaking references in an existing Rmarkdown file. This
#' function:
#' \enumerate{
#' \item Takes in a text string, and a file path to the .bib file
#' \item Extracts citekeys from both the text string and bib file
#' \item Uses fuzzy matching to replace the citekeys in text with the most
#' likely matching citekeys from bibfile.
#' }
#' @param text Character string. Text from an Rmarkdown file.
#' @param bibfile Character string. Path to .bib file.
#' @param ... Arguments passed to \code{\link[base]{agrep}}
#' @return A character string.
#' @examples
#' \dontrun{
#' update_citekeys(readLines("clipboard"), "references.bib")
#' }
#' @rdname update_citekeys
#' @export
update_citekeys <- function(text, bibfile, ...){
  keys <- readLines(bibfile)
  keys <- keys[grepl("^@\\w+\\{(.+),$", keys)]
  keys <- sapply(keys, gsub, pattern = "^@\\w+\\{(.+),$", replacement = "\\1", USE.NAMES = FALSE)

  textkeys <- substring(unlist(strsplit(trimws(gsub("(@\\w+)|.", "\\1 ", text)), "\\s+")), first = 2)

  sapply(textkeys, agrep, x = keys, value = TRUE)
  outstring <- text
  for(replacekey in textkeys){
    outstring <- gsub(replacekey, agrep(replacekey, x = keys, value = TRUE, ...), outstring)
  }
  outstring
}

#' Find text in files
#'
#' Finds literal text in files. Useful if, for example, you want to replace a
#' function name in all files of an R-package, or if you want to see whether a
#' function can be deprecated because it is not used.
#' @param text Character vector. Literal text to search
#' @param max_size Numeric. Maximum file size to read.
#' @param dir Character. Which directory to search.
#' @param ... Additional arguments, passed on to \code{list.files}.
#' @return Nothing. Prints directly to the console.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' # Make me!
find_in_files <- function(text, max_size = 100000, dir = getwd(), ...){
  text <- escape_special(text)
  all_files <- list.files(dir, full.names = TRUE, ...)
  file_info <- file.info(all_files)
  all_files <- all_files[file_info$size < max_size & file_info$size > 0]
  file_contents <- lapply(all_files, readLines)
  text_occurs_here <- lapply(file_contents, function(x){
    which(sapply(1:length(x), function(i){ grepl(pattern = text, x = x[i])}))
  })
  found_it <- which(sapply(text_occurs_here, length) != 0)

  out_list <- lapply(found_it, function(x){
    cat("In file ", all_files[x], ":\n\n")
    cat(paste(file_contents[[x]][text_occurs_here[[x]]], collapse = "\n"), "\n\n")
  })

}


#' Escape special characters
#'
#' Takes in a string, and escapes any special characters so you can search for
#' the literal string using regex.
#' @param strings Character vector.
#' @return Character vector.
#' @export
#' @examples
#' escape_special("What the $^&(){}.*|?")
escape_special <- function(strings){
  vals <- c("\\\\", "\\[", "\\]", "\\(", "\\)",
            "\\{", "\\}", "\\^", "\\$","\\*",
            "\\+", "\\?", "\\.", "\\|")
  replace.vals <- paste0("\\\\", vals)
  for(i in seq_along(vals)){
    strings <- gsub(vals[i], replace.vals[i], strings)
  }
  strings
}



#' Run if file does (not) exist, else load file
#'
#' Check if a file exists, and if it doesn't, run an expression, and save the
#' resulting objects to the specified filename. Useful when writing Rmarkdown
#' reports with time-intensive analyses; these can be run once and then saved.
#' @param filename Check whether this file exists or not. If it does exist, load
#' the contents of that file into the global environment.
#' @param expr An expression to be evaluated if the filename does not exist.
#' Multiple expressions should be enclosed in curly braces. Objects created by
#' this expression are stored in a file called \code{filename}.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' ifnofile("example.RData", {
#'   a <- 1
#'   b <- 2
#' })
#' ifnofile("example.RData", {
#'   a <- 1
#'   b <- 2
#' })
ifnofile <- function(filename, expr) {
  if(!file.exists(filename)){
    old_envir <- ls(envir = parent.frame())
    invisible(eval(express_if_not_exists, envir = parent.frame()))
    new_objects <- ls(envir = parent.frame())[which(!ls(envir = parent.frame()) %in% old_envir)]
    save(list = new_objects, file = filename, envir = parent.frame())
  } else {
    load(filename, envir = parent.frame())
  }
}

#' Batch rename files
#'
#' Replace pattern with replacement for all files in the specified folder
#' matching the specified filefilter. Function is run for its side effect of
#' renaming files.
#' @param folder Path to the folder containing files to be renamed.
#' @param filefilter an optional regular expression. Only file names which match
#' the regular expression will be returned.
#' @param pattern regular expression describing the pattern in filenames to
#' be replaced.
#' @param replacement regular expression describing the pattern in filenames to
#' replace pattern with.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' file.create("1 a.txt")
#' file.create("1 b.txt")
#' batchRename(filefilter = "\\d \\w\\.txt", pattern = "\\s", replacement = "")
batchRename <- function(folder = getwd(), filefilter = NULL, pattern, replacement){
  files <- list.files(path = folder, pattern = filefilter, full.names = TRUE)
  sapply(files, FUN=function(eachPath){
    file.rename(from = eachPath,to = gsub(pattern, replacement, eachPath))
  })
}

#' Report formatted number
#'
#' Report a number, rounded to a specific number of decimals (defaults to two),
#' using C-style formats. Intended for RMarkdown reports.
#' @param x Numeric. Value to be reported
#' @param digits Integer. Number of digits to round to.
#' @param equals Logical. Whether to report an equals (or: smaller than) sign.
#' @return An atomic character vector.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' report(.0234)
report <- function(x, digits = 2, equals = TRUE){
  equal_sign <- "= "
  if(x%%1==0){
    outstring <- formatC(x, digits = 0, format = "f")
  } else {
    if(abs(x) <  10^-digits){
      equal_sign <- "< "
      outstring <- 10^-digits
    } else {
      outstring <- formatC(x, digits = digits, format = "f")
    }
  }
  ifelse(equals, paste0(equal_sign, outstring), outstring)
}


#' Pool standard deviations
#'
#' @param ns Vector of sample sizes for the standard deviations to pool.
#' @param sds Vector of standard deviations to pool.
#' @return An atomic numeric vector.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' poolSD(ns = c(64, 33), sds = c(.66, .78))
poolSD <- function(ns, sds){
    sqrt(sum((ns-1)*sds)/(sum(ns)-length(ns)))
}

#' Calculate point-biserial correlation from F-value and sample sizes
#'
#' For a one-way ANOVA, calculate the point-biserial correlation from F-value
#' and sample sizes. The point-biserial correlation is used when the dichotomous
#' variable is a true dichotomy. When the dichotomy is artificial, use the
#' biserial correlation (see \code{\link{rb_from_rpb}}).
#' @param f F-value.
#' @param ne Sample size of the 'experimental condition'.
#' @param nc Sample size of the 'control condition'.
#' @return An atomic numeric vector.
#' @author Caspar J. van Lissa
#' @seealso \code{\link{rb_from_rpb}}
#' @export
#' @examples
#' rpb_from_f(2.44, 54, 65)
rpb_from_f<-function(f, ne, nc){
  return(sqrt(f/(f+ne+nc-2)))
}

#' Calculate biserial correlation from point-biserial correlation
#'
#' Calculate the biserial correlation from point-biserial correlation and sample
#' sizes. The biserial correlation is used when the dichotomous variable is an
#' artificial dichotomy. When the dichotomy is real, use the point-biserial
#' correlation (see \code{\link{rb_from_rpb}}).
#' @param rpb F-value.
#' @param ne Sample size of the 'experimental condition'.
#' @param nc Sample size of the 'control condition'.
#' @return An atomic numeric vector.
#' @author Caspar J. van Lissa
#' @seealso \code{\link{rpb_from_f}}
#' @export
#' @examples
#' rb_from_rpb(rpb_from_f(2.44, 54, 65), 54, 65)
rb_from_rpb<-function(rpb, ne, nc){
  return((rpb*sqrt(ne*nc))/(abs(qnorm(ne/(ne+nc)))*(ne+nc)))
}


#' Convert factor with numeric labels to numeric
#'
#' @param x A factor vector.
#' @return A numeric vector.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' factor_vector <- factor(c("2", "4", "6", "3", "1"))
#' numeric_vector <- as.numeric.factor(factor_vector)
#' factor_vector
#' numeric_vector
as.numeric.factor <- function(x){
  as.numeric(levels(x))[x]
}


#' Satorra-Bentler corrected chi-square test
#'
#' Computes Satorra-Bentler corrected chi-square test.
#' @param Chisq1 Chi square value of model 1.
#' @param df1 Degrees of freedom of model 1.
#' @param scf1 Scale correction factor of model 1.
#' @param Chisq2 Chi square value of model 2.
#' @param df2 Degrees of freedom of model 2.
#' @param scf2 Scale correction factor of model 2.
#' @return Named numeric vector with chi-square value, degrees of freedom, and
#' p-value.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{SB_chisq_Pvalues}} to apply SBChisquare to a table of
#' model chi-square values.
#' @export
#' @examples
#' df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
#' SBChisquare(24, 78, 1.02, 65, 70, 1.28)
SBChisquare <- function(Chisq1, df1, scf1, Chisq2, df2, scf2) {
  if (df1 == df2) {
    warning("Models cannot be nested, DF are equal")
    return(c(
      Chisq = NaN,
      df = NaN,
      p = NaN))
  }

  more_complex <- FALSE
  fit_worse <- FALSE

  if (df2 > df1){ # If DF increased, model became more complex
    more_complex <- TRUE
  }
  delta_df <- abs(df2-df1)

  if (Chisq2-Chisq1 > 0){ # Fit became worse
    fit_worse <- TRUE
  }

  TRd = abs(Chisq1 * scf1 - Chisq2 * scf2) /
      ((df1 * scf1 - df2 * scf2) / (df1 - df2))

  return(c(
    Chisq = round(ifelse((more_complex&fit_worse)|(!more_complex&!fit_worse), 1, -1)*TRd, 2),
    df = ifelse((more_complex&fit_worse)|(!more_complex&!fit_worse), 1, -1)*delta_df,
    p = ifelse((more_complex&fit_worse)|(!more_complex&!fit_worse),
               round(1-pchisq(TRd, delta_df, lower.tail = FALSE), 3),
               round(pchisq(TRd, delta_df, lower.tail = FALSE), 3))
  ))
}

#' Satorra-Bentler corrected chi-square tests for table
#'
#' Computes Satorra-Bentler corrected chi-square test for a table of model chi-
#' square values, degrees of freedom, and scale correction factors.
#' @param tableChi_df_scf A table of model chi-square values, degrees of freedom
#' , and scale correction factors.
#' @return A data.frame of chi-square values, degrees of freedom, and p-values
#' for chi-square difference tests.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{SBChisquare}} for a single chi-square test.
#' @export
#' @examples
#' df <- data.frame(chi2 = c(23, 44, 65), df = c(78, 74, 70), scf = c(1.02, 1.12, 1.28))
#' SB_chisq_Pvalues(df)
SB_chisq_Pvalues<-function(tableChi_df_scf){
  chisquares<-sapply(1:nrow(tableChi_df_scf), function(x){
    if(x==1){return(c(Chisq=NA, df=NA, p=NA))}
    if(x>1){
      return(SBChisquare(tableChi_df_scf[x,1], tableChi_df_scf[x,2], tableChi_df_scf[x,3], tableChi_df_scf[x-1,1], tableChi_df_scf[x-1,2], tableChi_df_scf[x-1,3]))
    }
  })
  return(as.data.frame(t(chisquares)))
}

#' Print Mplus results table formatted for publication
#'
#' Takes an mplusModel object returned by \code{readModels}, and formats it as
#' a publication-ready table.
#' @param mplusModel An mplusModel object, as returned by \code{readModels}.
#' @param parameters A character string corresponding to the name of an element
#' of the $parameters list in \code{mplusModel}. Usually one of
#' \code{c("unstandardized", "stdyx.standardized", "stdy.standardized")}.
#' @param keepCols A character vector of columns to retain from the results
#' section. Standard column names are
#' \code{c("paramHeader", "param", "est", "se", "est_se", "pval")}. Special
#' column names added by \code{printResultsTable} are:
#' \code{c("est_sig", "confint", "label")}. These correspond to 1) the "est"
#' column with significance asterisks appended; 2) a formatted confidence
#' interval; 3) a label, obtained by concatenating the "paramHeader" and "param"
#' columns.
#' @param digits Number of digits to round to when formatting columns.
#' \code{c("unstandardized", "stdyx.standardized", "stdy.standardized")}.
#' @param ... Logical expressions used to filter the rows of results returned.
#' @return A data.frame of formatted Mplus results.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
printResultsTable <- function(mplusModel, parameters = "unstandardized", keepCols = c("label", "est_sig", "confint"), digits = 2, ...){
  args <- list(...)
  results <- mplusModel$parameters[[parameters]]
  value_columns <- c("est", "se", "est_se", "pval", "posterior_sd")
  value_columns <- value_columns[which(value_columns %in% names(results))]
  add_cis <- FALSE
  if(!is.null(mplusModel$parameters[[paste0("ci.", parameters)]])){
    if(dim(results)[1]==dim(mplusModel$parameters[[paste0("ci.", parameters)]])[1]){
      add_cis <- TRUE
      results <- cbind(results, mplusModel$parameters[[paste0("ci.", parameters)]][, c("low2.5", "up2.5")])
    }
  }

  if(!is.null(mplusModel$indirect[[parameters]])){
    overall <- mplusModel$indirect[[parameters]]$overall
    paramHeader <- "Sum.of.indirect"
    param <- paste(overall$outcome, overall$pred, sep = ".")
    overall$pred <- paramHeader
    overall$outcome <- param
    names(overall)[c(1, 2)] <- c("paramHeader", "param")
    if(add_cis){
      overall <- cbind(overall, mplusModel$indirect[[paste0("ci.", parameters)]]$overall[, c("low2.5", "up2.5")])
    }

    results <- rbind(results, overall[, -3])
    specific <- mplusModel$indirect[[parameters]]$specific
    paramHeader <- "Specific.indirect"
    param <- paste(specific$pred, specific$intervening, specific$outcome, sep = ".")
    specific$pred <- paramHeader
    specific$intervening <- param
    names(specific)[c(1, 2)] <- c("paramHeader", "param")
    if(add_cis){
      specific <- cbind(specific, mplusModel$indirect[[paste0("ci.", parameters)]]$specific[, c("low2.5", "up2.5")])
    }
    results <- rbind(results, specific[, -3])
  }

  var_classes <- sapply(results[value_columns], class)
  results[value_columns[which(var_classes == "character")]] <- lapply(results[value_columns[which(var_classes == "character")]], as.numeric)
  results[value_columns[which(var_classes == "factor")]] <- lapply(results[value_columns[which(var_classes == "factor")]], as.numeric.factor)

  constrained_rows <- results$pval == 999

  results$label <- param_label(results)
  if(all(c("est", "pval") %in% names(results))){
    results$est_sig <- est_sig(results, digits)
  }
  results$confint <- conf_int(results, digits)
  filter_columns <- names(args)[which(names(args) %in% names(results))]
  if(!is.null(filter_columns)){
    atomic_filters <- which(sapply(args[filter_columns], length) == 1)
    args[filter_columns][atomic_filters] <- toString(shQuote(args[filter_columns][atomic_filters]))
    num_cols <- which(sapply(results[filter_columns], is.numeric))
    num_filter <- NULL
    if(length(num_cols > 0)){
      num_filter <- sapply(filter_columns[num_cols], function(x){
        paste(paste0("results[['", x, "']] >= ", args[[x]][1], " & ", "results[['", x, "']] <= ", args[[x]][2]), collapse = " & ")
      })

      row_filter <- paste(c(num_filter,
                            sapply(filter_columns[-num_cols], function(x){
                              paste0("results[['", x, "']] %in% ", args[x])
                            })),
                          collapse = " & ")
    } else {
      row_filter <- paste(sapply(filter_columns, function(x){
        paste0("results[['", x, "']] %in% ", args[x])
      }), collapse = " & ")

    }
    results <- results[eval(parse(text = row_filter)), ]
  }
  constrained_rows <- results$pval == 999

  results[, value_columns] <- lapply(results[, value_columns], formatC, digits = digits, format = "f")
  results[constrained_rows, which(names(results) %in% c("se", "pval", "est_se", "confint"))] <- ""
  if(!is.null(keepCols)) results <- results[ , keepCols]
  results
}

#' Add significance asterisks to Mplus output
#'
#' Takes an mplusModel object returned by \code{readModels}, and adds
#' significance asterisks to the "est" column, based on the "pval" column.
#' @param mplusresults An mplusModel object, as returned by \code{readModels}.
#' @param digits Integer. The number of digits to round the "est" column to.
#' @return A character vector of formatted Mplus estimates.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
est_sig <- function(mplusresults, digits){
  paste0(formatC(mplusresults$est, digits = digits, format = "f"), ifelse(mplusresults$pval<.05, "*", ""), ifelse(mplusresults$pval<.01, "*", ""), ifelse(mplusresults$pval<.001, "*", ""))
}

#' Add confidence intervals to Mplus output
#'
#' Takes an mplusModel object returned by \code{readModels}, and constructs
#' nicely formatted confidence intervals. The method depends on the output;
#' if the results section already contains confidence/credible intervals, these
#' are reported. If not, the confidence interval is constructed from the "est"
#' and "se" columns.
#' @param mplusresults An mplusModel object, as returned by \code{readModels}.
#' @param digits Integer. The number of digits to round the confidence interval
#' to.
#' @return A character vector of formatted confidence intervals.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' data <- data.frame(est = c(1.325, 2.432), se = c(.05336, .00325))
#' conf_int(data)
conf_int <- function(mplusresults, digits = 2){
  if("low2.5" %in% names(mplusresults) | "lower_2.5ci" %in% names(mplusresults)){
    if("low2.5" %in% names(mplusresults)){
      message("Used bootstrapped confidence intervals.")
      confint <- paste0("[", formatC(mplusresults$low2.5, digits = digits, format = "f"), ", ", formatC(mplusresults$up2.5, digits = digits, format = "f"), "]")
    } else {
      confint <- paste0("[", formatC(mplusresults$lower_2.5ci, digits = digits, format = "f"), ", ", formatC(mplusresults$upper_2.5ci, digits = digits, format = "f"), "]")
    }
  } else {
    message("Calculated confidence intervals from est and se.")
    confint <- paste0("[", formatC(mplusresults$est-(1.96*mplusresults$se), digits = digits, format = "f"), ", ", formatC(mplusresults$est+(1.96*mplusresults$se), digits = digits, format = "f"), "]")
  }
  gsub("^ \\[", "\\[ ", gsub("([^-]\\d\\.\\d{2})", " \\1", confint))
}

#' Add parameter labels to Mplus output
#'
#' Sometimes a single parameter label is more convenient than the two (or more)
#' columns returned by \code{readModels}. This function constructs parameter
#' labels by concatenating the paramHeader and param columns, or other relevant
#' label columns
#' @param mplusresults An mplusModel object, as returned by \code{readModels}.
#' @return A character vector of parameter labels.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' data <- data.frame(paramHeader = c("F.BY", "F.BY"), param = c("A", "B"))
#' param_label(data)
param_label <- function(mplusresults){
  if(!is.null(mplusresults[["paramHeader"]])&!is.null(mplusresults[["param"]])){
    return(paste(mplusresults$paramHeader, mplusresults$param, sep = "."))
  }
  if(!is.null(mplusresults[["pred"]])&!is.null(mplusresults[["intervening"]])&!is.null(mplusresults[["outcome"]])){
    return(paste("IND", mplusresults$pred, mplusresults$intervening, mplusresults$outcome, sep = "."))
  }
  if(!is.null(mplusresults[["pred"]])&!is.null(mplusresults[["summary"]])&!is.null(mplusresults[["outcome"]])){
    return(paste(gsub("\\s", "\\.", mplusresults$summary), mplusresults$outcome, mplusresults$pred, sep = "."))
  }
}


#' Row-binds tables for publication
#'
#' Converts tables (data.frames, matrices) to character, and row-binds them,
#' inserting a label into the first column for each sub-table.
#' @param table_list A list of tables.
#' @return A table.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @export
#' @examples
#' table_list <- list(
#'   table_f = data.frame(paramHeader = c("F.BY", "F.BY"), param = c("A", "B")),
#'   table_g = data.frame(paramHeader = c("G.BY", "G.BY"), param = c("A", "B")))
#' table_list <- list(
#'   data.frame(paramHeader = c("F.BY", "F.BY"), param = c("A", "B")),
#'   data.frame(paramHeader = c("G.BY", "G.BY"), param = c("A", "B")))
#' rbind_tables(table_list)
rbind_tables <- function(table_list){
  if(length(unique(sapply(table_list, ncol))) > 1) stop("Not all tables have the same number of columns.")
  if(is.null(names(table_list))) names(table_list) <- 1:length(table_list)
  do.call(rbind,
    lapply(names(table_list), function(x){
      rbind(
        c(x, rep("", (ncol(table_list[[x]])-1))), sapply(table_list[[x]], as.character))
    })
  )
}

#' Extract correlation tables from mplusModel
#'
#' Takes an mplusModel object returned by \code{readModels}, and extracts a
#' publication-ready correlation matrix.
#' @param mplusModel An mplusModel object, as returned by \code{readModels}.
#' @param parameters A character string corresponding to the name of an element
#' of the $parameters list in \code{mplusModel}. Usually one of
#' \code{c("unstandardized", "stdyx.standardized", "stdy.standardized")}.
#' @param valueColumn Character. Which column to use to propagate the matrix.
#' Defaults to "est_sig", the estimate with significance asterisks.
#' @param digits Number of digits to round to when formatting values.
#' @return A Matrix or a list of matrices (in case there are between/within
#' correlation matrices).
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
corTable <- function(mplusModel, parameters = "stdyx.standardized", valueColumn = "est_sig", digits = 2){
  correlations <- mplusModel$parameters[[parameters]]
  if("BetweenWithin" %in% names(correlations)){
    cornames <- unique(correlations$BetweenWithin)
    correlations <- lapply(cornames, function(x){
      correlations[correlations$BetweenWithin == x, ]
    })
    names(correlations) <- cornames
  } else {
    correlations <- list(correlations)
  }
  correlations <- lapply(correlations, function(cors){
    cors <- cors[grep("WITH$", cors$paramHeader), ]
    cors$paramHeader <- gsub("\\.WITH", "", cors$paramHeader)
    cors$paramHeader <- substr(cors$paramHeader,  1, 8)
    cors$param <- substr(cors$param,  1, 8)
    if(valueColumn == "est_sig"){
      cors$est_sig <- est_sig(cors, digits = digits)
    }
    if(valueColumn == "confint"){
      cors$confint <- conf_int(cors, digits = digits)
    }

    cors <- cors[ , c("paramHeader", "param", valueColumn)]
    cor_order <- unique(c(rbind(cors$paramHeader, cors$param)))
    names(cors)[3] <- "value"
    cors <- rbind(cors, data.frame(paramHeader = cors$param, param = cors$paramHeader, value = cors$value))
    cors <- rbind(cors, data.frame(paramHeader = unique(cors$paramHeader), param = unique(cors$paramHeader), value = rep(ifelse(valueColumn %in% c("est", "est_sig"), 1, NA), length(unique(cors$paramHeader)))))
    cors <- reshape(cors, v.names = "value", timevar = "paramHeader", idvar = "param", direction = "wide")
    names(cors) <- substr(names(cors), 7, 15)

    cors <- cors[match(cor_order, cors[[1]]), na.omit(match(cor_order, names(cors)))]
    row.names(cors) <- cor_order
    cors[is.na(cors)] <- ""
    cors
  })
  if(length(correlations) == 1){
    correlations <- correlations[[1]]
  }
  correlations
}



#' Tabulate Mplus output (old, redundant version)
#'
#' This function was used for my paper on parenting and adolescents' emotion
#' regulation. It is largely deprecated by printResultsTable, but contains some
#' interesting code for labeling Z-tests for new parameters, and tabulating
#' multi-group output. Should be re-examined and merged with printResultsTable.
#' @param location Character. Location to read the Mplus output files from.
#' @param mplusoutput modelList. A list of mplusModel outputs.
#' @param results Character. Which section of the results to retain; one of
#' \code{c("unstandardized", "stdyx.standardized", "stdy.standardized")}.
#' @param se Logical. Report se or not.
#' @return A list of Mplus results.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @import MplusAutomation
#' @examples
#' #Make me!
mplusToTable<-function(location=getwd(), mplusoutput=NULL, results="stdyx.standardized", se=FALSE){
  require(stringr)
  require(reshape2)
  #Check if required package MplusAutomation is installed, and install it if it is missing
  if(!require(MplusAutomation)){
	install.packages("MplusAutomation")
  library(MplusAutomation)
	}

  #Read all models into "mplusoutput"
  if(is.null(mplusoutput)){
    mplusoutput<-readModels(target=location)#, ...)
  }

  groups<-sapply(mplusoutput, function(x){
    x$summaries$NGroups
  })
  if(length(unique(groups))>1){
    cat(sapply(1:length(mplusoutput), function(x){
      paste(c("Model", mplusoutput[[x]]$summaries$Filename, "has", groups[x], "groups."), collapse=" ")
    }),
    sep="\n"
    )
    stop("Some models had different lengths.")
  }
  groups<-unique(groups)

  #Make list of results and z-tests
  results.list<-lapply(mplusoutput, function(x){x$parameters[[results]][!(x$parameters[[results]]$paramHeader=="New.Additional.Parameters"),]})

  ztest.list<-lapply(mplusoutput, function(x){x$parameters[["unstandardized"]][x$parameters[["unstandardized"]]$paramHeader=="New.Additional.Parameters",]})

  if(all(sapply(ztest.list, function(x){length(x$paramHeader)})==0)){
    ztests<-NULL
    } else {
      label.list<-lapply(mplusoutput, function(x){
        gsub("out$", "inp", x$summaries$Filename)
        paste0(location, "/", gsub("out$", "inp", x$summaries$Filename))
      })

      label.list<-lapply(label.list, function(x.filename){
        tmp<-readLines(x.filename)
        tmp<-tmp[((grep("!Begin label and parameter data", tmp)+1):(grep("!End label and parameter data", tmp)-1))]
        tmp<-data.frame(do.call(rbind, sapply(tmp, function(x){
          str_split(x, "\t")}, USE.NAMES = FALSE)))
        names(tmp)<-c("param", "Label")
        tmp$param<-gsub(".+L(\\d+)", "L\\1", tmp$param)
        tmp$Label<-paste(tmp$param, tmp$Label)
        tmp
      })

      ztest.list<-mapply(FUN=function(labels, results){
        merge(labels, results, by="param", sort=FALSE)
      }, labels=label.list, results=ztest.list, SIMPLIFY = FALSE)

        #Get columns with significance asterisks, standard errors and p-values
        #Drop unused columns
        ztest.list<-lapply(names(ztest.list), function(x) {
          tmp<-data.frame(Filename=x, ztest.list[[x]][,c("Label", "est", "se", "pval")])
          })
        #Add significance asterisks

        ztest.list<-mapply(FUN=function(x, y) {
          "[<-"(x, "est_sig", value = y)},
          x=ztest.list,
          y=lapply(ztest.list, function(x){
            paste0(formatC(x$est, digits = 2, format = "f"), ifelse(x$pval<.05, "*", ""), ifelse(x$pval<.01, "*", ""), ifelse(x$pval<.001, "*", ""))})
          , SIMPLIFY = FALSE)
        ztest.list<-lapply(ztest.list, function(x) {x[,c("Filename" , "Label", "est", "se", "est_sig")]})
        #Merge into one table
        ztests<-do.call("rbind", ztest.list)
      }


  #Add collumn with label to each dataframe
  #results.list<-mapply(function(x, y) "[<-"(results.list[[x]], "Label", value = y) , names(results.list), lapply(names(results.list), function(x){factor(paste0(results.list[[x]]$paramHeader, ".", results.list[[x]]$param), levels=unique(paste0(results.list[[x]]$paramHeader, ".", results.list[[x]]$param)))}), SIMPLIFY = FALSE)

  results.list<-mapply(function(x, y){
    "[<-"(results.list[[x]], "Label", value = y)},
    x = names(results.list),
    y = lapply(names(results.list), function(x){
      factor(paste0(results.list[[x]]$paramHeader, ".", results.list[[x]]$param),
             levels=unique(paste0(results.list[[x]]$paramHeader, ".", results.list[[x]]$param)))}),
    SIMPLIFY = FALSE)



  if(groups==1){
    #Get columns with significance asterisks, or with standard errors and p-values
    if(se){
      #Drop unused columns
      results.list<-lapply(results.list, function(x) {x[,c("Label", "est", "se", "pval")]})

      #Merge into one table
      merged<-do.call("cbind", results.list)
    } else {
      #Drop unused columns
      results.list<-lapply(results.list, function(x) {x[,c("Label", "est", "pval")]})

      #Add significance asterisks
      #
      results.list<-mapply(function(x, y) "[<-"(results.list[[x]], "est_sig", value = y) , names(results.list), lapply(names(results.list), function(x){paste0(formatC(results.list[[x]]$est, digits = 2, format = "f"), ifelse(results.list[[x]]$pval<.05, "*", ""), ifelse(results.list[[x]]$pval<.01, "*", ""), ifelse(results.list[[x]]$pval<.001, "*", ""))}), SIMPLIFY = FALSE)

      #Merge into one table
      merged<-do.call("cbind", results.list)
      merged<-merged[,sort(c(grep("Label", names(merged)), grep("est_sig", names(merged))))]
    }
  } else {
        #Get columns with significance asterisks, or with standard errors and p-values
    if(se){
      #Drop unused columns
      results.list<-lapply(results.list, function(x) {x[,c("Label", "est", "se", "pval", "Group")]})

      #for(res in names(results.list)){
        #x<-results.list[[1]]
        #results.list[[res]]$Label<-mapply(FUN=function(num, label){paste(str_pad(num, 4, pad = "0"), label, sep="_")}, num=c(rep(1:(length(results.list[[1]]$Label)/groups), groups)), label=results.list[[res]]$Label)

          #sapply(1:length(x$Label), function(i){paste(str_pad(i, 4, pad = "0"), x$Label[i], sep="_")})
      #}

      results.list<-lapply(results.list, function(x){melt(x, id.vars=c("Label", "Group"))})
      results.list<-lapply(results.list, function(x){dcast(x, Label~Group+variable)})

      #Merge into one table
      merged<-do.call("cbind", results.list)
    } else {
      #Drop unused columns
      results.list<-lapply(results.list, function(x) {x[,c("Label", "est", "pval", "Group")]})
      #Need for numeric padding should be replaced by making Label an ordered factor
      #for(res in names(results.list)){
      #  results.list[[res]]$Label<-mapply(FUN=function(num, label){paste(str_pad(num, 4, pad = "0"), label, sep="_")}, num=c(rep(1:(length(results.list[[1]]$Label)/groups), groups)), label=results.list[[res]]$Label)
      #}

      #Add significance asterisks
      #
      results.list<-mapply(function(x, y) "[<-"(results.list[[x]], "est_sig", value = y) , names(results.list), lapply(names(results.list), function(x){paste0(formatC(results.list[[x]]$est, digits = 2, format = "f"), ifelse(results.list[[x]]$pval<.05, "*", ""), ifelse(results.list[[x]]$pval<.01, "*", ""), ifelse(results.list[[x]]$pval<.001, "*", ""))}), SIMPLIFY = FALSE)

      results.list<-lapply(results.list, function(x){melt(x[,c("Label", "est_sig", "Group")], id.vars=c("Label", "Group"))})
      results.list<-lapply(results.list, function(x){dcast(x, Label~Group+variable)})

      #Merge into one table
      merged<-do.call("cbind", results.list)
    }

  }
  list(modelparameters=mplusoutput, results=merged, ztests=ztests)
}



#' Constrain Mplus syntax (old, redundant version)
#'
#' This function was used for my paper on parenting and adolescents' emotion
#' regulation. It is intended to constrain non-significantly different
#' parameters across groups. Interesting idea, which could be implemented more
#' systematically using mplusModeler().
#' @param mplusoutput modelList. A list of mplusModel outputs.
#' @param ztests A table of z-tests, returned by mplusToTable.
#' @param location Character. The location of relevant input files.
#' @param outputlocation Character. The location where to write the constrained
#' input files.
#' @return Nothing. Function is run for its side effect of writing files.
#' @author Caspar J. van Lissa
#' @family Mplus functions
#' @seealso \code{\link{readModels}}.
#' @export
#' @examples
#' #Make me!
MplusConstrainModels<-function(mplusoutput=NULL, ztests=NULL, location=getwd(), outputlocation=getwd()){

  significant_ztests<- ztests[grep("\\*", ztests$est_sig),]

  ztest.labels<-lapply(levels(significant_ztests$Filename), function(x){
    labs<-unique(unlist(str_split(gsub("^L\\d+\\s", "", significant_ztests[significant_ztests$Filename==x,]$Label), "-")))
  })

  file.list<-lapply(mplusoutput, function(x){
    gsub("out$", "inp", x$summaries$Filename)
    paste0(location, "/", gsub("out$", "inp", x$summaries$Filename))
  })

  file.list<-lapply(file.list, function(x.filename){
    readLines(x.filename)
  })

  retainlines.constraints<-mapply(FUN=function(file, zlabel){
    modellines<-grep("^model", tolower(file))
    modellines<-modellines[!modellines %in% c(grep("^model (test|constraint)", tolower(file)))]

    zlabel<-unique(paste0("\\(", str_sub(zlabel, 1,-2)))
    zlabel<-zlabel[zlabel!="\\("]

    #Get lines in input that have this constraint
    constraintlines<-unlist(sapply(zlabel, function(zlab){
      #Remove group tag (final character) from constraint labels, and filter out 0 labels
      #zlab<-str_sub(zlab, 1,-2)
      #print(zlab[zlab!=""])
      grep(zlab, file)
    }))
    sort(unique(c(1:(modellines[2]-1), modellines[-1], constraintlines)))
  }, file=file.list, zlabel=ztest.labels)

  constrained.files<-mapply(FUN=function(file, lines, ztestlabs){
    c(file[lines], "", "", "!Constraint labels:", paste(ztestlabs, collapse = " "))
  }, file=file.list, lines=retainlines.constraints, ztestlabs=ztest.labels)

  for(x in names(constrained.files)){
    file.name<-paste0(outputlocation, "/", gsub(".out", "_constrained.inp", x), collapse="")
    writeLines(constrained.files[[x]], file.name)
  }
}

#' summarySE
#'
#' Takes in a data.frame, and returns summary statistics for the specified
#' \code{measurevar} by \code{groupvars}.
#' @param data A data.frame.
#' @param measurevar Character. The name of the variable to summarize.
#' @param groupvars Character vector. Name of the grouping variables to
#'  summarize by.
#' @param na.rm Logical. Remove NA values before summarizing?
#' @param conf.interval Numeric. What percentile to use for confidence interval?
#' @param .drop Logical. Drop unused levels of the grouping variables.
#' @return A data.frame.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' dat <- iris
#' dat$Long <- cut(dat$Petal.Length, 2)
#' summarySE(dat, "Sepal.Length", c("Species", "Long"))
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

#' Apply POMS-coding to data
#'
#' Takes in a data.frame, and applies POMS (proportion of of maximum)-coding to
#' the numeric columns.
#' @param data A data.frame.
#' @return A data.frame.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' data <- data.frame(a = c(1, 2, 2, 4, 1, 6),
#'                    b = c(6, 6, 3, 5, 3, 4),
#'                    c = c("a", "b", "b", "t", "f", "g"))
#' poms(data)
poms <- function(data){
  nums <- sapply(data, is.numeric)
  minscores <- sapply(data[, nums], min, na.rm=TRUE)
  data[, nums] <- sweep(data[, nums], 2, minscores, `-`)
  maxscores <- sapply(data[, nums], max, na.rm=TRUE)
  data[, nums] <- sweep(data[, nums], 2, maxscores, `/`)
  data
}

#' Screen data for nonsense answers
#'
#' Experimental function, intended to flag nonsense responders in survey data.
#' @param data The dataset.
#' @param keys.list A (named) list, with one entry for each questionnaire,
#' containing a character vector with all items belonging to that questionnaire.
#' @param maxpercentidentical Numeric. Maximum allowed percentage of consecutive
#' identical answers within a questionnaire.
#' @param maxquestionnaires Maximum number of questionnaires allowed to exceed
#' maxpercentidentical.
#' @return A list with elements \code{"screen"}: A logical vector of length
#' equal to the number of rows in \code{data}, identifying whether cases are
#' valid according to the thresholds specified; \code{"exceeded_by_person"}:
#' a numeric vector, indicating for how many questionnaires each person exceeded
#' \code{maxpercentidentical}; \code{"exceeded_by_questionnaire"}:
#' a numeric vector, indicating for each questionnaire how many persons exceeded
#' \code{maxpercentidentical}.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' # Makeme!
screenResponses <- function(data = NULL, keys.list = NULL, maxpercentidentical = 1, maxquestionnaires = 2){
  runlengthperquestionnaire <- sapply(keys.list, function(x){
    apply(data[, x], 1, function(x){
      max(rle(x)$lengths)
    })
  })
  repeatedanswers<-sweep(runlengthperquestionnaire, 2, sapply(keys.list, length), `/`)
  exceeded_maxp <- repeatedanswers >= maxpercentidentical
  exceeded_person <- rowSums(exceeded_maxp)
  exceeded_q <- colSums(exceeded_maxp)
  list(screen = !(exceeded_person > maxquestionnaires), exceeded_by_person = exceeded_person, exceeded_by_questionnaire = exceeded_q)
}

#' Make a keys list
#'
#' Attempts to generate a keys.list, as used by functions from the \code{psych}
#' package, and several functions in this \code{motley} package.
#'
#' @param questionnaire_filter Character. A regex filter that identifies
#' variable names that are part of a questionnaire.
#' @param item_filter Character. A regex filter that identifies the incremental
#' part of multiple items belonging to the same questionnaire (e.g., if items
#' are labelled c("a1", "a2", "a3"), then the item_filter would be \code{"\\d$"}
#' ).
#' @param skip_questionnaires Character vector. Names of the questionnaires to
#' skip.
#' @param questionnaire_length Integer. Minimum number of items required to be
#' included in the list of questionnaires.
#' @return A named list with elements corresponding to the questionnaires
#' identified. Each element is a character vector with the variable names
#' belonging to that questionnaire.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' makeKeysList(var_names = c("bfi_1", "bfi_2", "bfi_3", "bfi_4", "bfi_5",
#' "mac_q_j_1", "mac_q_j_2", "mac_q_j_3", "mac_q_j_4", "mac_q_j_5", "mac_q_j_6",
#' "mac_q_j_7", "mac_q_j_8", "mac_q_j_9", "mac_q_j_10", "mac_q_j_11",
#' "mac_q_j_12", "mac_q_j_13", "mac_q_j_14", "mac_q_j_15", "mac_q_j_16",
#' "mac_q_j_17", "mac_q_j_18", "mac_q_j_19", "mac_q_j_20", "mac_q_j_21",
#' "mac_q_r_1", "mac_q_r_2", "mac_q_r_3", "mac_q_r_4", "mac_q_r_5", "mac_q_r_6",
#' "mac_q_r_7", "mac_q_r_8", "mac_q_r_9", "mac_q_r_10", "mac_q_r_11",
#' "mac_q_r_12", "mac_q_r_13", "mac_q_r_14", "mac_q_r_15", "mac_q_r_16",
#' "mac_q_r_17", "mac_q_r_18", "mac_q_r_19", "mac_q_r_20", "mac_q_r_21"),
#' questionnaire_filter = "^[a-zA-Z_]+")
makeKeysList <- function(var_names,
                         questionnaire_filter = "^\\w+\\d+$",
                         item_filter = "\\d+$",
                         skip_questionnaires = NULL,
                         questionnaire_length = 3L){
  items <- grep(questionnaire_filter, var_names, value = TRUE)
  questionnaires <- factor(gsub(item_filter, "", items))
  num_items <- table(questionnaires)
  retain_q <- names(num_items)[which(num_items >= questionnaire_length)]
  retain_q <- retain_q[!retain_q %in% skip_questionnaires]
  outlist <- lapply(retain_q, function(x){items[questionnaires == x]})
  names(outlist) <- retain_q
  outlist
}

#' Calculate partial eta squared for anova
#'
#' Takes in an anova model, and returns partial eta squared for the different
#' predictors.
#' @param x An anova model (from \code{aov()}).
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' model1 <- aov(weight ~ group, data = PlantGrowth)
#' EtaSq(model1)
EtaSq<-function (x){
    anovaResults <- summary.aov(x)[[1]]
    anovaResultsNames <- rownames(anovaResults)
    SS <- anovaResults[,2] #SS effects and residuals
    k <- length(SS) - 1  # Number of factors
    ssResid <- SS[k + 1]  # Sum of Squares Residual
    ssTot <- sum(SS)  # Sum of Squares Total
    SS <- SS[1:k] # takes only the effect SS
    anovaResultsNames <- anovaResultsNames[1:k]
    etaSquared <- SS/ssTot # Should be the same as R^2 values
    partialEtaSquared <- SS/(SS + ssResid)
    res <- cbind(etaSquared, partialEtaSquared)
    colnames(res) <- c("Eta^2", "Partial Eta^2")
    rownames(res) <- anovaResultsNames
    return(res)
}

#' Return pseudo R2s for a logistic model
#'
#' Takes in a logistic regression model, and returns Hosmer & Lemeshow, Cox &
#' Snell, and Nagelkerke's pseudo-R2 estimates
#' @param logModel A logistic regression model (from \code{glm()}).
#' @author Andy Field
#' @export
#' @examples
#' dat <- mtcars
#' model1 <- glm(am ~ mpg, data = dat, family =
#' "binomial")
#' logisticPseudoR2s(model1)
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  out <- c(round(R.l, 3), round(R.cs, 3), round(R.n, 3))
  names(out) <- c("R2 (Hosmer & Lemeshow)", "R2 (Cox & Snell)", "R2 (Nagelkerke)")
  out
}



#' Format numeric columns
#'
#' Formats the numeric columns of a data.frame, to round to a specific number
#' of digits.
#' @param x A data.frame.
#' @param digits The desired number of digits.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' dat <- mtcars
#' format_numeric(dat, 1)
format_numeric <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  lapply(x[numeric_columns], formatC, digits, format = "f")
  x
}

#' Compare logistic regression models
#'
#' Returns a model fit table with significance tests for a list of logistic
#' regression models.
#' @param models A (named) list of logistic regression models
#' (output from \code{lm}))
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' dat <- mtcars
#' model1 <- glm(am ~ mpg, data = dat, family =
#' "binomial")
#' model2 <- glm(am ~ mpg + wt, data = dat, family =
#' "binomial")
#' compareLogisticModels(models = list("Only mpg" = model1,
#'                                     "Mpg and wt" = model2))
compareLogisticModels<-function(models){

  pseudor2s <- data.frame(t(sapply(models, logisticPseudoR2s)))
  names(pseudor2s)<-c("Hosmer.Lemeshow", "Cox.Snell", "Nagelkerke")
  rownames(pseudor2s)<-names(models)

  modelsums<-lapply(models, summary)

  chis<-paste0(round(unlist(lapply(models, function(x) x$null.deviance - x$deviance)), 2), paste0(" ("), paste0(unlist(lapply(models, function(x) x$df.null - x$df.residual)), ")"))

  chisq.prob <-unlist(lapply(models, function(x) 1 - pchisq((x$null.deviance - x$deviance), (x$df.null - x$df.residual))))

  test<-models
  trythis<-NULL

  for(i in 1:length(models)){
    out<-""
    for(r in c(1:length(models))[-i]){
      test_results <- anova(test[[i]], test[[r]], test ="Chisq")
      if(!is.na(test_results[["Pr(>Chi)"]][2])){
        if(test_results$`Pr(>Chi)`[2]<.05){out<-paste0(out, r)}
      }
    }
    trythis<-c(trythis, out)
  }

  modeltable2<-data.frame(unlist(lapply(models, function(x) x$null.deviance - x$deviance)), unlist(lapply(models, function(x) x$df.null - x$df.residual)), unlist(lapply(models, function(x) 1 - pchisq((x$null.deviance - x$deviance), (x$df.null - x$df.residual)))), unlist(lapply(models, function(x) x$aic)), pseudor2s, trythis)
  names(modeltable2)[c(1:4, 8)]<-c("Chi square", "df", "p", "AIC", "Sig. diff from")

  modeltable2
}


#' Conduct a full EFA analysis
#'
#' Conducts a full EFA analysis including assumption checks. Returns the
#' results as a list, writes to .csv files, and prints to the console.
#' @param data Data.frame. Datafile to be factor analyzed.
#' @param name Character. The name of the output (for files to be written etc.)
#' @param nfactors Integer. Hypothesized number of factors; an EFA with this
#' number of factors will be included in the results.
#' @param write_files Logical. Whether to write results to .csv files or not.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' efa_results <- doEFA(bfi[1:500, 1:25], "bfi", 2)
doEFA<-function(data, name, nfactors=NULL, write_files = FALSE){
  library(psych)
  library(ggplot2)
  output<-list()
  output[["cor"]]<-cor(data, use="pairwise.complete.obs")
  output[["N"]]<-nrow(data)
  output[["determinant"]]<-det(output[["cor"]])
  output[["kmo"]]<-KMO(output[["cor"]])
  output[["kmo.range"]]<-round(range(output[["kmo"]]$MSAi),2)
  output[["kmo"]]<-round(output[["kmo"]]$MSA,2)
  output[["bartlett"]]<-cortest.bartlett(output[["cor"]], n=output[["N"]])
  output[["bartlett"]]<-paste0(c(" X2(", output[["bartlett"]]$df, ") = ", round(output[["bartlett"]]$chisq,2), ", p = ", round(output[["bartlett"]]$p.value, 2)), collapse = "")
  capture.output(
  output[["parallel"]]<-fa.parallel(output[["cor"]], n.obs=output[["N"]])$nfact # Parallel
  )
  output[["maxfa"]]<-fa(output[["cor"]], nfactors=(length(names(data))-1),  rotate="oblimin", n.obs=output[["N"]], fm="ml")

  output[["screeplot"]]<-data.frame(1:round(.9*length(names(data))), output[["maxfa"]]$values[1:round(.9*length(names(data)))])
  names(output[["screeplot"]])<-c("Factor", "Eigenvalue")
  output[["screeplot"]]<-ggplot(output[["screeplot"]], aes(x=Factor, y=Eigenvalue)) + geom_line()+ geom_point()+theme_bw()
  #screeMR # Screeplot 2 factors
  output[["Kaiser_criterion"]]<-length(output[["maxfa"]]$values[output[["maxfa"]]$values>1])

  output[["parallel_fa"]] <- fa(output[["cor"]], nfactors=output[["parallel"]],  rotate="oblimin", n.obs=output[["N"]], fm="ml")

  output[["kaiser_fa"]]<-fa(output[["cor"]], nfactors=output[["Kaiser_criterion"]],  rotate="oblimin", n.obs=output[["N"]], fm="ml")
  if(!is.null(nfactors)){
    for(i in nfactors){
      output[[paste0(i, "_fa")]]<-fa(output[["cor"]], nfactors=i,  rotate="oblimin", n.obs=output[["N"]], fm="ml")
      if(write_files) write.csv(output[[paste0(i, "_fa")]]$loadings, file=paste0(c(name, i, "factors fa.csv"), collapse=" "))
    }
  }
  if(write_files){
    write.csv(output[["maxfa"]]$loadings, file=paste0(c(name, (length(names(data))-1), "factors fa.csv"), collapse=" "))
    write.csv(output[["parallel_fa"]]$loadings, file=paste0(c(name, output[["parallel"]], "factors fa.csv"), collapse=" "))
    write.csv(output[["kaiser_fa"]]$loadings, file=paste0(c(name, output[["Kaiser_criterion"]], "factors fa.csv"), collapse=" "))
    ggsave(filename = paste0(name, " screeplot.pdf"),
           plot = output[["screeplot"]],
           width=6,
           height=4,
           units="in")
  }
  cat("\nModel ", name, "\n",
      "N = ", output[["N"]], "\n",
  "Determinant = ", output[["determinant"]], "\n",
  "KMO = ", output[["kmo"]], "\n",
  "KMO range = ", output[["kmo.range"]], "\n",
  "Bartlett's test of sphericity = ", output[["bartlett"]], "\n",
  "Parallel analysis number of factors = ", output[["parallel"]], "\n",
  "Kaiser's kriterion number of factors = ", output[["Kaiser_criterion"]], "\n")
  output
}

#
#Plot distributions
#
plotDistributions<-function(data, scales.list, type="density"){
require(ggplot2)
library(reshape2)
plotlist<-lapply(names(scales.list), function(x){
  #x<-scales.list[1]
#print(unlist(scales.list[x]))
  plotdata<-melt(as.data.frame(data[unlist(scales.list[x])]))
  reldist<-ggplot(plotdata, aes(x=value))
  if(type=="density"){reldist<-reldist + geom_density()
  type<-"Density"}
  if(type=="hist"){reldist<-reldist + geom_histogram()
  type<-"Histogram"}
  if(type=="bar"){reldist<-reldist + geom_bar()
  type<-"Barchart"}
  reldist<-reldist+ facet_wrap(~variable, ncol=5)+ylab("Density")+
    theme_bw() + theme(axis.title.x = element_blank())
  ggsave(file=paste0(c(x," " ,type, ".pdf"), collapse=""), plot=reldist, width=21, height=29, dpi=300, units="cm")
})
}

#
#Reliability analyses
#
doReliability <- function (data, keys.list, name, missing = TRUE, impute = "none",
                           omega = FALSE, omega.factors = NULL, write_files = FALSE,
                           digits = 2)
{
  require(psych)
  require(pastecs)
  scoredatanames <- as.vector(gsub("-", "", unlist(keys.list)))
  scoredatanames <- unique(scoredatanames)
  data <- subset(data, select = (names(data) %in% scoredatanames))
  keys <- make.keys(length(scoredatanames), keys.list = keys.list,
                    item.labels = scoredatanames)
  scores <- scoreItems(keys, data, missing = missing, impute = impute)
  if (omega) {
    if (!is.null(omega.factors)) {
      sapply(1:length(keys.list), function(x) {
        if (omega.factors[x] == "pa") {
          return(fa.parallel(data[keys.list[[x]]], fa = "fa",
                             use = ifelse(missing == TRUE, "pairwise.complete.obs",
                                          "complete.obs"), plot = FALSE)$nfact)
        }
        else {
          return(x)
        }
      })
    }
    else {
      omega.factors <- rep(3, length(keys.list))
    }
    omegas <- unlist(sapply(1:length(keys.list), function(x) {
      omega(data[keys.list[[x]]], nfactors = omega.factors[x])$omega.tot
    }))
  }
  interpret <- function(reliability = NULL) {
    interpretation <- rep("Unacceptable", length(reliability))
    interpretation[reliability >= 0.5] <- "Poor"
    interpretation[reliability >= 0.6] <- "Questionable"
    interpretation[reliability >= 0.7] <- "Acceptable"
    interpretation[reliability >= 0.8] <- "Good"
    interpretation[reliability >= 0.9] <- "Excellent"
    return(interpretation)
  }
  table_descriptives <- data.frame(Subscale = colnames(scores$scores),
                                   Items = unlist(lapply(keys.list, length)), as.matrix(describe(scores$scores))[,
                                                                                                                 c(2, 3, 4, 8, 9)], t(stat.desc(scores$scores, basic = FALSE,
                                                                                                                                                norm = TRUE)[c(8, 9, 10, 11), ]), Alpha = as.vector(scores$alpha),
                                   Interpret.a = interpret(as.vector(scores$alpha)))
  table_descriptives[, sapply(table_descriptives, is.numeric)] <- lapply(table_descriptives[,
                                                                                            sapply(table_descriptives, is.numeric)], formatC, digits = digits,
                                                                         format = "f")
  if (omega) {
    table_descriptives <- data.frame(table_descriptives,
                                     Omega = omegas, Interpret.O = interpret(omegas))
  }
  if (write_files)
    write.csv(table_descriptives, paste0(name, " scale table.csv"),
              row.names = F)

  cordat <- data.frame(scores$scores)
  if(missing == FALSE){
    cordat <- cordat[complete.cases(cordat), ]
  }

  combos <- expand.grid(names(cordat), names(cordat))
  cortab <- matrix(mapply(function(x, y){
    tmp <- cor.test(cordat[[x]], cordat[[y]])
    paste0(formatC(tmp$estimate, digits = digits, format = "f"), ifelse(tmp$p.value < .05, "*", ""), ifelse(tmp$p.value < .01, "*", ""), ifelse(tmp$p.value < .001, "*", ""))
  }, x = combos$Var1, y = combos$Var2), nrow = ncol(cordat))
  colnames(cortab) <- rownames(cortab) <- names(cordat)
  cortab[upper.tri(cortab)] <- ""
  if (write_files)
    write.csv(cortab, paste0(name, " correlation table.csv"))
  return(list(table_descriptives = table_descriptives, Correlations = cortab,
              scores = scores$scores))
}



itemDesc<-function(data, scales.list=NULL, write_files = FALSE){
  require(psych)
  require(pastecs)

  if(is.null(scales.list)){
    scales.list<-list(names(data))
  }

  datafiles<-lapply(scales.list, function(x){data[, names(data) %in% x]})

  tables<-lapply(datafiles, function(x){data.frame(Item=names(as.data.frame(x)), round(describe(as.data.frame(x))[,c(2,3,4,8,9)], 2), t(round(stat.desc(as.data.frame(x), basic = FALSE, norm = TRUE),2)[c(8,9,10,11),]))})

  ##Write list to csv files
  if(write_files){
    for(x in names(tables)){
      write.csv(tables[[x]], paste0(x, " item descriptives.csv"), row.names = F)
    }
  }

  return(tables=tables)
}


#
#Get avarage factor loadings over time for a longitudinal dataset
#
averageloadingsforparceling<-function(df, items, prefix, postfix, waves){
  require(dplyr)
  require(psych)
  factorout<-NULL
  for(i in waves){
    tempdat<-select(df, one_of(datamultiwave(items=items, prefix=prefix, postfix=postfix, waves=i)))
    tempdat<-sapply(tempdat, as.numeric)
    factorout<-cbind(factorout, as.vector(fa(tempdat, use="pairwise.complete.obs")$loadings))
  }
  theloadings<-round(rowMeans(factorout), 2)
  names(theloadings)<-items
  print(sort(theloadings))
}

#
#Get range of reliabilities for longitudinal dataset
#
reliabilityranges <- function(df, items, prefix, postfix, waves){
  require(dplyr)
  require(psych)
  alphas<-numeric(length(waves))
  omegas<-numeric(length(waves))
  for(i in waves){
    itemlist<-paste0(prefix, paste0(i, paste0(postfix, items)))
    reldata<-select(df, one_of(itemlist))
    reldata<-sapply(reldata, as.numeric )
    omeresults<-omega(reldata)
    omegas[i-(waves[1]-1)]<-omeresults$omega.tot
    alphas[i-(waves[1]-1)]<-omeresults$alpha
  }
  outstring<-round(c(range(alphas), range(omegas)),2)
  return(outstring)
}

#
#Get the names of items for multiple waves
#
datamultiwave <- function(items, prefix, postfix, waves){
  require(dplyr)
  itemlist<-NULL
  for(i in waves){
    itemlist<-c(itemlist, paste0(prefix, paste0(i, paste0(postfix, items))))
  }
  return(itemlist)
}

readLsacdata<-function(datalocation="C:/data/Wave 5 GR CD/Confidentialised/STATA/"){
  require(readstata13)
  require(data.table)
  #Read data files
  suppressWarnings(datab1<-data.table(read.dta13(paste0(datalocation, "lsacgrb0.dta"), convert.factors = FALSE)))
  suppressWarnings(datab2<-data.table(read.dta13(paste0(datalocation, "lsacgrb2.dta"), convert.factors = FALSE)))
  suppressWarnings(datab3<-data.table(read.dta13(paste0(datalocation, "lsacgrb4.dta"), convert.factors = FALSE)))
  suppressWarnings(datab4<-data.table(read.dta13(paste0(datalocation, "lsacgrb6.dta"), convert.factors = FALSE)))
  suppressWarnings(datab5<-data.table(read.dta13(paste0(datalocation, "lsacgrb8.dta"), convert.factors = FALSE)))
  suppressWarnings(datak3<-data.table(read.dta13(paste0(datalocation, "lsacgrk4.dta"), convert.factors = FALSE)))
  suppressWarnings(datak4<-data.table(read.dta13(paste0(datalocation, "lsacgrk6.dta"), convert.factors = FALSE)))
  suppressWarnings(datak5<-data.table(read.dta13(paste0(datalocation, "lsacgrk8.dta"), convert.factors = FALSE)))
  suppressWarnings(datak6<-data.table(read.dta13(paste0(datalocation, "lsacgrk10.dta"), convert.factors = FALSE)))
  suppressWarnings(datak7<-data.table(read.dta13(paste0(datalocation, "lsacgrk12.dta"), convert.factors = FALSE)))
  return(list(datab1=datab1, datab2=datab2, datab3=datab3, datab4=datab4, datab5=datab5, datak3=datak3, datak4=datak4, datak5=datak5, datak6=datak6, datak7=datak7))
}

getLsacdata<-function(lsacfiles=NULL, invar=NULL, variant=NULL, check.weirdvars=FALSE, remove.duplicates=TRUE){
  require(data.table)
  #test harness
  #lsacfiles=datafiles
  #variant=c("[a-g]f0[6]m[12]","[a-g]f0[7]m[12]")
  #variant=NULL
#describe(datak4$dscagem)$mean/12
#datafiles<-c("datab1", "datab2", "datab3", "datab4", "datab5", "datak3", "datak4", "datak5", "datak6", "datak7")
  #add ID number, cohort and child sex to invar variables
  invar<-c("hicid", "cohort", "zf02m1", invar)

  #Get all names of all data files
  allnames<-unlist(lapply(lsacfiles, names))
  variantnames<-NULL

  #Get unique variant names that exist in allnames
  variantnames<-unique(unlist(lapply(variant, function(x){grep(x, allnames, value=TRUE)})))

  #Select variables from all datasets and merge them
  #data<-vector(mode="list", length=length(lsacfiles))
  data<-lapply(lsacfiles, function(datafile){
      datafile[, .SD, .SDcols=which(names(datafile) %in% c(invar, variantnames))]
    })

#   datab <- data[[1]][data[[2]], on=c("hicid", "cohort", "zf02m1")][data[[3]], on=c("hicid", "cohort", "zf02m1")][data[[4]], on=c("hicid", "cohort", "zf02m1")][data[[5]], on=c("hicid", "cohort", "zf02m1")]
#  datab[, grep("^i\\.", names(datab), value=TRUE) := NULL]

 datab<-data[[1]]
  for(i in 2:5) {
    datab <- merge(datab,
            data[[i]][, .SD, .SDcols=names(data[[i]])[!(names(data[[i]]) %in% invar[-c(1:3)])]], by = c("hicid", "cohort", "zf02m1"),
            all = TRUE)
  }

# datak <- data[[6]][data[[7]], on=c("hicid", "cohort", "zf02m1")][data[[8]], on=c("hicid", "cohort", "zf02m1")][data[[9]], on=c("hicid", "cohort", "zf02m1")][data[[10]], on=c("hicid", "cohort", "zf02m1")]
# datak[, grep("^i\\.", names(datak), value=TRUE) := NULL]

  datak<-data[[6]]
  for(i in 7:10) {
    datak <- merge(datak,
            data[[i]][, .SD, .SDcols=names(data[[i]])[!(names(data[[i]]) %in% invar[-c(1:3)])]], by = c("hicid", "cohort", "zf02m1"),
            all = TRUE)
  }
  #length(unique(unlist(lapply(lsacfiles, function(x){paste(c(x$hicid,x$cohort,x$zf02m1), collapse="")}))))

  duplicates<-c(datab$hicid, datak$hicid)[which(duplicated(c(datab$hicid, datak$hicid)))]
  #table(datab$zf02m1)
  #tmp<-relationship.cp[relationship.cp$hicid %in% duplicates,]

  #Check if any variables have different types in the two datasets
  duplicatenames<-c(names(datab), names(datak))[duplicated(c(names(datab), names(datak)))]
  #duplicatenames<-duplicatenames
  ktypes<-sapply(datak[,.SD, .SDcols=duplicatenames], class)
  btypes<-sapply(datab[,.SD, .SDcols=duplicatenames], class)
  #btypes<-sapply(datab[,names(datab) %in% duplicatenames], class)
  samediff<-ktypes==btypes

  if(check.weirdvars){
    weirdvariables<-NULL
    if(!all(samediff)){
      for(i in duplicatenames[samediff==FALSE]){
        text<-paste(c("Variable ", i, " has class ", class(datab[, i]), " in the B cohort and class ", class(datak[, i]), " in the K cohort"), collapse="")
        warning(text, call. = FALSE)
        weirdvariables<-c(weirdvariables,i)
      }
    }

    #Check if any variables have different factor levels in the two datasets
     for(i in names(ktypes)[ktypes=="factor"]){
        if(!all(levels(datab[,i]) %in% levels(datak[,i]))){
          text<-paste0("The following factors have different levels in the B and K cohorts:", i)
          warning(text, call. = FALSE)
          weirdvariables<-c(weirdvariables,i)
        }
      }


    #Check if any variables have different range
    for(i in names(ktypes)[ktypes=="integer"|ktypes=="numeric"]){
      if(!all(min(datak[,i], na.rm=TRUE)==min(datab[,i], na.rm=TRUE)&
              max(datak[,i], na.rm=TRUE)==max(datab[,i], na.rm=TRUE))){
        text<-paste(c("Variable ", i, " has range ", min(datab[, i], na.rm=TRUE),"-", max(datab[, i], na.rm=TRUE), " in the B cohort and range ", min(datak[, i], na.rm=TRUE), "-", max(datak[, i], na.rm=TRUE)," in the K cohort"), collapse="")
        warning(text, call. = FALSE)
        weirdvariables<-c(weirdvariables,i)
      }
    }

    names(datab)[names(datab) %in% weirdvariables]<-paste0(names(datab)[names(datab) %in% weirdvariables], "_bcohort")
    names(datak)[names(datak) %in% weirdvariables]<-paste0(names(datak)[names(datak) %in% weirdvariables], "_kcohort")
  }

  #library(dplyr)
  combineddat<- rbindlist(list(datab, datak), fill=TRUE)
  #Merge duplicates
  if(remove.duplicates){
  source<-which(duplicated(combineddat$hicid))
  destination<-which(combineddat$hicid %in% duplicates)
  destination<-destination[-which(destination %in% source)]
  combineddat[destination[c(2,5,6)], zf02m1:=NA]
  combineddat[destination, .SD]
  for(i in 1:length(destination)){
    miss.values<-names(combineddat)[which(is.na(combineddat[destination[i], ]))]
    if(!length(miss.values)==0){
      set(combineddat, i=destination[i], j=(miss.values), value=combineddat[source[i], .SD, .SDcols=miss.values])
    }
  }
  combineddat<-combineddat[-source,]
  }
  combineddat[combineddat<0]<-NA
  return(combineddat)
}


#
#Turn wide dataset into long dataset for MPlus measurement invariance test
#
exportinvardata<-function(file_name, df, id_var, items, prefix, postfix, waves){
  require(MplusAutomation)
  require(dplyr)
  variables<-datamultiwave(items, prefix, postfix, waves)
  invardata<-select(df, one_of(id_var), one_of(variables))
  namelist<-NULL
  for(i in waves) namelist<-c(namelist, paste0(paste0(paste0(rep("item", length(items)), 1:length(items)), "_"), i))
  names(invardata)[2:length(names(invardata))]<-namelist
  invardata[,2:length(names(invardata))] <- lapply(invardata[,2:length(names(invardata)),drop=FALSE],as.numeric)
  invardata_long<-reshape(invardata, varying=namelist, direction="long", idvar=id_var, sep="_")
  prepareMplusData(invardata_long, filename=file_name, inpfile=T)
}
