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

#' Report rounded value
#'
#' Report a number, rounded to a specific number of decimals (defaults to two),
#' using C-style formats.
#' @param value Value to be rounded.
#' @param digits Number of digits to round to.
#' @return An atomic character vector.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' p <- .0234
#' report(p)
report <- function(value, digits = 2){
  compnum <- as.numeric(paste(c(".", rep("0", digits-1), "1"), collapse = ""))
  ifelse(p < compnum, paste("<", compnum), paste("=", formatC(p, digits = digits, format = "f")))
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

### REMOVE, deprecated by dplyr
merge.with.order <- function(...){
  stop("merge.with.order is deprecated. Use dplyr function left_join()")
}

#' COnvert factor with numeric labels to numeric
#'
#' @param x A factor vector.
#' @return A numeric vector.
#' @author Caspar J. van Lissa
#' @export
#' @examples
#' char_vector <- c("2", "4", "6", "3", "1")
#' factor_vector <- factor(char_vector)
#' numeric_vector <- as.numeric.factor(factor_vector)
#' numeric_vector
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


#
#Mplus functions
#

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
#' # Make me!
SBChisquare <- function(Chisq1, df1, scf1, Chisq2, df2, scf2) {
  if (df1 > df2) {
    TRd = (Chisq1 * scf1 - Chisq2 * scf2) /
      ((df1 * scf1 - df2 * scf2) / (df1 - df2))
  }
  if (df2 > df1) {
    TRd <-
      (Chisq2 * scf2 - Chisq1 * scf1) / ((df2 * scf2 - df1 * scf1) / (df2 - df1))
  }
  if (df1 == df2) {
    stop("Models cannot be nested, DF are equal")
  }
  df <- abs(df1 - df2)
  return(c(
    Chisq = round(TRd, 2),
    df = round(df),
    p = round(pchisq(TRd, df, lower.tail = FALSE), 3)
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


mplusToTable<-function(location=getwd(), mplusoutput=NULL, results="stdyx.standardized", se=FALSE, ...){
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
return(list(modelparameters=mplusoutput, results=merged, ztests=ztests))
}



#### Automatically adjust mplus syntax
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

###


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
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

#
#POMS coding
#
poms<-function(data){
nums <- sapply(data, is.numeric)
minscores<-as.numeric(apply(data[,nums], 2, function(x) min(x, na.rm=TRUE)))
data[,nums]<-sweep(data[,nums], 2, minscores, `-`)
maxscores<-as.numeric(apply(data[,nums], 2, function(x) max(x, na.rm=TRUE)))
data[,nums]<-sweep(data[,nums], 2, maxscores, `/`)
return(data)
}

#
#Screen data for nonsense answers
#
screenResponses<-function(data, exclude=NULL, maxpercentidentical=1, maxquestionnaires=2, questionnaire_regex="\\w+\\d+", questionnairelength=3){
#Test harness:
 # exclude="marital"
 # maxpercentidentical = 1
 # maxquestionnaires = 2
 # questionnairelength=2
 # questionnaire_regex="\\w+\\d+"
 # questionnairelength=3

#Unique names of questionnaires
questionnaires<-names(data)[grep(questionnaire_regex, names(data))]
questionnaires<-gsub("\\d", "", questionnaires)
print("Original questionnaires:")
print(questionnaires<-as.data.frame(table(questionnaires)))
questionnaires<-questionnaires[questionnaires$Freq>=questionnairelength,]

questionnaires<-questionnaires[!questionnaires$questionnaires %in% exclude,]
#questionnaires<-as.vector(questionnaires$questionnaires)

#total length of each questionnaire
#lengthofeach<-unlist(lapply(questionnaires, function(x){length(grep(paste0("^",x), names(data)))}))

#Run length per questionnaire
runlengthperquestionnaire<-as.data.frame(lapply(as.vector(questionnaires$questionnaires), function(x){
  dt<-dplyr::select(data, starts_with(x))
  unlist(lapply(apply(dt, 1, rle), function(x){max(x$length)}))
}))
names(runlengthperquestionnaire)<-questionnaires$questionnaires

#Percentage repeated
repeatedanswers<-sweep(runlengthperquestionnaire, 2, questionnaires$Freq, `/`)

repeatedanswers<-ifelse((repeatedanswers>=maxpercentidentical), 1, 0)
repperperson<-apply(repeatedanswers, 1, sum)
print("Questionnaires used:")
questionnaires$Repeated<-apply(repeatedanswers, 2, sum)
print(questionnaires)

plotrep<-table(repperperson)

#Flag cases with a certain number of scales with all identical answers
toomuchrepetition<-as.factor(ifelse((repperperson>maxquestionnaires), "Flagged", "Valid"))
tabletoomuchrep<-table(toomuchrepetition)
#names(tabletoomuchrep)<-c("Valid", "Flagged")

print(tabletoomuchrep)
return(list(screen=toomuchrepetition, repPerPerson=repperperson, repetitionplot=plotrep, number.flagged=tabletoomuchrep))
}


###########################################
# Calculate partial eta squared for anova #
###########################################
EtaSq<-function (x)
{
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

#
#Return pseudo R2s for logistic models
#
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  list(c("R2 (Hosmer & Lemeshow)", round(R.l, 3)),
       c("R2 (Cox & Snell)", round(R.cs, 3)),
       c("R2 (Nagelkerke)", round(R.n, 3)))
}



#
#Round all numeric variables in data frame X
#
round_numeric <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#
#Make table for comparing logistic regression models
#
compareLogisticModels<-function(model){

pseudor2s<-data.frame(matrix(unlist(lapply(model, logisticPseudoR2s)), nrow=length(model), byrow=T))[,c(2,4,6)]
colnames(pseudor2s)<-c("Hosmer & Lemeshow", "Cox & Snell", "Nagelkerke")
rownames(pseudor2s)<-names(model)

modelsums<-lapply(model, summary)

chis<-paste0(round(unlist(lapply(model, function(x) x$null.deviance - x$deviance)), 2), paste0(" ("), paste0(unlist(lapply(model, function(x) x$df.null - x$df.residual)), ")"))

chisq.prob <-unlist(lapply(model, function(x) 1 - pchisq((x$null.deviance - x$deviance), (x$df.null - x$df.residual))))


test<-model
trythis<-NULL

for(i in 1:length(model)){
  out<-""
  for(r in c(1:length(model))[-i]){
    if(anova(test[[i]], test[[r]], test ="Chisq")$`Pr(>Chi)`[2]<.05){out<-paste0(out, r)}
  }
  trythis<-c(trythis, out)
}

modeltable2<-data.frame(unlist(lapply(model, function(x) x$null.deviance - x$deviance)), unlist(lapply(model, function(x) x$df.null - x$df.residual)), unlist(lapply(model, function(x) 1 - pchisq((x$null.deviance - x$deviance), (x$df.null - x$df.residual)))), unlist(lapply(model, function(x) x$aic)), pseudor2s, trythis)
names(modeltable2)[c(1:4, 8)]<-c("Chi square", "df", "p", "AIC", "Sig. diff from")

return(modeltable2)
}


#
# Factor analysis
#
doEFA<-function(data, name, nfactors=NULL, write_files = FALSE){
  require(psych)
  require(ggplot2)
  require(dplyr)
  output<-NULL
  output[["cor"]]<-cor(data, use="pairwise.complete.obs")
  output[["N"]]<-nrow(data)
  output[["determinant"]]<-det(output[["cor"]])
  output[["kmo"]]<-KMO(output[["cor"]])
  output[["kmo.range"]]<-round(range(output[["kmo"]]$MSAi),2)
  output[["kmo"]]<-round(output[["kmo"]]$MSA,2)
  output[["bartlett"]]<-cortest.bartlett(output[["cor"]], n=output[["N"]])
  output[["bartlett"]]<-paste0(c(" X2(", output[["bartlett"]]$df, ") = ", round(output[["bartlett"]]$chisq,2), ", p = ", round(output[["bartlett"]]$p.value, 2)), collapse = "")

  output[["parallel"]]<-fa.parallel(output[["cor"]], n.obs=output[["N"]])$nfact # Parallel

  output[["maxfa"]]<-fa(output[["cor"]], nfactors=(length(names(data))-1),  rotate="oblimin", n.obs=output[["N"]], fm="ml")

  output[["screeplot"]]<-data.frame(1:round(.9*length(names(data))), output[["maxfa"]]$values[1:round(.9*length(names(data)))])
  names(output[["screeplot"]])<-c("Factor", "Eigenvalue")
  output[["screeplot"]]<-ggplot(output[["screeplot"]], aes(x=Factor, y=Eigenvalue)) + geom_line()+ geom_point()+theme_bw()
  #screeMR # Screeplot 2 factors
  output[["Kaiser_criterion"]]<-length(output[["maxfa"]]$values[output[["maxfa"]]$values>1])

  output[["parallel_fa"]]<-fa(output[["cor"]], nfactors=output[["parallel"]],  rotate="oblimin", n.obs=output[["N"]], fm="ml")

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
    ggwrite_files(filename=paste0(name, " screeplot.pdf"), output[["screeplot"]], width=200, height=290, units="mm")
  }
  cat("Model ", name)
  cat("N = ", output[["N"]])
  cat("Determinant = ", output[["determinant"]])
  cat("KMO = ", output[["kmo"]])
  cat("KMO range = ", output[["kmo.range"]])
  cat("Bartlett's test of sphericity = ", output[["bartlett"]])
  cat("Parallel analysis number of factors = ", output[["parallel"]])
  cat("Kaiser's kriterion number of factors = ", output[["Kaiser_criterion"]])
  return(output)
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
doReliability<-function(data, keys.list, name, missing=TRUE, impute="none", omega=FALSE, omega.factors=NULL, write_files = FALSE){
  require(psych)
  require(pastecs)


  scoredatanames<-as.vector(gsub("-", "", unlist(keys.list)))
  scoredatanames<-unique(scoredatanames)

  data<-subset(data, select=(names(data) %in% scoredatanames))

  keys<-make.keys(length(scoredatanames), keys.list=keys.list,item.labels=scoredatanames)

  scores <- scoreItems(keys,data, missing=missing, impute=impute)

  if(omega){
    if(!is.null(omega.factors)){
      sapply(1:length(keys.list), function(x){
        if(omega.factors[x]=="pa"){
          return(fa.parallel(data[keys.list[[x]]], fa="fa",
                             use=ifelse(missing==TRUE, "pairwise.complete.obs", "complete.obs"),
                             plot=FALSE)$nfact)
        } else{
          return(x)
        }
      })
    } else {
      omega.factors<-rep(3, length(keys.list))
    }
    omegas<-unlist(sapply(1:length(keys.list), function(x){
      omega(data[keys.list[[x]]], nfactors=omega.factors[x])$omega.tot
    }))
  }

  interpret<-function(reliability=NULL){
    interpretation<-rep("Unacceptable", length(reliability))
    interpretation[reliability>=.5]<-"Poor"
    interpretation[reliability>=.6]<-"Questionable"
    interpretation[reliability>=.7]<-"Acceptable"
    interpretation[reliability>=.8]<-"Good"
    interpretation[reliability>=.9]<-"Excellent"
    return(interpretation)
  }


  table_descriptives<-data.frame(Subscale=colnames(scores$scores), Items=unlist(lapply(keys.list, length)) , round(describe(scores$scores)[,c(2,3,4,8,9)], 2), t(round(stat.desc(scores$scores, basic = FALSE, norm = TRUE),2)[c(8,9,10,11),]), Alpha=round(as.vector(scores$alpha), 2), Interpret.a=interpret(round(as.vector(scores$alpha), 2)))

  if(omega){
    table_descriptives<-data.frame(table_descriptives, Omega=round(omegas, 2), Interpret.O=interpret(round(omegas, 2)))
  }

  if(write_files) write.csv(table_descriptives, paste0(name, " scale table.csv"), row.names = F)

  cortab<-as.data.frame(round(cor(scores$scores, use=ifelse(missing==TRUE, "pairwise.complete.obs", "complete.obs")), 2))
  cortab[upper.tri(cortab)]<-""

  if(write_files) write.csv(cortab, paste0(name, " correlation table.csv"))
  return(list("table_descriptives"=table_descriptives, "Correlations"=cortab, "scores"=scores$scores))
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
#Incremental predictive validity
#
predictiveValidity<-function(data, predictors1, predictors2, dvs, namePred1, namePred2){
  require(dplyr)
  predictor1models<-lapply(dvs, function(x){
    dt<-dplyr::select(data, one_of(predictors1))
    lm(data[,x]~., data=dt)})

  names(predictor1models)<-dvs

  predictor2models<-lapply(dvs, function(x){
    dt<-dplyr::select(data, one_of(predictors2))
    lm(data[,x]~., data=dt)})

  names(predictor2models)<-dvs

  combinedmodels<-lapply(dvs, function(x){
    dt<-dplyr::select(data, one_of(predictors1), one_of(predictors2))
    lm(data[,x]~., data=dt)})

  names(combinedmodels)<-dvs

  deltas1vcombined<-lapply(dvs, function(x){anova(predictor1models[[x]], combinedmodels[[x]])})
  names(deltas1vcombined)<-dvs
  deltas2vcombined<-lapply(dvs, function(x){anova(predictor2models[[x]], combinedmodels[[x]])})
  names(deltas2vcombined)<-dvs

  rsquaredtable<-data.frame(unlist(lapply(predictor1models, function(x){summary(x)$r.squared})), unlist(lapply(deltas1vcombined, function(x){x[2,6]})), unlist(lapply(predictor2models, function(x){summary(x)$r.squared})), unlist(lapply(deltas2vcombined, function(x){x[2,6]})), unlist(lapply(combinedmodels, function(x){summary(x)$r.squared})))
  names(rsquaredtable)<-c(paste0("R2 ",namePred1), paste0("p-value adding ", namePred2),  paste0("R2 ", namePred2), paste0("p-value adding ", namePred1), "R2 combined")

  rsquaredtable$AddPredictors2<-apply(rsquaredtable, 1, function(x){x[5]-x[1]})
  rsquaredtable$AddPredictors1<-apply(rsquaredtable, 1, function(x){x[5]-x[3]})

  rsquaredtable<-round(rsquaredtable[,c(1,6,2,3,7,4)], 2)
  names(rsquaredtable)[c(2,5)]<-c(paste0("Add ", namePred2), paste0("Add ", namePred1))

  write.csv(rsquaredtable, file=paste0(c(namePred1, "vs", namePred2, "Incremental predictive validity.csv"), collapse = " "))
  print(paste0(c(namePred1, "vs", namePred2, "Incremental predictive validity:"), collapse = " "))
  print(rsquaredtable)
  return(rsquaredtable)
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
