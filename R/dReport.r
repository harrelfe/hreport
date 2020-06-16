#' Descriptive Statistics Report
#'
#' Generate graphics and LaTeX with descriptive statistics
#' 
#' \code{dReport} generates multi-panel charts, separately for categorical analysis variables and continuous ones.  The Hmisc \code{summaryP} function and its plot method are used for categorical variables, and \code{bpplotM} is used to make extended box plots for continuous ones unless \code{what='byx'}.   Stratification is by treatment or other variables.  The user must have defined a LaTeX macro \code{\\eboxpopup} (which may be defined to do nothing) with one argument.  This macro is called with argument \code{extended box plot} whenever that phrase appears in the legend, so that a \code{PDF} popup may be generated to show the prototype.  See the example in \code{report.Rnw} in the \code{tests} directory.  Similarly a popup macro \code{\\qintpopup} must be defined, which generates a tooltip for the phrase \code{quantile intervals}.
#'
#' @param formula a formula accepted by the \code{bpplotM} or \code{summaryP} functions.  \code{formula} must have an \code{id(subjectidvariable)} term if there are repeated measures, in order to get correct subject counts as \code{nobs}.
#' @param groups a superpositioning variable, usually treatment, for categorical charts.  For continuous analysis variables, \code{groups} becomes the \code{y}-axis stratification variable.  This is a single character string.
#' @param what \code{"hist"} (the default) or \code{"xy"} for continuous analysis variables, or \code{"proportions"} (or shorter) for categorical ones.  Instead, specifying \code{what="byx"} results in an array of quantile intervals for continuous \code{y}, Wilson confidence intervals for proportions when \code{y} is binary, or means and parametric confidence limits when \code{y} is not continuous but is not binary.  If \code{what} is omitted or \code{what="byx"}, actions will be inferred from the most continuous variable listed in \code{formula}.  When \code{fun} is given, different behavior results (see below).
#' @param study character string identifying the study; used in multi-study reports or where distinct patient strata are analyzed separately.  Used to fetch the study-specific metadata stored by \code{\link{sethreportOption}}.  Single study reports just use \code{study=' '}.
#' @param byx.type set to \code{"quantiles"} to show vertical quantile intervals of \code{y} at each \code{x} for when \code{what="byx"} and the \code{y} variable is continuous numeric, or set \code{byx.type="violin"} (the default) to plot half-violin plots at each \code{x}.
#' @param violinbox set to \code{TRUE} to add violin plots to box plots
#' @param violinbox.opts a list to pass to \code{panel.violin}
#' @param summaryPsort set to \code{TRUE} to sort categories in descending order of frequencies
#' @param exclude1 logical used for \code{latex} methods when \code{summaryM} or \code{summaryP} are called by \code{dReport}, or for plot methods for \code{summaryP}.  The default is \code{TRUE} to cause the most frequent level of any two-level categorical variable to not be used as a separate category in the graphic or table.  See \code{\link[Hmisc]{summaryM}}.
#' @param stable set to \code{FALSE} to suppress creation of backup supplemental tables for graphics
#' @param fun a function that takes individual response variables (which may be matrices, as in \code{\link[survival]{Surv}} objects) and creates one or more summary statistics that will be computed while the resulting data frame is being collapsed to one row per condition.  Dot charts are drawn when \code{fun} is given.
#' @param data data frame
#' @param subset a subsetting epression for the entire analysis
#' @param na.action a NA handling function for data frames, default is \code{na.retain}
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used
#' @param tail optional character string.  Specifies final text in the figure caption, e.g., what might have been put in a footnote in an ordinary text page.  This appears just before any needles.
#' @param continuous the minimum number of numeric values a variable must have in order to be considered continuous.  Also passed to \code{summaryM}.
#' @param h numeric.  Height of plot, in inches
#' @param w numeric.  Width of plot
#' @param \dots. Passed to \code{summaryP} or \code{bpplotM}
#' @param sopts list specifying extra arguments to pass to \code{histboxpM}, \code{ecdfpM}, \code{summaryP}, or \code{summaryS}
#' @param popts list specifying extra arguments to pass to a plot method.
#' @export
#' @importFrom Formula Formula
#' @importFrom grDevices adjustcolor
#' @examples
#' # See test.Rnw in tests directory

dReport <-
  function(formula, groups=NULL,
           what=c('hist', 'ecdf', 'proportions', 'xy', 'byx'),
           study=' ',
           byx.type=c('hist', 'quantiles', 'violin'),
           violinbox=TRUE,
           violinbox.opts=list(col=adjustcolor('blue', alpha.f=.25),
             border=FALSE),
           summaryPsort=FALSE, exclude1=TRUE,
           stable=TRUE,
           fun=NULL, data=NULL, subset=NULL, na.action=na.retain,
           head=NULL, tail=NULL,
           continuous=10, h=NULL, w=NULL,
           sopts=NULL, popts=NULL)
{
  mwhat    <- missing(what)
  what     <- match.arg(what)
  byx.type <- match.arg(byx.type)
  tvar     <- gethreportOption('tx.var', study=study)

  options(grType='plotly')
  popts <- c(popts, list(colors=gethreportOption('tx.col', study=study)))

  margpres <- length(data) && '.marginal.' %in% names(data)

  ## Find the number of observations in the Y variables grouped
  ## by the value found in gethreportOption('tx.var')
  Nobs <- nobsY(formula, group=tvar,
                data=data, subset=subset, na.action=na.action)
  formula.no.id <- Nobs$formula   ## removes id()
  form <- Formula::Formula(formula)
  environment(form) <- new.env(parent = environment(form))
  en <- environment(form)
  assign(envir = en, 'id', function(x) x)

  ## if argument 'subset' is present then
  ## Create a dataset that is a subset of the dataset 'data' using the argument 'subset'.
  ## Otherwise Create a dataset from the dataset 'data' using the formula 'form' from the
  ## argument formula
  Y <- if(length(subset)) model.frame(form, data=data, subset=subset,
                                      na.action=na.action)
       else model.frame(form, data=data, na.action=na.action)
  ## Split the dataset 'Y' in the left and right hand sides of the
  ## formula
  X <- model.part(form, data=Y, rhs=1)
  Y <- model.part(form, data=Y, lhs=1)

  ## Extract the terms of the right hand side including
  ## the id column declared using the 'id' functuion as special.
  rhs <- terms(form, rhs=1, specials='id')
  sr  <- attr(rhs, 'specials')
  ## specials counts from lhs variables
  wid <- sr$id
  if(length(wid)) wid <- wid - ncol(Y)

  ## If argument 'groups' (Defines name of grouping term in formula) has
  ## length then get the levels of the grouping term.
  glevels <- if(length(groups)) levels(X[[groups]])
  ## If there are more the 3 levels in variable 'glevels' then
  ## set variable 'manygroups' to 'TRUE'
  manygroups <- length(glevels) > 3
  nstrata <- 1
  
  ## If missing argument 'what' assign value of 'what' based
  ## on other arguments.
  if(mwhat) {
    ## If 'fun' is present 'what' is set to value 'xy'.
    ## Otherwise if the first element of object 'Y' is a character, a
    ## factor or it inherits 'ynbind' then argument 'what' is set to
    ## value 'proportions'.
    ## Otherwise argument 'what' remains 'hist' per match.arg
    if(length(fun)) what <- 'xy'
    else {
      y <- Y[[1]]
      if(is.character(y) || is.factor(y) || inherits(y, 'ynbind'))
        what <- 'proportions'
    }
  }
  
  ## Extract Labels from the right hand side of the formula using
  ## Hmisc function 'label'
  labs      <- sapply(X, label)
  ## If id() column exists then remove that label from the vector of label values
  ## the right hand side of the formula
  if(length(wid)) labs <- labs[- wid]
  ## Set replace blank labels in variable 'labs' to the name of the term in
  ## variable 'X'
  stratlabs <- ifelse(labs == '',
                      if(length(wid)) names(X)[-wid] else names(X), labs)
  ## Extract Labels from the left hand side of the formula with the Hmisc
  ## function 'label'
  ylabs     <- sapply(Y, label)
  ## if the labels in variable 'ylabs' are blank replace with the term name
  ## in variable 'Y'
  ylabs     <- ifelse(ylabs == '', names(Y), ylabs)

  ## paste together a comma seperated lexical list
  past <- function(x) {
    l <- length(x)
    if(l < 2) x
    else if(l == 2) paste(x, collapse=' and ')
    else paste(paste(x[1 : (l - 1)], collapse=', '), x[l], sep=', and ')
  }

  ## Extract the 0.05, 0.125, 0.25, 0.375, 0.625, 0.75, 0.875, and 0.95
  ## quantiles, the median, standard deviation, and length from the given vector.
  ## if less then 3 elements in the given vector then return the meadian
  ## 9 NA's and the length of the given vector.
  quant <- function(y) {
    probs <- c(0.05, 0.125, 0.25, 0.375)
    probs <- sort(c(probs, 1 - probs))
    y <- y[! is.na(y)]
    if(length(y) < 3) {
      w <- c(median(y), rep(NA, 9), length(y))
      names(w) <- c('Median', format(probs), 'se', 'n')
      return(w)
    }
    w <- hdquantile(y, probs)
    m <- hdquantile(y, 0.5, se=TRUE)
    se <- as.numeric(attr(m, 'se'))
    c(Median=as.numeric(m), w, se=se, n=length(y))
  }

  ## Get the mean and standard deviation and confidence interval
  ## for the given vector
  meanse <- function(y) {
    y <- y[! is.na(y)]
    n <- length(y)
    se <- if(n < 2) NA else sd(y) / sqrt(n)
    if(is.logical(y) || all(y %in% c(0., 1.))) {
      p  <- mean(y)
      ci <- binconf(sum(y), n)[1, ]
      if(p == 0. || p == 1.) {
        ## Don't trust se=0 at extremes; backsolve from Wilson interval
        w  <- diff(ci[c('Lower', 'Upper')])
        se <- 0.5 * w / qnorm(0.975)
      } else se <- sqrt(p * (1. - p) / n)
    }
    else ci <- smean.cl.boot(y, na.rm=FALSE)
    z <- c(ci, se=se, n=n)
    names(z) <- c('Mean', 'Lower', 'Upper', 'se', 'n')
    z
  }

  ## Find the proportion, lower and upper confidence intervals, the
  ## standard deviation and length of the given vector.
  propw <- function(y) {
    y <- y[!is.na(y)]
    n <- length(y)
    p <- mean(y)
    ci <- binconf(sum(y), n)[1, ]
    if(p == 0. || p == 1.) {
      ## Don't trust se=0 at extremes; backsolve from Wilson interval
      w  <- diff(ci[c('Lower', 'Upper')])
      se <- 0.5 * w / qnorm(0.975)
    }
    else se <- sqrt(p * (1. - p) / n)
    structure(c(ci, se=se, n=n),
              names=c('Proportion', 'Lower', 'Upper', 'se', 'n'))
  }

  ## If argument 'what' is the value 'byx' then determine which summary function to use
  ## when summarizing a variable.  Also determine final value of
  ## 'what' argument.
  if(what == 'byx') {
    if(length(fun)) stop('may not specify fun= when what="byx"')
    ## Function to determine the number of distinct numeric values
    ## in a vector
    g <- function(y) {
      if(is.logical(y)) 2
      else if(! is.numeric(y)) 0
      else length(unique(y[! is.na(y)]))
    }
    ## 'nu' contains the maximum number of unique values for all elements
    ## of 'Y'.
    nu <- max(sapply(Y, g))
    ## Set 'what' to its final value
    ## if the maximum number of unique values for all elements of 'Y' is less the 3
    ## then set variable 'fun' to the 'propw' function which displays the
    ## propotions of the elements of 'Y'. Then set 'what' to the value 'byx.binary'.
    what <- if(nu < 3) {
      fun <- propw
      'byx.binary'
      ## if the maximum number of unique values for all elements of 'Y' (dataset of
      ## left hand side of formula) is less then the number specified in the
      ## function argument 'continuous' (the minimum number of numberic values
      ## a variable must have in order to be considered continuous) then set
      ## argument 'fun' () to the 'meanse' function which
      ## displays the mean, standard deviation and confidence interval for the
      ## elements of 'Y'. Otherwise set 'what' to the value 'byx.discrete'.
    } else if(nu < continuous) {
      fun <- meanse
      'byx.discrete'
      ## Otherwise if argument 'byx.type' (determines the type of byx plot done.
      ## either 'quantiles' or 'violin') equals the value 'quantiles' then set
      ## 'fun' to the function 'quant' which displays the quantiles for the 
      ## elements of 'Y'. Then set 'what' to the value 'byx.cont'.
    } else {
      if(byx.type == 'quantiles') fun <- quant
      ## NOTE: if argument 'byx.type' equals 'NULL' or the value 'violin' then 'fun'
      ## is 'NULL'.
      'byx.cont'
    }
  }

  ## If argument 'what' (main control variable) is equal to value 'hist'
  ## and argument 'groups' (Defines name of grouping term in formula) is
  ## not specified and dataset 'X' has 1 column then set variable 'manygroups'
  ## the value 'TRUE' if the number of levels in the first column of
  ## of the dataset 'X' is more then 3. Otherwise set variable 'manygroups'
  ## to value 'FALSE'.
  ## Otherwise do nothing.
  if(what == 'hist' && ! length(groups) && ncol(X) == 1)
    manygroups <- length(levels(X[[1]])) > 3

  ## Is first x variable on the x-axis of an x-y plot the result
  ## of a summarizing function?
  ## This is determined if argument 'what' (main control variable for
  ## dReport) equals the value 'xy' and argument 'fun' (summarizing
  ## function for dataset) is not specified or if the first 3 letters of
  ## argument 'what' is equal to the value 'byx'
  fx <- (what == 'xy' && ! length(fun)) || substring(what, 1, 3) == 'byx'
  ## Determine the base part of the title of the plot.
  ## if variable 'fx' is TRUE then the this is a versus plot where one
  ## or more y values is ploted vs. the stratification variables.
  ## Otherwise this plot is a just one or more y values plotted together.
  a <- if(fx) {
    if(length(ylabs) < 7)
      paste(if(what != 'xy') 'for', past(ylabs), 'vs.', stratlabs[1])
     else paste('for', length(ylabs), 'variables vs.', stratlabs[1])
  } else paste('for',
               if(length(ylabs) < 7) past(ylabs) else
               paste(length(ylabs), 'variables'))

  ## Capitalize the base part of the title
  al <- upFirst(a, alllower=TRUE)

  ## Create the default title if the argumnt 'head' (optional header
  ## text to display) is not speficied.
  if(!length(head))
    head <-
      switch(what,
       hist         = paste('Histograms', al),
       ecdf         = paste('Empirical cumultive distribution functions', al),      
       proportions  = paste('Proportions', al),
       xy           = if(length(fun)) 'Statistics' else a,
       byx.binary   = paste('Proportions and confidence limits', al),
       byx.discrete =
             paste('Means and 0.95 bootstrap percentile confidence limits', al),
       byx.cont     = paste('Medians',
                            switch(byx.type,
                                   hist='with histograms',
                                   quantiles='with quantile intervals',
                                   violin='with violin (density) plots'),
                            al))

  ## Create statification label by creating a english language list of
  ## stratification variables labels except for the first element if the argument
  ## 'what' (main controlling variable for dReport) value is 'xy' and the
  ## argument 'fun' (a function for summarizing data for display) is not set
  ## or argument 'what' (main controlling variable for dReport) starts with the value 'byx'.
  ## Otherwise create a statification label by creating an english language
  ## list of the stratification variable labels.
  sl <- tolower(past(if((what == 'xy' && ! length(fun)) || 
                        what %in% c('byx.binary', 'byx.discrete',
                                    'byx.cont'))
                       stratlabs[-1] else stratlabs))

  ## create short caption for graphic if length of variable 'sl' is 0 then
  ## use argument 'head' (initial text in the figure caption) as the
  ## begining of the caption variable 'cap'.
  ## Otherwise join argument 'head' and variable 'sl' with the string value
  ## ' stratified by '.
  cap <- if(!length(sl)) head
  else sprintf('%s stratified by %s', head, sl)

  ## Save the current value of the variable 'cap' (graphic caption) in the variable
  ## 'shortcap'
  shortcap <- cap

  ## Make a list containing the forumula with no id, the data, the subset,
  ## na.action for later use in summarizing functions.
  dl <- list(formula=formula.no.id,
             data=data, subset=subset, na.action=na.action)

  ## Generate the plot of the object based on the value of the argument 'what'
  ## (main controlling variable for dReport)

  switch(what,
         ## Spike histograms
         hist = {
           p <- do.call('histboxpM',
                        c(list(x=Y,
                               group=interaction(X, sep='\n')), sopts))
         },
         ## ECDFs
         ecdf = {
           p <- do.call('ecdfpM',
                        c(list(x=Y,
                               group=interaction(X, sep='\n')), sopts))
           },
         proportions = {
           ## Over write the element 'sort' from the argument 'sopts' (options
           ## to pass to the summarizing function) with the value found in argument
           ## 'summaryPsort' (whether to sort categories in descending order of
           ## frequencies.
           sopts$sort <- summaryPsort
           ## Run summaryP on variable 'dl' (data for use in summarizing functions)
           ## and argument 'sopts' (options to pass to the summarizing function) passed
           ## as arguments.
           s <- do.call('summaryP', c(dl, sopts))
           popts$col    <- popts$colors
           popts$colors <- NULL
           p <- do.call('plot', c(list(s, marginVal=if(margpres) 'All',
                                       groups=groups), popts))
         },   # end proportions
         xy = {
           ## Create xy plots using the given summary function provided in argument
           ## 'fun' (function that transforms the response variables into summary
           ## statistics)
           s <- do.call('summaryS', c(dl, list(fun=fun), sopts))
           p <- do.call('plotp', c(list(data=s, groups=groups), popts))
         },
         byx.binary = ,
         byx.discrete =,
         byx.cont = {
           ## Create xy plots with function 'summaryS' using the argument 'fun'
           ## (can be one of the following functions 'quant', 'meanse',
           ## and 'propw').  If argument 'fun' is 'NULL' then do function
           ## 'summaryS' default action.
           s <- do.call('summaryS', c(dl, list(fun=fun), sopts))
           ylim <- NULL
           ## if the value of argument 'what' (main controlling variable for dReport)
           ## is either value 'byx.binary' or value 'byx.discrete' and element 'y' of
           ## variable 's' (result of 'summaryS' function call) contains both the
           ## values 'Lower' and 'Upper' then extract the upper and lower confidence
           ## intervals for each level of element 'yvar' of 's' and store them in
           ## variable 'ylim' for future use as the y limits of the plot
           if(what %in% c('byx.binary', 'byx.discrete') &&
              all(c('Lower', 'Upper') %in% colnames(s$y))) {
             yvl <- levels(s$yvar)
             ylim <- vector('list', length(yvl))
             names(ylim) <- yvl
             for(yv in levels(s$yvar)) {
               j <- s$yvar == yv
               ylim[[yv]] <- c(min(s$y[j, 'Lower'], na.rm=TRUE),
                               max(s$y[j, 'Upper'], na.rm=TRUE))
             }
           }
           ## Do a plotly plot of summaryS
           p <- do.call('plotp',
                 c(list(data=s, groups=groups, ylim=ylim,
                   sfun=if(byx.type == 'hist' && what == 'byx.cont')
                         medvpl else mbarclpl),
                   popts))
         } )  # end switch(what)

  ## store the element 'nobs' (the number of non-NA observations of each
  ## variable in the left hand side of a formula) in variable 'nobs'
  nobs <- Nobs$nobs
  ## Get the min and max of varaibel 'nobs' (number of non-NA observations
  ## of each variable in the left hand side of a formula) and store that
  ## vector in the variable 'r'
  r <- range(nobs)
  ## If the min and max values of variable 'nobs' (number of non-NA observations
  ## of each variable in the left hand side of a formula) are the same
  ## then deduplicate those values and store that value in variable 'n'
  ## Otherwise join the min value to the max value with the string value
  ## ' to '
  nn <- if(r[1] == r[2]) r[1] else paste(r[1], 'to', r[2])
  ## append Nobs range to end of caption contained in variable 'cap'
  cap <- paste0(cap, '. N=', nn)
  ## If argument 'tail' (user specified end of the caption) exists then
  ## append argument 'tail' on to the end of the caption contained in
  ## variable 'cap'.
  if(length(tail)) cap <- paste(cap, tail, sep='. ')
  ## Create needle graphic pop-up for caption
  ## set variable 'n' to max of nobs
  n <- c(randomized=r[2])
  nobsg <- Nobs$nobsg
  ## if variable 'nobsg' (number of observations of the left hand side
  ## of the formula grouped by the grouping variable) is not NULL then
  ## append the max of each row of variable 'nobsg'.
  if(length(nobsg)) n <- c(n, apply(nobsg, 1, max))

  ned <- function(...) {
    sf <- sampleFrac(..., study=study)
    structure(dNeedle(sf, study=study), table=attr(sf, 'table'))
  }
  extra <- function(x) c(attr(x, 'table'), x)
  needle <- ned(n, nobsY=Nobs)
  putHcap(cap, scap=shortcap, extra=extra(needle))
  p
}
