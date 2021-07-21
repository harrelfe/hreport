#' Survival Report
#'
#' Generate a Survival Report with Kaplan-Meier Estimates
#'
#' @param formula a formula with survival (\code{Surv}) objects on the left hand side and an optional stratification factor on the right (or \code{1} if none).  The survival object component variables should be labeled; these labels are used for graph annotation.  If any of the \code{Surv} objects are competing risk objects (see \code{\link[survival]{Surv}}), event labels come from the factor levels in the variable that was the second argument to \code{Surv}, and the first factor level must correspond to right-censored observations.
#' @param data data.frame
#' @param subset optional subsetting criteria
#' @param na.action function for handling \code{NA}s while creating a data frame
#' @param study character string identifying the study; used in multi-study reports or where distinct patient strata are analyzed separately.  Used to fetch the study-specific metadata stored by \code{\link{sethreportOption}}.  Single study reports just use \code{study=' '}.
#' @param ylab character. Passed to \code{\link[rms]{survplotp.npsurv}} as the \code{ylab} argument.  Constructed by default.
#' @param what \code{"1-S"} (the default) to plot cumulative incidence functions or \code{"S"} to plot cumulative survival functions.  If any of the survival time objects on the left hand side are competing risk objects, \code{"S"} may not be used.
#' @param conf character. See \code{\link[rms]{survplotp.npsurv}}.
#' @param cause character vector or list.  If a vector, every \code{Surv} term on the left hand side of \code{formula} will have cumulative incidence plotted for all causes that appear in \code{cause}.  If a list, the list elements must correspond to the \code{Surv} terms in order, and specify which causes to display from the corresponding \code{Surv} object.  When \code{cause} is a list and one of its elements contains more than one character string, or when \code{cause} is a vector and for one \code{Surv} object it matches multiple causes, \code{survReport} produces more plots than there are \code{Surv} objects.
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used.
#' @param tail optional character string.  Specifies final text in the figure caption, e.g., what might have been put in a footnote in an ordinary text page.  This appears just before any needles.
#' @param h numeric. Height of plots in pixels.
#' @param w numeric. Width of plots in pixels.
#' @param mylim numeric 2-vector.  Used to force expansion of computed y-axis limits.  See \code{survplotp}.
#' @param aehaz logical.  Set to \code{FALSE} to not print number of events and hazard rate on plots.
#' @param times numeric vector.  If specified, prints cumulative incidence probabilities at those times on the plots.
#' @param opts list.  A list specifying arguments to \code{survReport} that override any other arguments.  This is useful when making a long series of \code{survReport} calls with the same options, as the options can be defined up front in a list.
#' @param \dots ignored
#' @importFrom rms npsurv survplotp
#' @importFrom survival survfit
#' @importFrom Formula Formula model.part
#' @references
#'    Boers M (2004): Null bar and null zone are better than the error bar to compare group means in graphs.  J Clin Epi 57:712-715.
#' @export
#' @examples
#' # See tests directory test.Rnw for a live example
#' \dontrun{
#'   set.seed(1)
#'   n <- 400
#'   dat <- data.frame(t1=runif(n, 2, 5), t2=runif(n, 2, 5),
#'                     e1=rbinom(n, 1, .5), e2=rbinom(n, 1, .5),
#'                     treat=sample(c('a','b'), n, TRUE))
#'   dat <- upData(dat,
#'                 labels=c(t1='Time to operation',
#'                          t2='Time to rehospitalization',
#'                          e1='Operation', e2='Hospitalization',
#'                          treat='Treatment')
#'                 units=c(t1='year', t2='year'))
#'   survReport(Surv(t1, e1) + Surv(t2, e2) ~ treat, data=dat)
#'
#'   dat <- upData(dat, labels=c(t1='Follow-up Time', t2='Time'),
#'                 cause=factor(sample(c('death','MI','censor'), n, TRUE),
#'                              c('censor', 'MI', 'death')))
#'   survReport(Surv(t1, cause) ~ treat, cause='death', data=dat)
#'   survReport(Surv(t1, cause) + Surv(t2, cause) ~ treat,
#'              cause=list(c('death', 'MI'), 'death'), data=dat)
#'   # Two plots for t1, one plot for t2
#' }

survReport <- function(formula, data=NULL, subset=NULL, na.action=na.retain,
                       study=' ',
                       ylab=NULL, what=c('1-S', 'S'),
                       conf=c('bands', 'none'),
                       cause=NULL,
                       head=NULL, tail=NULL,
                       h=425, w=675,
                       mylim=NULL, aehaz=TRUE, times=NULL,
                       opts=NULL, ...)
{
  conf <- match.arg(conf)
  what <- match.arg(what)
  if(length(cause)) what <- '1-S'
  sfun <- switch(what, 'S'=function(y) y, '1-S'=function(y) 1 - y)

  ## Bring arguments from opts as if they were listed outside opts
  if(length(opts) && is.list(opts))
    for(j in 1 : length(opts))
      assign(names(opts)[j], opts[[j]], immediate=TRUE)

  kmlab <- if(what == 'S') 'Kaplan-Meier estimates'
           else 'Kaplan-Meier cumulative incidence estimates'

  past <- function(x) {
    l <- length(x)
    y <- if(l < 2) x
    else if(l == 2) paste(x, collapse=' and ')
    else paste(paste(x[1 : (l - 1)], collapse=', '), x[l], sep=', and ')
    upFirst(y, alllower=TRUE)
  }

  ned <- function(...) {
    sf <- sampleFrac(..., study=study)
    structure(dNeedle(sf, study=study), table=attr(sf, 'table'))
  }
  extra <- function(x) c(attr(x, 'table'), x)

  form <- Formula::Formula(formula)
  Y <- if(length(subset)) model.frame(form, data=data, subset=subset,
                                      na.action=na.action)
  else model.frame(form, data=data, na.action=na.action)
  X <- model.part(form, data=Y, rhs=1)
  Y <- model.part(form, data=Y, lhs=1)
  
  if(length(cause) && is.list(cause) && length(cause) != length(Y))
    stop('when cause is a list it must have length = number of Surv objects on left side of model')
  

  namx <- labx <- NULL
  if(length(X)) {
    x <- X[[1]]
    namx <- names(X)[1]
    labx  <- upFirst(ifelse(label(x) == '', namx, label(x)), alllower=TRUE)
  }

  hro  <- gethreportOption(study=study)
  tvar <- hro$tx.var
  Nobs <- nobsY(formula, group=tvar,
                data=data, subset=subset, na.action=na.action,
                matrixna='any')

  ny <- nycause <- ncol(Y)
  ## nycause is the total number of plots counting any separate plots for
  ## multiple causes

  if(length(cause)) {
    Cause <- list()
    for(i in 1 : ny) {
      y <- Y[[i]]
      states <- attr(y, 'states')
      usecause <- ''
      if(length(states)) {
        selectedCauses <- if(is.list(cause)) cause[[i]] else cause
        if(! length(selectedCauses))
          stop(paste('cause not specified for Surv object #', i))
        if(is.list(cause) && any(selectedCauses %nin% states))
          stop(paste('a selected cause is not in the list of states for Surv object #',
                     i, '\nstates:', paste(states, collapse=','),
                     '\ncause:', paste(selectedCauses, collapse=',')))
        usecause <- intersect(states, selectedCauses)
      }
      Cause[[i]] <- usecause
    }
    nycause <- length(unlist(Cause))
  }
  
  x.is.tx <- FALSE; ng <- 0
  if(length(X)) {
    x <- X[[1]]
    ng <- if(is.factor(x)) length(levels(x)) else
     length(unique(x[!is.na(x)]))
    if(namx == tvar) {
      x.is.tx <- TRUE
      col <- hro$tx.linecol
      lwd <- hro$tx.lwd
      if(! is.factor(x)) x <- factor(x, names(hro$denom)[-(1:2)])
    }
    else {
      col <- rep(hro$nontx.col, length=ng)
      lwd <- rep(c(1, 3), length=ng)
    }
  } else {
    x <- rep('', nrow(Y)) 
    col <- 1
    lwd <- 2
  }

  ## nobs <- rep(0, 1 + x.is.tx * ng)
  nobs <- Nobs$nobs
  evlab <- character(nycause)
  capconf <-
    if(conf == 'none') '. $N$='
    else
      if(ng == 0) ', along with pointwise 0.95 confidence bands. $N$='
    else
      ', along with half-height of 0.95 confidence limits for differences centered at estimate midpoints. $N$='

  P    <- Cap <- list()
  ifig <- 0
  for(i in 1 : ny) {
    y <- Y[[i]]

    states <- attr(y, 'states')
    usecause <- ''
    if(length(attr(y, 'states'))) {
      if(! length(cause))
        stop('cause must be specified if any Surv objects on the left side are for competing risks')
      usecause <- Cause[[i]]
    }
    
    s <- rms::npsurv(y ~ x)
    ncurrent <- sum(! is.na(y) & ! is.na(x))

    icause <- 0
    for(cau in usecause) {
      icause <- icause + 1
      ifig   <- ifig   + 1
      evlab[icause] <- if(cau == '') label(y) else cau
      yl <- ylb <- if(length(ylab)) ylab else upFirst(evlab[icause])
      yl <- if(what == 'S') paste(yl, '-Free Probability', sep='')
      else paste('Cumulative Incidence of', yl)

      P[[ifig]] <-
        switch(what,
               S     = rms::survplotp(s, fun=sfun,
                        conf=conf,
                        col=col, ylab=yl, mylim=mylim,
                        aehaz=aehaz, times=times,
                        width=w, height=h,
                        ...),
               '1-S' = rms::survplotp(s, state=if(length(cause)) cau,
                        fun=sfun, conf=conf,
                        col=col, ylab=yl, mylim=mylim,
                        aehaz=aehaz, times=times,
                        width=w, height=h,
                        ...))


      shortcap <-
        if(length(head)) head
        else if(cau == '')
          paste(kmlab, 'for',
                upFirst(evlab[icause], alllower=TRUE))
        else paste('Cumulative incidence of',
                   upFirst(cau, alllower=TRUE),
                   if(length(states) > 2) 'with competing events'
                   else 'with competing event',
                   past(setdiff(states, cau)))
      if(length(labx))
        shortcap <- paste(shortcap, 'stratified by', labx)
      cap <- paste(shortcap, capconf, ncurrent, '. ', tail, sep='')
      n <- c(randomized=ncurrent)
      nobsg <- Nobs$nobsg
      if(length(nobsg)) n <- c(n, apply(nobsg, 1, max))
      needle <- ned(n, nobsY=Nobs)
      Cap[[ifig]] <-
        putHcap(cap, scap=shortcap, extra=extra(needle), file=FALSE)
      }  # end over cause
  }  # end over Y

  markupSpecs$html$mdchunk(Cap, P)
  invisible()
}
