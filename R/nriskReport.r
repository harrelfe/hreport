#' Number at Risk Report
#'
#' Graph number of participants at risk
#' 
#' \code{nriskReport} generates several plots where stratification is by the cross-classifications of all right-hand-side variables other than the time variable and an \code{id} variable.  The first plot depicts the number at risk as a function of follow-up time.  It is assumed that this function is only run on randomized participants.  If an \code{id} variable is present but stratification variables are not, other plots are also produced: a histogram of the number of contacts per participant, a histogram of times at which participants have contacts, the average number of contacts as a function of elapsed time, and a histogram showing the distribution of the longest gap between contacts over participants.
#' @param formula a formula with time on the left hand side, and with variables on the right side being possible stratification variables.  If no stratification put \code{1} as the right hand side.  Specify unique participant IDs by including a term \code{id()} if participantss have more than one observation.
#' @param time0 a character string defining the meaning of time zero in follow-up.  Default is \code{"randomization"}.
#' @param data data frame
#' @param subset a subsetting epression for the entire analysis
#' @param na.action a NA handling function for data frames, default is \code{na.retain}
#' @param study character string identifying the study; used in multi-study reports or where distinct patient strata are analyzed separately.  Used to fetch the study-specific metadata stored by \code{\link{sethreportOption}}.  Single study reports just use \code{study=' '}.
#' @param ylab character string if you want to override \code{"Number Followed"}
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used
#' @param tail optional character string.  Specifies final text in the figure caption, e.g., what might have been put in a footnote in an ordinary text page.  This appears just before any needles.
#' @param h numeric.  Height of plot, in pixels
#' @param w numeric.  Width of plot
#' @export
#' @importFrom grDevices gray
#' @importFrom stats supsmu
#' @examples
#' # See test.Rnw in tests directory

nriskReport <-
  function(formula, 
           time0='randomization',
           data=NULL, subset=NULL, na.action=na.retain,
           study=' ',
           ylab='Number Followed', head=NULL, tail=NULL,
           h=400, w=700)
{
  ohead <- head
  hro   <- gethreportOption(study=study)
  tvar  <- hro$tx.var

  mu <- markupSpecs$html

  Nobs <- nobsY(formula, group=tvar,
                data=data, subset=subset, na.action=na.action)
  formula.no.id <- Nobs$formula   ## removes id()
  form <- Formula::Formula(formula.no.id)
  environment(form) <- new.env(parent = environment(form))
  en <- environment(form)
  assign(envir = en, 'id', function(x) x)

  Y <- if(length(subset)) model.frame(form, data=data, subset=subset,
                                      na.action=na.action)
       else model.frame(form, data=data, na.action=na.action)
  
  id     <- Nobs$id
  nam    <- names(Y)   # names of all var in formula except id
  yraw   <- Y[, 1]
  yunits <- units(yraw)
  yraw   <- yraw[! is.na(yraw)]
  if(yunits == '') yunits <- 'days'

  nx <- ncol(Y) - 1

  if(length(id) && anyDuplicated(id)) {
    attrib <- keepHattrib(Y)
    ## Reduce data matrix to one row per subject per stratum with
    ## maximum follow-up time, number of follow-up contacts, and
    ## longest gap between follow-up contacts for the subject
    Y <- data.table(Y, .id.=id)    # note Y already has no id
    setnames(Y, nam[1], '.y.')
    by <- c(nam[-1], '.id.')
    mx <- function(w) as.double(if(any(! is.na(w))) max(w, na.rm=TRUE) else NA)
    gp <- function(w) {
      w <- w[! is.na(w)]
      as.double(if(length(w) > 0) max(diff(sort(c(0, w)))) else NA)
      }
    Y <- Y[, list(.maxy. = mx(.y.),
                  .n.    = length(.y.),
                  .gap.  = gp(.y.)       ),
           by=by]
    Y <- Y[, c('.maxy.', '.n.', '.gap.', nam[-1]), with=FALSE]
    setnames(Y, '.maxy.', nam[1])
    Y <- restoreHattrib(as.data.frame(Y), attrib)
  }

  tx.used <- FALSE
  if(nx > 0) {
    X <- Y[nam[-1]]  
    xnam    <- names(X)
    tx.used <- tvar %in% xnam
    labs    <- sapply(X, label)
    labs    <- ifelse(labs == '', xnam, labs)
    past <- function(x) {
      l <- length(x)
      if(l < 2) x
      else if(l == 2) paste(x, collapse=' and ')
      else paste(paste(x[1 : (l - 1)], collapse=', '), x[l], sep=', and ')
    }
    stratlabs <- past(labs)
  }

  y <- Y[[nam[1]]]
  
  if(! length(head))
    head <- sprintf('Number of participants followed at least x %s from %s',
                    yunits, time0)

  cap <- if(nx == 0) head
         else
           sprintf('%s stratified by %s', head, stratlabs)
  shortcap <- cap

  colors  <- if(tx.used) hro$tx.linecol
             else
               rep(c(gray(c(0, .7)), 'blue', 'red', 'green'), 10)
  if(nx > 1) colors <- NULL

  gr <- if(nx > 0) interaction(X, drop=TRUE, sep='<br>')
        else
          rep('', length(y))

#  if(tx.used) {
#    col <- hro$tx.linecol
#    lwd <- hro$tx.lwd
#  } else {
#    col <- rep(c(gray(c(0, .7)), 'blue', 'red', 'green'), 10)
#    lwd <- rep(c(1, 3), length=10)
#  }

  p <- ecdfpM(y, group=gr, what='1-f', ylab=ylab,
              xlab=paste(upFirst(yunits), 'from', upFirst(time0)),
              height=h, width=w, colors=colors, extra=c(0, 0.025))
  
  if(length(tail)) cap <- paste(cap, tail, sep='. ')
  no <- c(Nobs$nobs, Nobs$nobs, Nobs$nobsg)
  names(no) <- c('enrolled', 'randomized', rownames(Nobs$nobsg))

  ned <- function(...) {
    sf <- sampleFrac(..., study=study)
    structure(dNeedle(sf, study=study), table=attr(sf, 'table'))
  }
  extra <- function(x) c(attr(x, 'table'), x)

  needle <- ned(no, nobsY=Nobs)
  hc <- putHcap(cap, scap=shortcap, extra=extra(needle), file=FALSE)

  P        <- Cap <- list()
  P[[1]]   <- p
  Cap[[1]] <- hc

  if(length(id) && anyDuplicated(id) && nx == 0) {
    head <- ohead
    if(! length(head))
      head <- sprintf('Distributions of follow-up contacts, with times in %s',
                      yunits)
    cap      <- head
    shortcap <- cap

    cap <- paste0(cap, '. Top left panel is a histogram showing the distribution of the number of contacts per participant.  Top right panel is a histogram showing the distribution of time from ', time0, ' to all ', length(yraw), ' contacts.  Bottom left panel is a histogram showing the distribution of the longest time gap between contacts per participant.  Bottom right panel shows the relationship between the time lapse between ', time0, ' and last contact per participant and the average number of contacts for the participant.')

    if(length(tail)) cap <- paste(cap, tail, sep='. ')
    lay <- function(p, xlab, ylab)
      plotly::layout(p, xaxis=list(title=xlab), yaxis=list(title=ylab))

    if(! length(h)) h <- 500
    if(! length(w)) w <- 740
    p1 <- p2 <- p3 <- p4 <- plotly::plot_ly(height=h, width=w)

    p1 <- plotly::add_histogram(p1, x = ~ .n., nbinsx=15, data=Y,
                                showlegend=FALSE)
    xlab <- 'Number of Contacts Per Participant'
    ylab <- 'Number of Participants'
    p1 <- lay(p1, xlab, ylab)

    p2 <- plotly::add_histogram(p2, x = ~ yraw, nbinsx=40, showlegend=FALSE)
    xlab <- paste0(upFirst(yunits), ' From ', upFirst(time0), ', All Contacts')
    ylab <- 'Number of Contacts'
    p2 <- lay(p2, xlab, ylab)
    
    p3 <- plotly::add_histogram(p3, x = ~ .gap., nbinsx=40, data=Y,
                                showlegend=FALSE)
    xlab <- mu$varlabel('Longest Gap Between Contacts Per Participant',
                        yunits)
    ylab <- 'Number of Participants'
    p3 <- lay(p3, xlab, ylab)
    
    z  <- supsmu(y, Y$.n.)
    p4 <- plotly::add_lines(p4, x = ~ z$x, y = z$y, showlegend=FALSE)
    xlab <- paste(upFirst(yunits), 'From', upFirst(time0), 'to Last Contact')
    ylab <- 'Number of Contacts Per Participant'
    p4 <- lay(p4, xlab, ylab)
    
#    needle <- ned(no, nobsY=Nobs)
    Cap[[2]] <- putHcap(cap, scap=shortcap, extra=extra(needle), file=FALSE)

    P[[2]] <- 
      plotly::subplot(p1, p2, p3, p4,
                      titleX=TRUE, titleY=TRUE, shareX=FALSE, shareY=FALSE,
                      nrows=2, margin=0.05)
  }
  
  mu$mdchunk(Cap, P)
  invisible()
  }

utils::globalVariables('.y.')
