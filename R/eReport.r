#' Event Report
#'
#' Generates graphics for binary event proportions
#' 
#' Generates dot charts showing proportions of subjects having events (at any time).  Events can be categorized by a single level or by major and minor levels (e.g., body system and preferred terms).  When there are two treatments, half-width CLs of treatment differences are drawn, centered at the midpoint of the two proportions, and CLs for differences appear in hover text.   Input data must contain one record per event, with this record containing the event name.  If there is more than one event of a given type per subject, unique subject ID must be provided.  Denominators come from \code{hreport} options. It is also assumed that only randomized subjects are included in the dataset.  
#'
#' @param formula a formula with one or two left hand variables (the first representing major categorization and the second minor), and 1-2 right hand variables.  One of these may be enclosed in \code{id()} to indicate the presence of a unique subject ID, and the other is treatment.
#' @param data input data frame
#' @param subset subsetting criteria
#' @param na.action function for handling \code{NA}s when creating analysis frame
#' @param refgroup a character string specifying which treatment group is subtracted when computing risk differences.  If there are two treatments the default is the first one listed in \code{hreport options}.
#' @param minincidence a number between 0 and 1 specifying the minimum incidence in any stratum that must hold before an event is included in the summary
#' @param conf.int confidence level for difference in proportions (passed to \code{dotchartpl})
#' @param etype a character string describing the nature of the events, for example \code{"adverse events"}, \code{"serious adverse events"}.  Used in figure captions.
#' @param panel panel string
#' @param subpanel a subpanel designation to add to \code{panel}
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used.
#' @param tail a character string to add to end of automatic caption
#' @param h height of graph
#' @param w width of graph
#' @author Frank Harrell
#' @export
#' @examples
#' # See test.Rnw in tests directory

eReport <- function(formula, data=NULL, subset=NULL, na.action=na.retain,
                    refgroup=NULL, minincidence=0, conf.int=0.95,
                    etype='adverse events',
                    panel='events', subpanel=NULL, head=NULL, tail=NULL,
                    h=6, w=7, append=FALSE, popts=NULL) {

  if(grepl('[^a-zA-Z-]', panel))
    stop('panel must contain only A-Z a-z -')
  if(length(subpanel) && grepl('[^a-zA-Z-]', subpanel))
    stop('subpanel must contain only A-Z a-z -')

  smaller2 <- markupSpecs$html$smaller2
  
  form <- Formula(formula)
  environment(form) <- new.env(parent = environment(form))
  en <- environment(form)
  assign(envir = en, 'id', function(x) x)

  Y <- if(length(subset)) model.frame(form, data=data, subset=subset,
                                      na.action=na.action)
   else model.frame(form, data=data, na.action=na.action)
  X <- model.part(form, data=Y, rhs=1)
  Y <- model.part(form, data=Y, lhs=1)
  rhs <- terms(form, rhs=1, specials='id')
  sr  <- attr(rhs, 'specials')
  ## specials counts from lhs variables
  wid <- sr$id
  if(length(wid)) wid <- wid - ncol(Y)

  nY <- ncol(Y)
  major <- NULL
  if(nY > 1) major <- Y[[1]]
  event <- Y[[nY]]
  id <- 1 : length(event)
  nX <- ncol(X)
  gname <- glabel <- ''
  if(nX > 1 + (length(wid) > 0))
    stop('cannot have more than one right hand variable other than id variable')
  if(length(wid)) {
    id    <- X[[wid]]
    j <- setdiff(1 : nX, wid)
  } else if(nX == 1) j <- 1
    else j <- 0
  if(j == 0) {
    group <- factor(rep('', length(event)))
    gname <- glabel <- ''
  }
  else {
    group <- X[[j]]
    gname <- names(X)[j]
    glabel <- label(group, default=gname)
  }

  event <- as.factor(event)
  levels(event) <- upFirst(levels(event))
#  event  <- as.character(event)
  uevent <- levels(event)
  nue    <- length(uevent)
  N <- gethreportOption('denom')
  n <- N[setdiff(names(N), c('enrolled', 'randomized'))]
  groups <- names(n)
  group <- as.character(group)
  if(length(groups) == 2 && ! length(refgroup)) refgroup <- groups[1]

  g <- function(i) {
    idi   <- id[i]
    num   <- length(unique(idi))
    denom <- n[group[i[1]]]
    c(.num=num, .denom=unname(denom))
  }

  z <- summarize(1 : length(group),
                 if(length(major)) list(major=major, event=event, group=group)
                 else
                   list(event=event, group=group),
                 g, stat.name=NULL)

  p <- with(z,
            dotchartpl(.num / .denom, major=major, minor=event,
                       group=group, num=.num, denom=.denom,
                       refgroup=refgroup, conf.int=conf.int,
                       minkeep=minincidence) )

  rem <- attr(p, 'levelsRemoved')
  small <- length(rem)
  
  if(! length(head))
    head <- paste('Proportion of', etype,
                  'by', upFirst(glabel, lower=TRUE),
                  'sorted by descending risk difference')
  if(minincidence > 0 && small > 0)
    head <- paste0(head, '. ', small, ' events with less than ',
                   minincidence,
                   ' incidence in all groups are not shown',
                   if(small < 11) smaller2(paste0(' (',
                                         paste(rem, collapse=', '), ')')),
                   '.')

  shortcap <- paste('Proportion of', etype, 'by',
                    upFirst(glabel, lower=TRUE))

  ned <- function(used) {
    sf <- sampleFrac(used)
    structure(dNeedle(sf), table=attr(sf, 'table'))
  }
  extra <- function(x) c(attr(x, 'table'), x)

  N[1] <- N[2]    # assume used only randomized subjects
                  # N[2] out of original N[1] subjects

  putHfig(p, head, scap=shortcap, extra=extra(ned(N)))
  invisible()
}
