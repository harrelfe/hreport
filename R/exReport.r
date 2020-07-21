#' Exclusion Report
#'
#' Generates graphics for sequential exclusion criteria
#' 
#' With input being a series of essentially binary variables with positive indicating that a subject is excluded for a specific reason, orders the reasons so that the first excludes the highest number of subjects, the second excludes the highest number of remaining subjects, and so on.  If a randomization status variable is present, actually randomized (properly or not) subjects are excluded from counts of exclusions.  First draws a single vertical axis graph showing cumulative exclusions, then creates a 2-panel dot chart with the first panel showing that information, along with the marginal frequencies of exclusions and the second showing the number of subjects remaining in the study after the sequential exclusions.  A pop-up table is created showing those quantities plus fractions.  There is an option to not sort by descending exclusion frequencies but instead to use the original variable order.  Assumes that any factor variable exclusions that have only one level and that level indicates a positive finding, that variable has a denominator equal to the overall number of subjects.
#'
#' An attribute dot chart is also drawn using the Hmisc package \code{combplotp} function, showing frequencies of all combinations of exclusions that occurred in the data.
#'
#' @param formula a formula with only a right-hand side, possibly containing a term of the form \code{pending(x)} to inform the function of which subjects have incomplete randomization ("randomization pending").  The \code{pending} variable is ignored if a subject has an exclusion marked.  A \code{randomized} variable is an optional \code{logical} vector specifying which subjects are considered to have been randomized.  The presence of this variable causes consistency checking against exclusions.  One or more \code{cond} variables provide binary/logical vectors used to define subsets of subjects for which denominators are used to compute additional fractions of exclusions that are reported in a detailed table.  The arguments of the \code{cond} function are the name of the original variable (assumed to be provided as a regular variable in \code{formula}, a single character string giving the label for the condition, and the vector of essentially binary values that specify the condition.
#' @param data input data frame
#' @param subset subsetting criteria
#' @param na.action function for handling \code{NA}s when creating analysis frame
#' @param study character string identifying the study; used in multi-study reports or where distinct patient strata are analyzed separately.  Used to fetch the study-specific metadata stored by \code{\link{sethreportOption}}.  Single study reports just use \code{study=' '}.
#' @param ignoreExcl a formula with only a right-hand side, specifying the names of exclusion variable names that are to be ignored when counting exclusions (screen failures)
#' @param ignoreRand a formula with only a right-hand side, specifying the names of exclusion variable names that are to be ignored when counting randomized subjects marked as exclusions
#' @param plotExRemain set to \code{FALSE} to suppress plotting a 2-panel dot plot showing the number of subjects excluded and the fraction of enrolled subjects remaining
#' @param autoother set to \code{TRUE} to add another exclusion \code{Unspecified} that is set to \code{TRUE} for non-pending subjects that have no other exclusions
#' @param sort set to \code{FALSE} to not sort variables by descending exclusion frequency
#' @param whenapp a named character vector (with names equal to names of variables in formula).  For each variable that is only assessed (i.e., is not \code{NA}) under certain conditions, add an element to this vector naming the condition
#' @param erdata a data frame that is subsetted on the combination of \code{id} variables when \code{randomized} is present, to print auxiliary information about randomized subjects who have exclusion criteria
#' @param head character string.  Specifies initial text in the figure caption, otherwise a default is used.
#' @param tail a character string to add to end of automatic caption
#' @param detailTail a character string to add to end of automatic caption for appendix table with listing of subject IDs
#' @param details set to \code{FALSE} to prevent writing details about exclusions (IDs, etc.)
#' @author Frank Harrell
#' @import htmlTable
#' @export
#' @examples
#' # See test.Rnw in tests directory

exReport <- function(formula, data=NULL, subset=NULL, na.action=na.retain,
                     study=' ',
                     ignoreExcl=NULL, ignoreRand=NULL, plotExRemain=TRUE,
                     autoother=FALSE, sort=TRUE, whenapp=NULL, erdata=NULL,
                     head=NULL, tail=NULL,
                     detailTail=NULL,
                     details=TRUE) {

  
  if(length(ignoreExcl)) ignoreExcl <- all.vars(ignoreExcl)
  if(length(ignoreRand)) ignoreRand <- all.vars(ignoreRand)

  environment(formula) <- new.env(parent = environment(formula))
  en <- environment(formula)
  assign(envir = en, 'pending',    function(x) x)
  assign(envir = en, 'randomized', function(x) x)
  assign(envir = en, 'id',         function(x) x)
  gcond <- function(x, label, condition) {
    attr(condition, 'what') <- c(variable = as.character(substitute(x)),
                                 label    = label)
    condition
  }
  assign(envir = en, 'cond', gcond)
  X <- if(length(subset)) model.frame(formula, data=data, subset=subset,
                                      na.action=na.action)
       else model.frame(formula, data=data, na.action=na.action)
  Terms <- terms(formula, specials=c('pending', 'randomized', 'cond', 'id'))
  s <- attr(Terms, 'specials')
  sp  <- s$pending
  sr  <- s$randomized
  sc  <- s$cond
  si  <- s$id
  Idnames <- if(length(si)) {
    a <- names(X)[si]
    a <- gsub('id\\(', '', a)
    gsub('\\)', '', a)
  }
         
  ispos <- function(x) {
    w <- if(is.logical(x)) x
    else if(is.numeric(x)) x > 0
    else tolower(as.character(x)) %in%
           c('present', 'yes', 'y', 'true', 'positive', '+')
    w[is.na(x)] <- FALSE
    w
  }

  mis <- function(x) if(is.factor(x) && length(levels(x)) == 1 &&
                        tolower(levels(x)) %in%
                        c('present', 'yes', 'y', 'true', 'positive', '+'))
                       rep(FALSE, length(x))
                     else is.na(x) | tolower(x) %in% c('unknown','n/a','u','uncertain')

  mu      <- markupSpecs$html    # in Hmisc
  mdchunk <- mu$mdchunk
  
#  mblue <- '#0080ff'
  
  N     <- gethreportOption('denom', study=study)[c('enrolled', 'randomized')]
  n <- norig <- nrow(X)
#  if(n != N['enrolled'])
#    warning(sprintf('number of observations (%s) does not equal number enrolled (%s) specified using sethreportOption(denom=)', n, N['enrolled']))

  rnd <- NULL
  if(length(sr)) rnd <- ispos(X[[sr]])

  ig <- if(length(ignoreExcl)) match(ignoreExcl, names(X))
  if(length(ig) && any(is.na(ig)))
    stop('ignoreExcl contains variables not in formula')
  margdenom <- sapply(if(length(c(sp, sr, sc, si, ig)))
                      X[, -c(sp, sr, sc, si, ig)]
                       else X,
                      function(x) sum(! mis(x)))
  Xc <- NULL
  if(length(sc)) {
    Xc <- X[, sc, drop=FALSE]
    colnames(Xc) <- sapply(Xc, function(x) attr(x, 'what')['variable'])
    Xclab        <- sapply(Xc, function(x) attr(x, 'what')['label'   ])
    names(Xclab) <- colnames(Xc)
  }

  Id <- if(length(si)) as.character(interaction(X[si]))

  npend <- 0
  if(length(sp)) {
    pending <- ispos(X[[sp]])
    ## Any observations marked as excluded should have pending ignored
    anyex <- rep(FALSE, n)
    namx  <- names(X)
    for(j in (1 : ncol(X))[- c(sp, sr, sc, si)])
      if(namx[j] %nin% ignoreExcl) anyex <- anyex | ispos(X[[j]])
    pending[anyex] <- FALSE
    ## Same with any observation marked as randomized
    if(length(rnd)) pending[rnd & ! is.na(rnd)] <- FALSE
    npend   <- sum(pending)
    n       <- n - npend
    X       <- X[! pending, - c(sp, sr, sc, si), drop=FALSE]
    if(length(Xc))  Xc  <- Xc [! pending,,  drop=FALSE]
    if(length(rnd)) rnd <- rnd[! pending]
    if(length(Id))  Id  <- Id [! pending]
  }
  else if(length(c(sr, sc, si))) X <- X[, - c(sr, sc, si), drop=FALSE]
  Xname <- names(X)
  k     <- ncol(X)
  Xlab  <- sapply(X, label)
  Xlab  <- ifelse(Xlab == '', Xname, upFirst(Xlab))

  anyre <- rep(FALSE, n)
  if(length(rnd)) {
    exclv <- character(0)
    nexr  <- integer(0)
    Ids   <- Idso <- character(0)
    for(i in 1 : k) {
      if(length(ignoreRand) && Xname[i] %in% ignoreRand) next
      x <- ispos(X[[i]])
      anyre <- anyre | (x & rnd)
      r <- sum(x & rnd, na.rm=TRUE)
      if(r > 0) {
        exclv <- c(exclv, Xlab[i])
        nexr   <- c(nexr,   r)
        if(length(Id)) {
          Ids  <- c(Ids, paste(Id[x & rnd], collapse=', '))
          Idso <- c(Idso, Id[x & rnd])
        }
      }
    }
    if(length(nexr)) {
      nnre  <- sum(anyre, na.rm=TRUE)
      exclv <- c(exclv, 'Total Partcipants with Any Exclusion')
      nexr   <- c(nexr, nnre)
      if(length(Ids)) Ids <- c(Ids, '')
      E <- data.frame(Exclusion=exclv, Frequency=nexr)
    }
  }

  if(length(ignoreExcl)) {
    X <- X[names(X) %nin% ignoreExcl]
    k <- ncol(X)
    Xname <- names(X)
    Xlab  <- Xlab[Xname]
  }

  ## If randomization status provided, don't count exclusions on
  ## randomized (rightly or wrongly) subjects, otherwise count all exclusions
  use       <- if(length(rnd)) ! rnd else TRUE

  R  <- list()
  Pl <- list()
  
  ## Draw combination (attribute) chart
  nP <- 1
  Pl[[nP]] <- combplotp(data=X[use, ], N=norig, includenone=FALSE)
  u <- if(length(rnd)) 'randomized participants' else 'the data'
  cap <- c('All combinations of exclusions occurring in',
           if(length(rnd)) 'non-randomized participants.' else 'the data.')
  if(npend > 0) cap <- c(cap, npend,
                         'pending participants were excluded from numerators.')
  R[[nP]]  <-
    putHcap(cap, 
            scap=paste('All combinations of exclusions',
                       if(length(rnd)) 'of non-randomized participants'),
            file=FALSE)
  
  marg      <- sapply(X, function(x) sum(ispos(x) & use, na.rm=TRUE))
  
  add      <- if(sort) which.max(marg) else 1
  cadd     <- Xname[add]
  exclude  <- ispos(X[[cadd]]) & use   ## new exclusion
  nexclude <- sum(exclude, na.rm=TRUE)
  nexcludec <- if(cadd %in% names(Xc)) sum(exclude & Xc[[cadd]], na.rm=TRUE)
   else nexclude
  X[exclude, ] <- NA  ## only consider subjects not previously excl.
  cond.denom <- n
  cd         <- n - nexclude
  
  if(k > 1) for(i in 2 : k) {
    remain   <- sapply(X, function(x) sum(ispos(x) & use, na.rm=TRUE))
    add      <- if(sort) which.max(remain) else i
    xn       <- Xname[add]
    exclude  <- ispos(X[[xn]]) & use
    nex      <- sum(exclude, na.rm=TRUE)
    nexc <- if(xn %in% names(Xc)) sum(exclude & Xc[[xn]], na.rm=TRUE)
     else nex
    if(nex > 0) {
      cadd         <- c(cadd, xn)
      nexclude     <- c(nexclude,  nex)
      nexcludec    <- c(nexcludec, nexc)
      X[exclude, ] <- NA
      cond.denom   <- c(cond.denom, cd)
      cd           <- cd - sum(exclude, na.rm=TRUE)
    }
  }

  nother <- n - sum(nexclude) - N['randomized']
  othlab <- character(0)
  if(autoother && nother > 0 && 'unspecified' %nin% tolower(cadd)) {
    othlab     <- c(Unspecified = 'Unspecified')
    nexclude   <- c(nexclude,    nother      )
    nexcludec  <- c(nexcludec,   nother      )
    cadd       <- c(cadd,       'Unspecified')
    marg       <- c(marg,       NA           )
    cond.denom <- c(cond.denom, cd           )
    margdenom  <- c(margdenom,  norig        )
  }

  fracnewTotal  <- nexclude / n
  fracnewRemain <- nexclude / cond.denom
  fracremain    <- 1. - cumsum(nexclude) / n
  marg <- marg[cadd]
  excl <- cadd
  elab <- c(Xlab, othlab)
  u <- rep('', k + length(othlab))
  names(u) <- c(Xname, othlab)
  if(length(whenapp)) u[names(whenapp)] <- paste(whenapp, ', ', sep='')
  b <- ifelse(u == '', paste(' / ', margdenom, sep=''),
                       paste(' (', u, 'n=', margdenom, ')', sep=''))
  elab <- ifelse(margdenom < norig, paste(elab, b, sep=''), elab)
  swr <- function(w, ...) 
    sapply(strwrap(w, ..., simplify=FALSE),
           function(x) paste(x, collapse='<br>'))
  elabl <- swr(elab, width=25)
  elabr <-  swr(elab, width=25, exdent=8)
  # When smaller font used, wrap with longer width
  elabl2 <- swr(elab, width=37)
  elabr2 <- swr(elab, width=37, exdent=8)

  names(elab) <- names(elabl) <- names(elabr) <- names(elabl2) <-
    names(elabr2) <- c(Xname, othlab)
  elab <- elab  [cadd]
  ell  <- elabl [cadd]
  elr  <- elabr [cadd]
  ell2 <- elabl2[cadd]
  elr2 <- elabr2[cadd]
  
  m <- sum(nexclude)
  cumex <- cumsum(nexclude)
  r  <- c(10 * floor(cumex[1] / 10), 10 * ceiling(m / 10))
  xx <- rep(0.0125, length(cumex))
  p  <- plotly::plot_ly()
  p  <- add_markers(p, x=~ xx, y=~ cumex, hoverinfo='y')

  an <- list() # orginally formulated for annotations argument to plotly::layout
  side <- 2
  ones <- character(0)
  sz <- function(cex) round(14 * cex)
  j <- 0
  for(i in 1 : length(cumex)) {
    a <- nexclude[i]
    if(a == 1) {
      ones <- c(ones, ell[i])
      next
    }
    cex <- if(a / m < 0.02) .6 else if(a / m < 0.05) .8 else 1
    el <- if(cex >= .8) ell[i] else ell2[i]
    er <- if(cex >= .8) elr[i] else elr2[i]
    y <- cumex[i]
    u <- if(side == 1) el else er
    v <- paste(if(i == 1) '' else '+', a, '  ', u, sep='')
    j <- j + 1
    an[[j]] <-
    ## See if not likely to vertically run into previous entry
    if(i < 3 || (y - cumex[i - 2]) / diff(r) > 0.01) {
      if(side == 1)
        list(x=-0.135, y=y, text=v, align='right', xanchor='right', size=sz(cex))
      else
        list(x=0.07, y=y, text=v, align='right', xanchor='left', size=sz(cex))
    } else {
      if(side == 1)
        list(x=01, y=y + 0 * 0.0035 * diff(r), text=v, align='left', xanchor='left', size=sz(cex))
        else
          list(x=1, y=y + 0 * 0.0035 * diff(r), text=v, align='right', xanchor='right', size=sz(cex))
          }
    side <- 3 - side
  }
  if(length(ones))
    an[[j + 1]] <- list(x=.96, y=r[2] - 0 * diff(r) / 15, size=sz(0.5),
                        text=paste(c('One exclusion due to:', ones),
                                   collapse='<br>'),
                        align='right', xanchor='right')

  resList <- function(z) {
    ## take a list of lists each containing possibly different random
    ## variables on one observation, and put together into a list of vectors
    v <- unique(as.vector(unlist(sapply(z, names))))
    n <- length(z)
    r <- list()   # without NA below a list with NULLs will be formed
    for(j in v)
      r[[j]] <- sapply(z, function(x) if(j %in% names(x)) x[[j]] else NA)
    r
  }

  p <- add_annotations(p, text=~text, x=~x, y=~y, xref='x', yref='y',
                       align=~align, xanchor=~xanchor, size=~size,
                       showarrow=FALSE, data=resList(an))
  
  p <- plotly::layout(p, 
                      xaxis=list(title='', range=c(-1.04, 1.04),
                                 zeroline=FALSE, showline=FALSE,
                                 showticklabels=FALSE,
                                 showgrid=FALSE),
                      yaxis=list(title='', range=c(r[2] + 1, r[1] - 1),
                                 zeroline=FALSE, position=0.5,
                                 anchor='free', showline=TRUE, showgrid=FALSE))

  narnd <- if(length(rnd)) ', for participants not actually randomized' else ''
  cap   <- if(length(head)) head
   else paste('Cumulative number of exclusions ($y$-axis) and number of additional exclusions after exclusions placed higher', narnd, '.', sep='')
  cap <- paste(cap,
               if(sort) 'Exclusions are sorted by descending number of incremental exclusions.'
               else 'Exclusions are in the prespecified order shown in the figure.')
  cap <- if(length(sp)) 
           paste(cap, N['enrolled'], 'participants were enrolled,',
               sum(pending),
               'non-excluded participants are pending randomization, and',
               m, 'participants were excluded.')
         else
           paste(cap, N['enrolled'], 'participants were enrolled and',
               m, 'participants were excluded.')
           
  if(length(rnd)) cap <- paste(cap, sum(rnd, na.rm=TRUE),
                               'participants were randomized.')
  
  wrn1 <- wrn2 <- character(0)
  if(norig != N['enrolled'])
    wrn1 <- sprintf('<b>Note</b>: Number of observations (%s) does not equal number officially enrolled (%s).',
                    norig, N['enrolled'])
  if(n - m != N['randomized'])
    wrn2 <- sprintf('<b>Note</b>: Number of enrolled (%s) minus number excluded (%s) does not match official number randomized (%s).',
                    n, m, N['randomized'])

  cap <- paste(cap, tail, wrn1, wrn2)

  nP <- nP + 1
  R[[nP]]  <- putHcap(cap, scap='Cumulative exclusions', file=FALSE)
  Pl[[nP]] <- p

  rf <- function(x) format(round(x, 3))
  f  <- function(x) ifelse(is.na(x), '', format(x))
  tabl <- data.frame(elab      = c(htmlTranslate(elab), mu$bold('Total')),
                     nexclude  = c(nexclude, m),
                     marg      = c(marg, NA),
                     frac      = rf(c(nexclude / n, m / n)),
                     frace     = rf(c(nexclude / m, 1)),
                     fracremain= rf(c(fracremain, (n - m) / n)),
                     row.names = 1 : (length(elab) + 1),
                     stringsAsFactors=FALSE)

  cap <- c(mu$italics('Incremental exclusions'),
           'are those in addition to exclusions in earlier rows.',
           mu$italics('Marginal exclusions'),
           'are numbers of participants excluded for the indicated reason',
           'whether or not she was excluded for other reasons.  The three',
           mu$italics('Fractions'), 'are based on incremental exclusions.', tail)

  coafter <- integer(0)    ## table row numbers after which to add lines spanning all columns
  cotext  <- character(0)
  for(i in 1 : nrow(tabl)) {
    cn <- cadd[i]
    co <- ''
    if(cn %in% names(Xc)) {
      sx <- sum(Xc[, cn], na.rm=TRUE)
      w <- paste(mu$lspace, mu$frac(nexcludec[i], sx), '=',
                 rf(nexcludec[i] / sx), 'of', htmlTranslate(Xclab[cn]))
      co <- if(co == '') w else paste0(co, '<br>', w)
    }
    if(co != '') {
      coafter <- c(coafter, i)
      cotext  <- c(cotext, co)
      }
  }

  w <- htmlTable(tabl, align='lrrrrr', rnames=FALSE,
                 header=c('Exclusions', 'Incremental<br>Exclusions',
                          'Marginal<br>Exclusions',
                          'Fraction of<br>Enrolled',
                          'Fraction of<br>Exclusions',
                          'Fraction<br>Remaining'),
                 css.cell='min-width: 6em;',
                 tspanner=c('', cotext),
                 n.tspanner=diff(c(0, coafter, nrow(tabl))),
                 css.tspanner.sep='', css.tspanner = "text-align: left;")
  nP <- nP + 1
  R[[nP]]  <- putHcap(cap, scap='Exclusions', table=TRUE, file=FALSE)
  Pl[[nP]] <- w
    
  ## If needed, display subjects marked as randomized who are marked as
  ## meeting exclusion criteria
  if(length(rnd) && length(nexr)) {
    cap   <- 'Frequency of exclusions for participants marked as randomized'
    scap  <- 'Exclusions in randomized participants'
    z <- htmlTable(E, rnames=FALSE, align='lr')
    nP <- nP + 1
    R[[nP]]  <- putHcap(cap, scap=scap, table=TRUE, file=FALSE)
    Pl[[nP]] <- z
    
    if(details && length(Ids)) {
      if(length(detailTail)) detailTail <- paste('.', detailTail)
#      cap <- paste0('Participant IDs for those randomized with exclusions', detailTail)
      
      le <- length(nexr) - 1
      idtable <- data.frame(Exclusion=E$Exclusion[1 : le], IDs=Ids[1 : le])
      z <- htmlTable(idtable, align='ll', rnames=FALSE)
      z <- mu$expcolld('Click to show participant IDs', z)
      nP <- nP + 1
      R[[nP]] <- ''
      Pl[[nP]] <- htmltools::HTML(z)
      
      if(length(erdata)) {
        erd <- erdata[as.character(interaction(erdata[Idnames]))
                        %in% Idso, ]
        z <- htmlTable(erd, rnames=FALSE)
        z <- mu$expcolld('Click to see more information about those participants', z)
        nP <- nP + 1
        R[[nP]] <- ''
        Pl[[nP]] <- htmltools::HTML(z)
      }
    }
  }
  mdchunk(R, Pl)
  invisible()
}
