#' Accrual Report
#'
#' Generate graphics and HTML to analyze subject accrual
#'
#' Typically the left-hand-side variables of the formula, in order, are date of enrollment and date of randomization, with subjects enrolled but not randomized having missing date of randomization.  Given such date variables, this function generates cumulative frequencies optionally with target enrollment/randomization numbers and with time-zooming.  Makes a variety of dot charts by right-hand-side variables:  number of subjects, number of sites, number of subjects per site, fraction of enrolled subjects randomized, number per month, number per site-month.
#'
#' @param formula formula object, with time variables on the left (separated by +) and grouping variables on the right.  Enrollment date, randomization date, region, country, and site when present must have the variables in parenthesis preceeded by the key words \code{enrollment, randomize, region, country, site}.
#' @param data data frame.
#' @param subset a subsetting epression for the entire analysis.
#' @param na.action a NA handling function for data frames, default is \code{na.retain}.
#' @param dateRange \code{Date} or character 2-vector formatted as \code{yyyy-mm-dd}.  Provides the range on the \code{x}-axis.
#' @param targetN integer vector with target sample sizes over time, same length as \code{targetDate}
#' @param targetDate \code{Date} or character vector corresponding to \code{targetN}
#' @param closeDate \code{Date} or characterstring.  Used for randomizations per month and per site-month - contains the dataset closing date to be able to compute the number of dates that a group (country, site, etc.) has been online since randomizating its first subject.
#' @param enrollmax numeric specifying the upper y-axis limit for cumulative enrollment when not zoomed
#' @param studynos logical.  Set to \code{FALSE} to suppress summary study numbers table.
#' @param minrand integer.  Minimum number of randomized subjects a country must have before a box plot of time to randomization is included.
#' @param panel character string.  Name of panel, which goes into file base names and figure labels for cross-referencing.
#' @param h numeric.  Height of ordinary plots, in inches.
#' @param w numeric.  Width of ordinary plots.
#' @param hb numeric.  Height of extended box plots.
#' @param wb numeric.  Weight of extended box plots.
#' @param hdot numeric.  Height of dot charts in inches.
#' @export
#' @examples
#' \dontrun{
#' # See test.Rmd in inst/tests directory
#' }

accrualReport <-
  function(formula, data=NULL, subset=NULL, na.action=na.retain,
           dateRange=NULL, zoom=NULL, targetN=NULL, targetDate=NULL,
           closeDate=NULL, enrollmax=NULL, studynos=TRUE,
           minrand=10, panel = 'accrual',
           h=2.5, w=3.75, hb=5, wb=5, hdot=3.5)
{
  formula <- Formula(formula)
  
  if(grepl('[^a-zA-Z-]', panel))
    stop('panel must contain only A-Z a-z -')
  
  environment(formula) <- new.env(parent = environment(formula))
  en <- environment(formula)
  f <- function(x) x
  assign(envir = en, "enroll",    f)
  assign(envir = en, "randomize", f)
  assign(envir = en, "region",    f)
  assign(envir = en, "country",   f)
  assign(envir = en, "site",      f)

  ned <- function(used) {
    sf <- sampleFrac(used)
    structure(dNeedle(sf), table=attr(sf, 'table'))
  }
  extra <- function(x) c(attr(x, 'table'), x)

  mdchunk <- markupSpecs$html$mdchunk   # in Hmisc

  lhs  <- terms(formula, lhs=1, specials=c('enroll', 'randomize'))
  sl   <- attr(lhs, 'specials')
  rhs  <- terms(formula, rhs=1, specials=c('region', 'country', 'site'))
  sr   <- attr(rhs, 'specials')

  Y <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.keep)
   else model.frame(formula, data=data, na.action=na.keep)
  X    <- model.part(formula, data=Y, rhs=1)
  Y    <- model.part(formula, data=Y, lhs=1)
  nY   <- NCOL(Y)
  nX   <- NCOL(X)
  namY <- all.vars(lhs)
  namX <- all.vars(rhs)
  enroll    <- sl$enroll
  randomize <- sl$randomize

  z <- function(x, nY) if(length(x)) x - nY else NULL
  ## specials counts from lhs variables
  region    <- z(sr$region,  nY)
  country   <- z(sr$country, nY)
  site      <- z(sr$site,    nY)

  penroll    <- length(enroll)    > 0
  prandomize <- length(randomize) > 0
  pregion    <- length(region)    > 0
  pcountry   <- length(country)   > 0
  psite      <- length(site)      > 0
  pclose     <- length(closeDate) > 0
  
  cr <- pcountry || pregion
  byl <- if(! (pregion | pcountry)) 'site'
         else
           if(pregion & pcountry) 'region and country'
         else
           if(pregion) 'region'
         else
           'country'
  
  dr <- dateRange
  if(!length(dr))
    dr <- range(pretty(do.call('range', c(as.list(Y), na.rm=TRUE))))
  else dr <- as.Date(dr)
  if(length(targetN) && ! length(targetDate))
    stop('must provide targetDate if using targetN')
  if(length(targetDate)) targetDate <- as.Date(targetDate)
  if(pclose)  closeDate  <- as.Date(closeDate)

  ylabs <- namY
  for(i in 1 : nY) {
    if(penroll    && enroll == i)    ylabs[i] <- 'enrolled'
    if(prandomize && randomize == i) ylabs[i] <- 'randomized'
  }
  xlabs <- namX
  for(i in 1 : nX) {
    if(pregion  && region == i)  xlabs[i] <- 'region'
    if(pcountry && country == i) xlabs[i] <- 'country'
    if(psite    && site == i)    xlabs[i] <- 'site'
  }

  z <- k <- character(0)
  g <- function(x, digits) as.character(round(x, digits))
  if(pcountry) {
    z <- g(length(unique(X[[country]])), 0)
    k <- 'Countries'
  }
  if(psite) {
    Site <- as.character(X[[site]])
    nsites <- length(unique(Site))
    z <- c(z, g(nsites, 0))
    k <- c(k, 'Sites')
  }
  if(penroll) {
    z <- c(z, sum(! is.na(Y[[enroll]])))
    k <- c(k, 'Participants enrolled')
  }

  if(psite && prandomize) {
    rdate    <- Y[[randomize]]
    nrand    <- sum(! is.na(rdate))
    persite  <- nrand / nsites
    nsitesr  <- length(unique(Site[! is.na(rdate)]))
    persiter <- nrand / nsitesr
    z <- c(z, c(nrand, g(persite, 1), nsitesr, g(persiter, 1)))
    k <- c(k, c('Participants randomized', 'Participants per site',
                'Sites randomizing',
                'Subjects randomized per randomizing site'))
    ## maxs = for each site the # months since that site first randomized
    ##        a subject (NA if none randomized)
    ## site months is sum of maxs
    ## avg. months since first randomized = mean maxs excluding NAs
    ## rand per site per month = # rand / site months
    ## Note: # rand / # sites / avg. months != rand per site per month
    ## because some sites have not randomized any subjects.  Such sites
    ## are counted in # sites but not in site-months
    if(pclose) {
      months <- as.numeric(difftime(closeDate, rdate, units='days')) /
        (365.25 / 12)
      mx <- function(x) if(! length(x) || all(is.na(x))) NA
       else max(x, na.rm=TRUE)
      maxs       <- tapply(months, Site, mx)
      sitemonths <- sum(maxs, na.rm=TRUE)
      z <- c(z, g(max(months, na.rm=TRUE), 1),
                g(sitemonths, 1),
                g(mean(maxs, na.rm=TRUE), 1),
                g(nrand / sitemonths, 2))
      k <- c(k, paste('Months from first subject randomized (',
                      format(min(rdate, na.rm=TRUE)), ') to ',
                      format(closeDate), sep=''),
                'Site-months for sites randomizing',
                'Average months since a site first randomized',
                'Participants randomized per site per month')
    }
  }

  if(penroll && prandomize) {
    ttr <- as.numeric(difftime(Y[[randomize]], Y[[enroll]], units='days'))
    z <- c(z, g(mean(ttr, na.rm=TRUE), 1))
    k <- c(k, 'Mean days from enrollment to randomization')
    z <- c(z, g(median(ttr, na.rm=TRUE), 1))
    k <- c(k, 'Median days from enrollment to randomization')
  }
  
  if(studynos && length(z)) {
    z <- data.frame(Number=z, Category=k)
    html(z, file='', align = c('r','l'), border=1,
         caption='Study Numbers')
  }
  
  ## For each date variable in Y, make a cumulative frequency chart
  ## If target sample size is present, add that as line graph to chart
  options(grType='plotly')

  R  <- list()
  Pl <- list()
  nP <- 0
  
  for(j in 1 : nY) {
    y   <- Y[[j]]
    nam <- namY[j]
    lab <- ylabs[j]
    sumnna <- sum(! is.na(y))
    y <- y[! is.na(y)]
    target <- if(! length(names(targetN))) targetN else targetN[[nam]]
    dtarget <- targetDate
    if(length(target) && min(target) > 0) {
      target <- c(0, target)
      dtarget <- c(dr[1], dtarget)
    }

    shortcap <- sprintf("Participants %s over time", lab)
    cap <- if(length(target))
             sprintf('The blue line depicts the cumulative frequency.  The thick grayscale line represent targets.', lab) else ''

    ## Interpolate some points for target just so hover text can have
    ## more resolution than at polygon bends
    p <- plot_ly()
    if(length(target)) {
      if(length(target) < 15) {
        dtarget2 <- seq(min(dtarget), max(dtarget), by='week')
        target   <- round(approx(dtarget, target, xout=dtarget2)$y)
        dtarget  <- dtarget2
      }
      p <- add_lines(p, x=~ dtarget, y=~ target, mode='lines',
                     line=list(color='lightgray', width=4),
                     name='Target')
    }

    tab <- table(y)
    cumfreq <- unname(cumsum(tab))
    dates   <- as.Date(names(tab))
    p <- add_lines(p, x=~dates, y=~cumfreq, mode='lines',
                   line=list(color='blue', width=1),
                   name='Actual')

    ymax <- if(length(target)) max(length(y), target)
            else if(lab == 'enrolled' && length(enrollmax)) enrollmax
            else length(y)
    p <- plotly::layout(p,
                        xaxis=list(title=paste('Date', upFirst(lab))),
                        yaxis=list(title='Cumulative Number',
                                   range=c(0, ymax)))
    
    needle <- ned(switch(lab,
                         enrolled   = c(enrolled=sumnna),
                         randomized = c(enrolled=sumnna, randomized=sumnna)))
    nP <- nP + 1
    R[[nP]] <- putHcap(cap, scap=shortcap, extra=extra(needle), file=FALSE)
    Pl[[nP]] <- p
  }

  ## Extended box plots of time to randomization for randomized subjects
  if(penroll && prandomize && (pregion || pcountry)) {
    x1 <- if(pregion)  X[[region]]
    x2 <- if(pcountry) X[[country]]
    y <- as.numeric(difftime(Y[[j]], Y[[enroll]], units='days'))
    use <- TRUE
    coexcl <- 0
    if(pcountry && minrand > 0) {
      ## Exclude countries randomizing fewer than minrand subject
      nrn <- tapply(y, x2, function(x) sum(! is.na(x)))
      if(any(nrn < minrand)) {
        coexcl <- sum(nrn < minrand)
        countrieskeep <- names(nrn)[nrn >= minrand]
        use <- x2 %in% countrieskeep
      }
    }

#    if(length(x1) > 0 && length(x2) > 0)
#      warning('at present not implemented for 2 stratification variables')

    xx <- if(length(x1)) x1 else x2
    f <- if(length(xx)) summaryM(y ~ xx) else summaryM(y ~ 1)

    form <- if(length(x1) && length(x2)) y ~ x1 + x2
     else if(length(x1)) y ~ x1
     else if(length(x2)) y ~ x2
     else y ~ 1

    h <- function(x) {
      x <- x[! is.na(x)]
      a <- quantile(x, (1 : 3) / 4)
      r <- c(mean(x), a, length(x))
      names(r) <- c('Mean', 'Q<sub>1</sub>', 'Median', 'Q<sub>3</sub>', 'N')
      r
    }
    sym <- c('circle', 'line-ns-open', 'cross', 'line-ns-open')
    p <- summaryD(form, fun=h, auxvar='N', symbol=sym,
                  col='blue', height='auto',
                  xlab='Days to Randomization',
                  legendgroup=c('Mean', 'Quartiles', 'Median', 'Quartiles'))

    excc <- if(coexcl > 0) paste('.', coexcl, 'countries with fewer than',
                                 minrand, 'randomized participants are not shown.')
            else ''

    nP <- nP + 1
    R[[nP]] <- putHcap(
      'Quartiles and mean number of days by', byl, excc,
      scap = 'Days from enrollment to randomization',
      extra=extra(needle), file=FALSE)
    Pl[[nP]] <- p
  }

  ## Chart number of subjects enrolled/randomized/... and other descriptors
  ## by right-hand variables
 if(nX == 0) {
   mdchunk(R, Pl)
   return(invisible())
   }

  if(psite) {
    P <- vector('list', nY)
    for(j in 1 : nY) {
      y <- X[[site]]
      y[is.na(Y[[j]])] <- NA
      lab  <- ylabs[j]
      clab <- capitalize(lab)
      nn   <- table(table(y))
      p <- plot_ly(x = as.numeric(names(nn)),
                   y = as.numeric(nn), mode='markers',
                   name=clab,
                   width=850, height=350)
      P[[j]] <-
        plotly::layout(p,
                       xaxis=list(title=paste('Participants', clab),
                                  zeroline=FALSE),
                       yaxis=list(title='Number of Sites'))
    }
    p <- plotly::subplot(P, titleY=TRUE, titleX=TRUE,
                         shareY=TRUE, nrows=1, margin=.05)
    if(nY > 1) lab <- ''
    nP <- nP + 1
    R[[nP]] <- putHcap(
      'Number of sites having the given number of participants', lab,
      scap=paste0('Number of sites &times; number of participants', lab),
      extra=extra(needle), file=FALSE)
    Pl[[nP]] <- p
  }

  ## Start with counts of subjects by non-site grouping variables
  ## Compute number of non-site right-hand variables
  ns <- setdiff(1 : nX, site)
  dat <- list()
  if(pregion)  dat$x1 <- X[[region]]
  if(pcountry) dat$x2 <- X[[country]]
  if(psite)    dat$x3 <- X[[site]]
  if(length(ns) > 2) {
    more <- setdiff(ns, c(region, country))
    k <- 2
    for(l in more) {
      k <- k + 1
      dat[[paste('x', k, sep='')]] <- X[[l]]
    }
  }
  form <- if(length(ns)) {
    xvars <- paste(paste('x', 1 : length(ns), sep=''), collapse=' + ')
    paste('y ~', xvars)
  } else if(psite) 'y ~ x3'
  else 'y ~ 1'
  form <- as.formula(form)

  by <- paste(xlabs[ns], collapse=' and ')
  types <- c('count',
             if(psite && cr)                'sites',
             if(penroll    && prandomize)   'fracrand',
             if(prandomize && pclose && cr) 'permonth',
             if(prandomize && pclose && psite && cr) 'persitemonth')

  np <- nY * sum(c('count', 'sites') %in% types) +
             sum(c('fracrand', 'permonth', 'persitemonth') %in%
                 types)
  scap <- if(psite) 'Subject and site counts' else 'Subject counts'
  cap  <- if(psite) 'Counts of numbers of participants and numbers of sites'
   else 'Counts of numbers of participants'
  for(type in types) {
    whichy <- if(type %in% c('fracrand', 'permonth', 'persitemonth'))
      randomize else 1 : nY
    if(length(whichy)) for(j in whichy) {
      gg <- function(x) length(unique(x[! is.na(x)]))
      
      if(type %in% c('permonth', 'persitemonth')) {
        ## Get country if there, otherwise region
        group <- if(pcountry) as.character(X[[country]])
         else    if(pregion)  as.character(X[[region]])

        ## Get more major grouping if present otherwise use above
        mgroup <- if(pregion) as.character(X[[region]]) else group

        ## Get enrollment date if present, otherwise use rand. date
        k <-  if(penroll)     enroll
         else if(prandomize)  randomize
         else 1
        months <- as.numeric(difftime(closeDate, Y[[k]], units='days')) /
          (365.25 / 12)

        ## Find maximum months on board for each group
        ## E.g. longest elapsed time within a country
        gmonths <- tapply(months, group, max, na.rm=TRUE)

        ## Find the maximum elapsed time over groups within major groups
        ## E.g. longest time for any country within that region
        mmonths <- tapply(months, mgroup, max, na.rm=TRUE)

        ## Create a major group lookup object given group
        tab <- subset(as.data.frame(table(group, mgroup)), Freq > 0)
        mg        <- as.character(tab$mgroup)
        names(mg) <- as.character(tab$group)

        ## For site-month calculation compute the maximum elapsed time
        ## per site, then sum that over all sites within a group
        ## Assume sites are unique over countries, regions
        if(type == 'persitemonth') {
          ## For each site lookup group
          tab <- subset(as.data.frame(table(Site, group)), Freq > 0)
          gr        <- as.character(tab$group)
          names(gr) <- as.character(tab$Site)
          maxs <- tapply(months, Site, max, na.rm=TRUE)

          ## Starting with only one record per site with that site's
          ## maximum time, sum the elapsed months within each group
          gsitesum <- tapply(maxs, gr[names(maxs)], sum, na.rm=TRUE)

          ## Similar over region
          msitesum <- tapply(maxs, mg[gr[names(maxs)]], sum, na.rm=TRUE)

          ## Spread to all subjects
          y <- cbind(randomized = ! is.na(Y[[randomize]]),
                     mmonths    = msitesum[mg[group]],
                     gmonths    = gsitesum[group])
          yy <- y[group == 'US',]
        }
          else y <- cbind(randomized = ! is.na(Y[[randomize]]),
                          mmonths    = mmonths[mg[group]],
                          gmonths    = gmonths[group])
        mg <- function(y) sum(y[, 1]) / y[1, 2]
        gg <- function(y) sum(y[, 1]) / y[1, 3]
      }

      switch(type,
             count = { y <- ! is.na(Y[[j]]); fun <- sum },
             sites = { y <- X[[site]]; y[is.na(Y[[j]])] <- NA; fun <- gg },
             fracrand     = { y <- ! is.na(Y[[j]]); fun <- mean },
             permonth     = { },
             persitemonth = { } )

      lab <- ylabs[j]
      clab <- capitalize(lab)
      dat$y <- y
      p <- if(type %in% c('permonth', 'persitemonth'))
             summaryD(form, fun=gg, funm=mg, data=dat,
                      height='auto',
                 xlab=switch(type,
                   permonth     = 'Number Randomized Per Month',
                   persitemonth = 'Number Randomized Per Site Per Month'))
      else summaryD(form, fun=fun, data=dat, height='auto',
                    ylab = if(psite && ! length(ns)) 'Site',
                    xlab=switch(type,
                      count=sprintf('Number of Participants %s',      clab),
                      sites=sprintf('Number of Sites That %s',    clab),
                      fracrand=sprintf('Fraction of Participants %s', clab)))

        cap <- switch(type,
                      count = paste('Participants', lab),
                      sites = paste('Sites that', lab),
                      fracrand = 'Fraction of enrolled participants randomized',
                      permonth = paste('Participants', lab, 'per month'),
                      persitemonth = paste('Partipants', lab,
                                           'per site per month'))

      cap <- paste(cap, 'by', byl)
      nP <- nP + 1
      R[[nP]] <- putHcap(cap, extra=extra(needle), file=FALSE)
      Pl[[nP]] <- p
      }
  }
  mdchunk(R, Pl)
  invisible()
}
