#' Interactive Graphical HTML Reports for Clinical Trials
#'
#' @author Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' @maintainer Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' @export accrualReport dNeedle dReport eReport exReport gethreportOption nriskReport sampleFrac sethreportOption survReport
#' @import Hmisc (>= 4.1.2) plotly (>= 4.8.0) data.table methods
#' @importFrom rms npsurv survplotp
#' @importFrom survival Surv survfit
#' @importFrom Formula Formula model.part
#' @importFrom grDevices adjustcolor gray
#' @importFrom stats as.formula median model.frame qnorm reshape sd terms ecdf
#' @importFrom htmltools HTML
#' @docType package
#' @aliases hreport package-hreport
#' @name hreport

# The caching and check for conflicts require looking for a pattern of objects; the search may be avoided by defining an object .noGenerics
# see ?library
.noGenerics <- TRUE
