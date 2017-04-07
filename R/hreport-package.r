#' Interactive Graphical HTML Reports for Clinical Trials
#'
#' @author Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' @maintainer Frank E Harrell Jr \email{f.harrell@@vanderbilt.edu}
#'
#' @export Merge accrualReport dNeedle dReport eReport endPlot exReport gethreportOption nriskReport putFig sampleFrac sethreportOption startPlot survReport
#' @import Hmisc plotly (>= 4.5.2) ggplot2 lattice data.table methods
#' @importFrom rms npsurv survplot
#' @importFrom survival Surv survfit
#' @importFrom Formula Formula model.part
#' @importFrom grDevices adjustcolor gray
#' @importFrom graphics abline axis box grconvertX grconvertY lines par plot plot.new points text
#' @importFrom base64 img
#' @importFrom stats as.formula median model.frame qnorm reshape sd terms ecdf
#' @importFrom htmltools HTML
#' @docType package
#' @aliases hreport package-hreport
#' @name hreport

# The caching and check for conflicts require looking for a pattern of objects; the search may be avoided by defining an object .noGenerics
# see ?library
.noGenerics <- TRUE
