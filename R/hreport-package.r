#' Interactive Graphical HTML Reports for Clinical Trials
#'
#' @author Frank E Harrell Jr \email{fh@fharrell.com}
#'
#' @import Hmisc plotly ggplot2 data.table methods
#' @importFrom graphics abline axis box lines par plot plot.new points text
#' @importFrom grDevices adjustcolor
#' @importFrom stats as.formula median model.frame qnorm reshape sd terms ecdf
#' @importFrom htmltools HTML
"_PACKAGE"

# The caching and check for conflicts require looking for a pattern of objects; the search may be avoided by defining an object .noGenerics
# see ?library
.noGenerics <- TRUE
