% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exReport.r
\name{exReport}
\alias{exReport}
\title{Exclusion Report}
\usage{
exReport(
  formula,
  data = NULL,
  subset = NULL,
  na.action = na.retain,
  study = " ",
  ignoreExcl = NULL,
  ignoreRand = NULL,
  plotExRemain = TRUE,
  autoother = FALSE,
  sort = TRUE,
  whenapp = NULL,
  erdata = NULL,
  head = NULL,
  tail = NULL,
  detailTail = NULL,
  details = TRUE
)
}
\arguments{
\item{formula}{a formula with only a right-hand side, possibly containing a term of the form \code{pending(x)} to inform the function of which subjects have incomplete randomization ("randomization pending").  The \code{pending} variable is ignored if a subject has an exclusion marked.  A \code{randomized} variable is an optional \code{logical} vector specifying which subjects are considered to have been randomized.  The presence of this variable causes consistency checking against exclusions.  One or more \code{cond} variables provide binary/logical vectors used to define subsets of subjects for which denominators are used to compute additional fractions of exclusions that are reported in a detailed table.  The arguments of the \code{cond} function are the name of the original variable (assumed to be provided as a regular variable in \code{formula}, a single character string giving the label for the condition, and the vector of essentially binary values that specify the condition.}

\item{data}{input data frame}

\item{subset}{subsetting criteria}

\item{na.action}{function for handling \code{NA}s when creating analysis frame}

\item{study}{character string identifying the study; used in multi-study reports or where distinct patient strata are analyzed separately.  Used to fetch the study-specific metadata stored by \code{\link{sethreportOption}}.  Single study reports just use \code{study=' '}.}

\item{ignoreExcl}{a formula with only a right-hand side, specifying the names of exclusion variable names that are to be ignored when counting exclusions (screen failures)}

\item{ignoreRand}{a formula with only a right-hand side, specifying the names of exclusion variable names that are to be ignored when counting randomized subjects marked as exclusions}

\item{plotExRemain}{set to \code{FALSE} to suppress plotting a 2-panel dot plot showing the number of subjects excluded and the fraction of enrolled subjects remaining}

\item{autoother}{set to \code{TRUE} to add another exclusion \code{Unspecified} that is set to \code{TRUE} for non-pending subjects that have no other exclusions}

\item{sort}{set to \code{FALSE} to not sort variables by descending exclusion frequency}

\item{whenapp}{a named character vector (with names equal to names of variables in formula).  For each variable that is only assessed (i.e., is not \code{NA}) under certain conditions, add an element to this vector naming the condition}

\item{erdata}{a data frame that is subsetted on the combination of \code{id} variables when \code{randomized} is present, to print auxiliary information about randomized subjects who have exclusion criteria}

\item{head}{character string.  Specifies initial text in the figure caption, otherwise a default is used.}

\item{tail}{a character string to add to end of automatic caption}

\item{detailTail}{a character string to add to end of automatic caption for appendix table with listing of subject IDs}

\item{details}{set to \code{FALSE} to prevent writing details about exclusions (IDs, etc.)}
}
\description{
Generates graphics for sequential exclusion criteria
}
\details{
With input being a series of essentially binary variables with positive indicating that a subject is excluded for a specific reason, orders the reasons so that the first excludes the highest number of subjects, the second excludes the highest number of remaining subjects, and so on.  If a randomization status variable is present, actually randomized (properly or not) subjects are excluded from counts of exclusions.  First draws a single vertical axis graph showing cumulative exclusions, then creates a 2-panel dot chart with the first panel showing that information, along with the marginal frequencies of exclusions and the second showing the number of subjects remaining in the study after the sequential exclusions.  A pop-up table is created showing those quantities plus fractions.  There is an option to not sort by descending exclusion frequencies but instead to use the original variable order.  Assumes that any factor variable exclusions that have only one level and that level indicates a positive finding, that variable has a denominator equal to the overall number of subjects.

An attribute dot chart is also drawn using the Hmisc package \code{combplotp} function, showing frequencies of all combinations of exclusions that occurred in the data.
}
\examples{
# See test.Rnw in tests directory
}
\author{
Frank Harrell
}
