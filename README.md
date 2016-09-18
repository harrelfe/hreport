R hreport package: Graphical HTML Reports for Clinical Trials with Interactive Graphics
=======
Statisticians and statistical programmers spend a great deal of time analyzing data and producing reports for clinical trials, both for final trial reports and for interim reports for data monitoring committees.  Point and Click interfaces and copy-and-paste are now believed to be bad models for reproducible research.  Instead, there are advantages to developing a high-level language for producing common elements of reports related to accrual, exclusions, descriptive statistics, adverse events, time to event, and longitudinal data.

It is well appreciated in the statistical and graphics design communities that graphics are much better than tables for conveying numeric information.  There are thus advantages for having statistical reports for clinical trials that are almost completely graphical.   For those reviewers of clinical trial reports who insist on seeing tables, and for those who occasionally like to have tables to see "exact" figures for certain data elements, supporting tables and other numeric data can appear as popup tooltips when the mouse is hovering over a graphical element.

hreport marries R, the R Hmisc, ggplot2 and plotly packages, rmarkdown, and html
to produce reproducible clinical trial reports with a minimum of
coding.  hreport composes all figure captions and makes heavy use of
analysis file annotations such as variable labels and units of
measurement.  Some new graphical elements are introduced such as
special dot charts that replace tables, extended box plots, split
violin plots for longitudinal continuous variables, half confidence
intervals for differences, new charts for representing patient flow,
and pop-up tooltips.

Current Goals
=============
* In accrual report cumulative randomized plots add text for deficit at last recorded randomized subject
* Add Svetlana Eden's function in rreport package for graphically summarizing adverse events by major and minor categories (e.g., body system and preferred term)
* Add function similar to that in rreport for group sequential monitoring boundary presentation
* Need executable tests in tests/
* See if current tests should become vignettes


Web Sites
=============
* Overall: http://biostat.mc.vanderbilt.edu/Greport
* CRAN: http://cran.r-project.org/web/packages/greport
* Changelog: https://github.com/harrelfe/greport/commits/master
