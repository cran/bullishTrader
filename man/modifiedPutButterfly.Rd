% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/11_modifiedPutButterflyGraph.R
\name{modifiedPutButterfly}
\alias{modifiedPutButterfly}
\title{Calculates Profit and Loss (PnL) per share (or unit of the underlying) at expiration and Breakeven (BE) point for Modified Put Butterfly and draws its graph in the Plots tab.}
\usage{
modifiedPutButterfly(
  ST,
  X1L,
  X2M,
  X3H,
  P1XL,
  PX2M,
  PX3H,
  hl = 0.8,
  hu = 1.3,
  xlab = "Spot Price ($) at Expiration",
  ylab = " Profit / Loss [ PnL ] at Expiration ($)",
  main = "Modified Put Butterfly ",
  sub = "bullishTrader / MaheshP Kumar"
)
}
\arguments{
\item{ST}{Spot Price at time T.}

\item{X1L}{Lower Strike Price or eXercise price.}

\item{X2M}{Medium Strike Price or eXercise price.}

\item{X3H}{Higher Strike Price or eXercise price.}

\item{P1XL}{Put Premium Paid for the bought Put at Lower Strike.}

\item{PX2M}{Put Premium received from the sold Put at Medium Strike.}

\item{PX3H}{Put Premium paid for the bought Put at higher Strike .}

\item{hl}{lower bound value for setting lower limit of X axis displaying spot price.}

\item{hu}{upper bound value for setting upper limit of X axis displaying spot price.}

\item{xlab}{X axis label.}

\item{ylab}{Y axis label.}

\item{main}{Title of the Graph.}

\item{sub}{Subtitle of the Graph.}
}
\value{
Returns a graph Modified Put Butterfly.
}
\description{
This is a variation of the long put butterfly strategy and the strikes are no longer equidistant; instead we have X3H minus X2M is less than X2M minus X1L. This results in a sideways strategy with a bullish bias (Cohen,2015).
}
\details{
According to conceptual details and formula given by Cohen (2015), this method is developed, and the given examples are created, to compute per share Profit and Loss and Breakeven (BE) point at expiration for Modified Put Butterfly and draw its graph in the Plots tab.
}
\examples{
modifiedPutButterfly(50,45,55,60,0.98,6.12,10.28)
modifiedPutButterfly(1000,980,1020,1030,10,60,100,hl=0.97,hu=1.05)
}
\references{
Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
