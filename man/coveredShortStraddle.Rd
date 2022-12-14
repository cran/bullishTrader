% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_coveredShortStraddle.R
\name{coveredShortStraddle}
\alias{coveredShortStraddle}
\title{Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point at expiration for Covered Short Straddle and draws its graph in the Plots tab.}
\usage{
coveredShortStraddle(
  ST,
  X,
  C,
  P,
  S0,
  hl = 0,
  hu = 1.5,
  xlab = "Spot Price ($) on Expiration",
  ylab = "Profit / Loss [ PnL ] at Expiration ($)",
  main = "Covered Short Straddle ",
  sub = "bullishTrader / MaheshP Kumar"
)
}
\arguments{
\item{ST}{Spot Price at time T.}

\item{X}{Strike Price or eXercise price.}

\item{C}{Call Premium received on shorted call.}

\item{P}{Put Premium received on shorted Put.}

\item{S0}{Initial Stock Price}

\item{hl}{lower bound value for setting lower limit of X axis displaying spot price.}

\item{hu}{upper bound value for setting upper limit of X axis displaying spot price.}

\item{xlab}{X axis label.}

\item{ylab}{Y axis label.}

\item{main}{Title of the Graph.}

\item{sub}{Subtitle of the Graph.}
}
\value{
returns a profit and loss graph of Covered Short Straddle.
}
\description{
This strategy amounts to augmenting a covered call by writing a put option with the same strike price(X) and time-to-maturity (TTM) as the sold call option and thereby increasing the income (Kakushadze & Serur, 2018).
}
\details{
According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and Breakeven (BE) point for Covered Short Straddle and draws its graph in the Plots tab.
}
\examples{
coveredShortStraddle(17,17,1.44,3.56,15.84)
coveredShortStraddle(50,50,4,11,48,hl=0.7,hu=1.2)
coveredShortStraddle(1000,1000,10,33,990,hl=0.97,hu=1.015)
}
\references{
Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. \cr
Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
