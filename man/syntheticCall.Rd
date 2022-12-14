% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/15_syntheticCall.R
\name{syntheticCall}
\alias{syntheticCall}
\title{Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point at expiration for Synthetic Call and draws its graph in the Plots tab.}
\usage{
syntheticCall(
  ST,
  X,
  P,
  S0,
  hl = 0,
  hu = 1.5,
  xlab = "Spot Price ($) on Expiration",
  ylab = "Profit / Loss [ PnL ] at Expiration ($)",
  main = "Synthetic Call/ Married put/ Protective put ",
  sub = "bullishTrader / MaheshP Kumar"
)
}
\arguments{
\item{ST}{Spot Price at time T.}

\item{X}{Strike Price or eXercise price.}

\item{P}{Put Premium paid on bought Put.}

\item{S0}{Initial Stock Price}

\item{hl}{lower bound value for setting lower limit of X axis displaying spot price.}

\item{hu}{upper bound value for setting upper limit of X axis displaying spot price.}

\item{xlab}{X axis label.}

\item{ylab}{Y axis label.}

\item{main}{Title of the Graph.}

\item{sub}{Subtitle of the Graph.}
}
\value{
returns a profit and loss graph of Synthetic Call.
}
\description{
This strategy is also known as married put or protective put and results from buying stock and an ATM or OTM put option with a strike price X is less than or equal to S0. The trader or investor has bullish outlook. This is a hedging strategy, the put option hedges the risk of the stock price falling (Kakushadze & Serur, 2018).
}
\details{
According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and Breakeven (BE) point for Synthetic Call and draws its graph in the Plots tab.
}
\examples{
syntheticCall(35,35,2.55,35.50)
syntheticCall(15,15,1.46,15.84,hl=0.6,hu=1.4)
syntheticCall(1000,1000,20,990,hl=0.98,hu=1.035)
}
\references{
Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
