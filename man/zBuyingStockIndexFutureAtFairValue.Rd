% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/17_zBuyingStockIndexFutureAtFairValue.R
\name{zBuyingStockIndexFutureAtFairValue}
\alias{zBuyingStockIndexFutureAtFairValue}
\title{Calculates  Profit and Loss at closeout of a bought Stock Index Future (like Micro E mini) and draws its graph in the Plots tab.}
\usage{
zBuyingStockIndexFutureAtFairValue(
  SIT,
  SI0,
  R,
  d,
  n,
  hl = 0.9995,
  hu = 1.006,
  xlab = "Stock Index at position closing",
  ylab = "Profit / Loss [PnL] at position closing ($)",
  main = "Long on Stock Index like Micro E mini S & P 500 ",
  sub = "bullishTrader / MaheshP Kumar"
)
}
\arguments{
\item{SIT}{Stock Index at time T.}

\item{SI0}{Stock Index Initial Value.}

\item{R}{annualized financing rate}

\item{d}{dividend yield of the index.}

\item{n}{represents days in Future like 90day Stock Index Future.}

\item{hl}{lower bound value for setting lower limit of X axis displaying spot price.}

\item{hu}{upper bound value for setting upper limit of X axis displaying spot price.}

\item{xlab}{X axis label.}

\item{ylab}{Y axis label.}

\item{main}{Title of the Graph.}

\item{sub}{Subtitle of the Graph.}
}
\value{
returns a profit and loss graph of Covered Call.
}
\description{
On initiation, this is a net debit Strategy and results in net cash outflow in the form of paying the amount of bought Stock Index Future. If the bullish outlook of the trader emerges as expected and the Stock Index Future (like Micro E mini) rises then the trader makes the profit as shown in the graph (TD Ameritrade, 2019).
}
\details{
According to the information provided by TD Ameritrade (2019) and  Hull (2022), this method is developed, and the given examples are created, to compute Profit and Loss at closing of the position in Stock Index Future (like Micro E mini) and draw its graph in the Plots tab.
}
\examples{
zBuyingStockIndexFutureAtFairValue(3700,3709,0.0275,0.02,90)
zBuyingStockIndexFutureAtFairValue(2900,2910,0.025,0.03,90,hl=0.9995,hu=1.005)
}
\references{
TD Ameritrade. (2019, July 26). Micro E-Mini Futures Contracts. YouTube.https://youtu.be/SShGjJepCdA\cr
Hull, J. C. (2022). Options, Futures, and Other Derivatives (11th ed.). Pearson Education (US). https://bookshelf.vitalsource.com/books/9780136940043.
}
\author{
MaheshP Kumar, \email{maheshparamjitkumar@gmail.com}
}
