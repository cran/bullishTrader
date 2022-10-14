#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point at expiration for Synthetic Call and draws its graph in the Plots tab.
#'@description
#'This strategy is also known as married put or protective put and results from buying stock and an ATM or OTM put option with a strike price X is less than or equal to S0. The trader or investor has bullish outlook. This is a hedging strategy, the put option hedges the risk of the stock price falling (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and Breakeven (BE) point for Synthetic Call and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price
#'@param X Strike Price or eXercise price.
#'@param P Put Premium paid on bought Put.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Synthetic Call.
#'@importFrom graphics abline
#'@importFrom graphics points
#'@importFrom graphics text
#'@importFrom graphics lines
#'@importFrom graphics par
#'@importFrom graphics plot
#'@importFrom graphics legend
#'@importFrom graphics axis
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'syntheticCall(35,35,2.55,35.50)
#'syntheticCall(15,15,1.46,15.84,hl=0.6,hu=1.4)
#'syntheticCall(1000,1000,20,990,hl=0.98,hu=1.035)
#'@export
syntheticCall <- function (ST,X,P,S0,hl=0,hu=1.5,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Synthetic Call/ Married put/ Protective put ", sub="bullishTrader / MaheshP Kumar"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- ((myData$spot-S0) + pmax((X-myData$spot),0)-P)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
   points(x=(S0+P), y=0,cex = 2, pch = 23, col ="red",bg="gold")
   text((S0+P)-P/3,1, labels=(S0+P), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
