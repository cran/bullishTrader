#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point at expiration for Covered Short Straddle and draws its graph in the Plots tab.
#'@description
#'This strategy amounts to augmenting a covered call by writing a put option with the same strike price(X) and time-to-maturity (TTM) as the sold call option and thereby increasing the income (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and Breakeven (BE) point for Covered Short Straddle and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price
#'@param X Strike Price or eXercise price.
#'@param C Call Premium received on shorted call.
#'@param P Put Premium received on shorted Put.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Covered Short Straddle.
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
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group. \cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'coveredShortStraddle(17,17,1.44,3.56,15.84)
#'coveredShortStraddle(50,50,4,11,48,hl=0.7,hu=1.2)
#'coveredShortStraddle(1000,1000,10,33,990,hl=0.97,hu=1.015)
#'@export
coveredShortStraddle <- function (ST,X,C,P,S0,hl=0,hu=1.5,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Covered Short Straddle ", sub="bullishTrader / MaheshP Kumar"){
  V0Cr=C+P
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
   myData$pl <- (myData$spot-S0)-pmax((myData$spot-X),0)-pmax((X-myData$spot),0)+V0Cr
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  points(x=1/2*(S0+X-V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text(1/2*(S0+X-V0Cr),1, labels=1/2*(S0+X-V0Cr), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = X,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
