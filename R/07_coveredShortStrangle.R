#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven (BE)point at expiration for Covered Short Strangle and draws its graph in the Plots tab.
#'@description
#'This strategy amounts to augmenting a covered call by writing an OTM put option with a higher strike price (XL) and the same time to maturity (TTM) as the sold call option (whose strike price is XH) and thereby increasing the income. Breakeven (BE) point of Covered Short Strangle varies depending on the relationship between the stock price, premiums received, and the strikes (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Covered Short Strangle and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price
#'@param XL Lower Strike Price or eXercise price.
#'@param XH Higher Strike Price or eXercise price.
#'@param CXH Call Premium received on shorted higher Strike call.
#'@param PXL Put Premium received on shorted lower Strike Put.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Sub title of the Graph.
#'@return returns a profit and loss graph of Covered Short Strangle.
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
#'coveredShortStrangle(17,21,12,1.44,3.56,15.84)
#'coveredShortStrangle(50,55,45,4,11,48,hl=0.7,hu=1.2)
#'coveredShortStrangle(1000,1010,990,10,24,990,hl=0.97,hu=1.02)
#'@export
coveredShortStrangle <- function (ST,XH,XL,CXH,PXL,S0,hl=0,hu=1.6,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Covered Short Strangle ", sub="bullishTrader / MaheshP Kumar"){
  V0Cr=CXH+PXL
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (myData$spot-S0)-pmax((myData$spot-XH),0)-pmax((XL-myData$spot),0)+ V0Cr
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  text((XL+3),1, labels="BE Point Varies", adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = XL,col = "gray",lty=5,lwd=1.25)
  abline(v = XH,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}

