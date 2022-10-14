#'Calculates Profit and Loss (PnL) per unit of the underlying and Breakeven point at expiration for Long Synthetic Future and draws its graph in the Plots tab.
#'@description
#'This strategy results from buying a call of the option on Future and selling a put of the option on the Future of the same strike price with the same expiration. On initiation, this is a net debit Strategy and results in net cash inflow as premium received on shorting a put of the option on Future is less than premium paid on buying a call of the option on Future (Kakushadze & Serur, 2018). This Strategy precisely mimics the long future position (Cohen, 2015).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss (per unit of the underlying)  at expiration and Breakeven (BE) point for Long Synthetic Future and draws its graph in the Plots tab.
#'@param STF  Future contract price at time T.
#'@param XF  Strike Price of Option on Future.
#'@param COF Call Premium paid on buying an Option on Future  .
#'@param POF Put premium received on shorting an Option on Future.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Long Synthetic Future.
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
#'zLongSyntheticFuture(12,14,4,2)
#'zLongSyntheticFuture(200,205,7,2,hl=0.95,hu=1.15)
#'@export
zLongSyntheticFuture <- function (STF,XF,COF,POF,hl=0,hu=2,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main=" Long Synthetic Future ", sub="bullishTrader / MaheshP Kumar"){
  V0Dr= COF-POF
  myData <- data.frame (spot = c((STF*hl):(STF*hu)))
  myData$pl <- (myData$spot- XF - V0Dr)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
   points(x=(XF+V0Dr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
   text((XF+V0Dr),1, labels=(XF+V0Dr), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = XF,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
