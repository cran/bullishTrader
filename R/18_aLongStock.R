#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point of a long position in a stock, at the time of sale of the held stock, and draws its graph in the Plots tab.
#'@description
#'Long stock is owning and holding the stock on the expectation that its price will rise (Hull, 2022).
#'@details
#'This method is developed, and the given examples are created, to compute per share Profit and Loss at the time of the sale of a held stock and also the Breakeven (BE) point for a long stock position and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price
#'@param C Transaction Cost.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Covered Call.
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
#'Hull, J. C. (2022). Options, Futures, and Other Derivatives (11th ed.). Pearson Education (US). https://bookshelf.vitalsource.com/books/9780136940043.
#'@examples
#'aLongStock(20,18)
#'aLongStock(90,85,hl=0.7,hu=1.2)
#'aLongStock(1000,990,hl=0.97,hu=1.02)
#'aLongStock(1000,990,5,hl=0.97,hu=1.02)
#'@export
aLongStock <- function (ST,S0,C=0,hl=0,hu=1.8,xlab="Spot Price ($) on Expiration",ylab="Realized Profit / Loss [ PnL ] upon Sale ($)",main="Long Stock ", sub="bullishTrader / MaheshP Kumar"){
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (myData$spot-S0)- C
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  points(x=(ST=S0+C), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text((ST=S0+C),1, labels=(ST=S0+C), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = S0,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
