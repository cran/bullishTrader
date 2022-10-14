#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point (BE) at expiration for Collar Strategy and draws its graph in the Plots tab.
#'@description
#'This strategy is a covered call augmented by a long put option as insurance against the stock price falling. It amounts to buying stock, buying an OTM put option with a strike price XL, and selling an OTM call option with a higher strike price XH. The outlook of trader or investor is moderately bullish (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and also the Breakeven (BE) point for Collar Strategy and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param S0 Initial Stock Price.
#'@param XH Higher Strike Price or eXercise price.
#'@param XL Lower Strike Price or eXercise price.
#'@param PXL Call Premium paid for the bought Calls at Lower Strike.
#'@param CXH Call Premium received for the sold Calls at higher Strike.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Collar Strategy.
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
#'collarStrategy(30,19,20,35,6.50,5)
#'collarStrategy(800,770,750,790,80,70,hl=0.92,hu=1.02)
#'@export
collarStrategy <- function (ST,S0,XL,XH,PXL,CXH,hl=0,hu=1.5,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Collar Strategy", sub="bullishTrader / MaheshP Kumar"){
  V0Dr= PXL- CXH
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (myData$spot -S0) + pmax(0,(XL-myData$spot)) - (pmax(0,(myData$spot-XH))) -V0Dr
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  # print(-(S0+PXL-XL-CXH))
  # print(XH-XL-(S0+PXL-XL-CXH))
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  points(x=(S0+PXL- CXH), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text((S0+PXL- CXH),0.25, labels=(S0+PXL- CXH), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  # abline(h = -(S0+PXL-XL-CXH),col = "blue",lty=5,lwd=1.25)
  # abline(h = XH-XL-(S0+PXL-XL-CXH),col = "blue",lty=5,lwd=1.25)
  abline(v = XL,col = "gray",lty=5,lwd=1.25)
  abline(v = XH,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
