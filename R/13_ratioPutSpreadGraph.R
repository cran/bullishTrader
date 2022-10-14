#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven point at expiration for Ratio Put Spread and draws its graph in the Plots tab.
#'@description
#'This strategy consists of a short position in NS close to ATM put options with a strike price X1L, and a long position in NL ITM put options with a strike price X2H, where NL is less than NS. Typically, NL is equal to 1 and NS is equal to 2, or NL is 2 and NS is 3. This is an income strategy if it is structured as a net credit trade. The trader’s outlook is neutral to bullish (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and also the Breakeven (BE) point for Ratio Put Spread and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X2H Higher Strike Price or eXercise price.
#'@param X1L Lower Strike Price or eXercise price.
#'@param PX1L  Premium paid for the bought s at Lower Strike.
#'@param PX2H  Premium received for the sold s at higher Strike.
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Ratio Put Spread.
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
#'ratioPutSpread(25,30,25,1.33,0.42)
#'@export
ratioPutSpread <- function (ST,X2H,X1L,PX2H,PX1L,hl=0,hu=1.7,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Ratio Put Spread ", sub="bullishTrader / MaheshP Kumar"){
  V0Dr= PX2H-2*PX1L
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <-  ((1*(pmax(0,(X2H-myData$spot)))-(2*(pmax(0,(X1L-myData$spot))))- V0Dr))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  if (V0Dr>=0){
    points(x=(X2H-V0Dr/1), y=0,cex = 2, pch = 23, col ="red",bg="gold")
    text((X2H-V0Dr/1),0.50, labels=(X2H-V0Dr/1), adj = 0.1,col="goldenrod3")
    }
     points(x=(2*X1L-1*X2H+V0Dr)/(2-1), y=0,cex = 2, pch = 23, col ="red",bg="gold")
     text((2*X1L-1*X2H+V0Dr)/(2-1),0.50, labels=(2*X1L-1*X2H+V0Dr)/(2-1), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = X2H,col = "gray",lty=5,lwd=1.25)
  abline(v = X1L,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}

