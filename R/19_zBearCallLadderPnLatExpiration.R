#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) at expiration and Breakeven point for Bear Call Ladder and draws its graph in the Plots tab.
#'@description
#'This is a vertical spread consisting of a short position in (usually) a close to ATM (at the money) call option with a strike price X1L, a long position in an OTM (out of the money) call option with a strike price X2M, and a long position in another OTM call option with a higher strike price X3H. A bear call ladder typically arises when a bear call spread (a bearish strategy) goes wrong (the stock trades higher), so the trader buys another OTM call option (with the strike price X1L) to reverse from the initial bearish outlook to emerging bullish trends. On initiation, this is a net credit Strategy and results in net cash inflow as premium received on shorting a call (at lower strike) is more than premium paid on buying two calls (buying one call at somewhat middle priced strike X2M and then buying one more call at higher strike X3H) (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss and Breakeven (BE) point at expiration for Bear Call Ladder and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2M Medium Strike Price or eXercise price.
#'@param X3H Higher Strike Price or eXercise price.
#'@param CX1L Call Premium received for the sold Call at Lower Strike.
#'@param CX2M Call Premium paid for the bought Call at Medium Strike.
#'@param CX3H Call Premium paid for the bought Call at higher Strike .
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return Returns a graph Bear Call Ladder.
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
#'zBearCallLadder(19,19,20,24,4.20,2.40,0.80)
#'zBearCallLadder(50,50,52,56,8.1,3.70,2.50,hl=0.8,hu=1.2)
#'zBearCallLadder(1000,1000,1005,1010,20,11,4,hl=0.995,hu=1.025)
#'@export
zBearCallLadder<-function (ST,X1L,X2M,X3H,CX1L,CX2M,CX3H,hl=0,hu=1.5,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [ PnL ] at Expiration ($)",main="Bear Call Ladder ", sub="bullishTrader / MaheshP Kumar"){
  V0Cr= CX1L-CX2M-CX3H
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <-  (pmax(0,(myData$spot-X3H))+pmax(0,(myData$spot-X2M))-pmax(0,(myData$spot-X1L))+V0Cr)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="honeydew",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot,myData$pl, type="b", pch=21,bg="cyan1",col="cyan1",cex=1.1, xlab = xlab, ylab = ylab, col.lab="springgreen4", cex.lab= 1.1,main = main,col.main="violetred",sub=sub, col.sub="lightsteelblue2",cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj = 1.1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  points(x=(X1L+V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  points(x=(X3H+X2M-X1L-V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text((X1L+V0Cr),0.5, labels=(X1L+V0Cr), adj = 0.1,col="goldenrod3")
  text((X3H+X2M-X1L-V0Cr),0.5, labels=(X3H+X2M-X1L-V0Cr), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = X1L,col = "gray", lty=5,lwd=1.25)
  abline(v = X2M,col = "gray", lty=5,lwd=1.25)
  abline(v = X3H,col = "gray", lty=5,lwd=1.25)
  abline(v = ,col = "gray",lty=5,lwd=2)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
