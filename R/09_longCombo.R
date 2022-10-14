#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven (BE) point at expiration for Long Combo  and draws its graph in the Plots tab.
#'@description
#'This strategy results from buying an OTM call option with a strike price X2H and selling an OTM put option with a strike price X1L, where X2H is greater than X1L (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed-form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and Breakeven (BE) point for Long Combo and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2H Higher Strike Price or eXercise price.
#'@param PX1L Put Premium received for the sold Put at Lower Strike.
#'@param CX2H Call Premium paid for the bought Call at higher Strike .
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Long Combo Strangle.
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
#'longCombo(19,15,25,1.50,2.00)
#'longCombo(19,15,25,1.50,1.50)
#'longCombo(19,15,25,1.80,1.50)
#'longCombo(60,47,57,5,4,hl=0.4,hu=1.4)
#'longCombo(1000,980,1010,80,80,hl=0.965,hu=1.025)
#'longCombo(1000,980,1010,80,78,hl=0.965,hu=1.025)
#'longCombo(1000,980,1010,76,78,hl=0.965,hu=1.025)
#'@export
longCombo <- function (ST,X1L,X2H,PX1L,CX2H,hl=0,hu=1.9,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Long Combo ", sub="bullishTrader / MaheshP Kumar"){
  V0Dr=CX2H -PX1L
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <-  (pmax(0,(myData$spot-X2H))-pmax(0,(X1L-myData$spot))-V0Dr)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  if(V0Dr>=0)
    {
    points(x=X2H+V0Dr, y=0,cex = 2, pch = 23, col ="red",bg="gold")
    text(X2H+V0Dr,1, labels=X2H+V0Dr, adj = 0.1,col="goldenrod3")
    }
    if(V0Dr<=0)
      {
      points(x=X1L+V0Dr, y=0,cex = 2, pch = 23, col ="red",bg="gold")
      text(X1L+V0Dr,1, labels=(X1L+V0Dr), adj = 0.1,col="goldenrod3")
      }
  abline(h = 0,col = "gray")
  abline(v = X1L,col = "gray",lty=5,lwd=1.25)
  abline(v = X2H,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}

