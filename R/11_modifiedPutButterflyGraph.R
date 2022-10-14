#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) at expiration and Breakeven (BE) point for Modified Put Butterfly and draws its graph in the Plots tab.
#'@description
#'This is a variation of the long put butterfly strategy and the strikes are no longer equidistant; instead we have X3H minus X2M is less than X2M minus X1L. This results in a sideways strategy with a bullish bias (Cohen,2015).
#'@details
#'According to conceptual details and formula given by Cohen (2015), this method is developed, and the given examples are created, to compute per share Profit and Loss and Breakeven (BE) point at expiration for Modified Put Butterfly and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price.
#'@param X2M Medium Strike Price or eXercise price.
#'@param X3H Higher Strike Price or eXercise price.
#'@param P1XL Put Premium Paid for the bought Put at Lower Strike.
#'@param PX2M Put Premium received from the sold Put at Medium Strike.
#'@param PX3H Put Premium paid for the bought Put at higher Strike .
#'@param hl lower bound value for setting lower limit of X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return Returns a graph Modified Put Butterfly.
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
#'modifiedPutButterfly(50,45,55,60,0.98,6.12,10.28)
#'modifiedPutButterfly(1000,980,1020,1030,10,60,100,hl=0.97,hu=1.05)
#'@export
modifiedPutButterfly<-function (ST,X1L,X2M,X3H,P1XL,PX2M,PX3H,hl=0.8,hu=1.3,xlab="Spot Price ($) at Expiration",ylab=" Profit / Loss [ PnL ] at Expiration ($)",main="Modified Put Butterfly ", sub="bullishTrader / MaheshP Kumar"){
  V0Cr= (2*PX2M)-(P1XL+PX3H)
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax(0,(X3H-myData$spot))+pmax(0,(X1L-myData$spot))-(2*(pmax(0,(X2M-myData$spot)))))+ V0Cr
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="honeydew",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot,myData$pl, type="b", pch=21,bg="cyan1",col="cyan1",cex=1.1, xlab = xlab, ylab = ylab, col.lab="springgreen4", cex.lab= 1.1,main = main,col.main="violetred",sub=sub, col.sub="lightsteelblue2",cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj = 1.1,
        pos = NULL, offset = 0.5, vfont = NULL,
        cex = 0.7, col = "red", font = NULL )
  points(x=(X1L+(X2M-X1L)-(X3H-X2M)-V0Cr), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text((X1L+(X2M-X1L)-(X3H-X2M)-V0Cr),-0.7, labels=(X1L+(X2M-X1L)-(X3H-X2M)-V0Cr), adj = 0.1,col="goldenrod3")
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

