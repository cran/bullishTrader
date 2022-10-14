#'Calculates  Profit and Loss at closeout of a bought Stock Index Future (like Micro E mini) and draws its graph in the Plots tab.
#'@description
#'On initiation, this is a net debit Strategy and results in net cash outflow in the form of paying the amount of bought Stock Index Future. If the bullish outlook of the trader emerges as expected and the Stock Index Future (like Micro E mini) rises then the trader makes the profit as shown in the graph (TD Ameritrade, 2019).
#'@details
#'According to the information provided by TD Ameritrade (2019) and  Hull (2022), this method is developed, and the given examples are created, to compute Profit and Loss at closing of the position in Stock Index Future (like Micro E mini) and draw its graph in the Plots tab.
#'@param SIT Stock Index at time T.
#'@param SI0 Stock Index Initial Value.
#'@param R annualized financing rate
#'@param d dividend yield of the index.
#'@param n represents days in Future like 90day Stock Index Future.
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
#'TD Ameritrade. (2019, July 26). Micro E-Mini Futures Contracts. YouTube.https://youtu.be/SShGjJepCdA\cr
#'Hull, J. C. (2022). Options, Futures, and Other Derivatives (11th ed.). Pearson Education (US). https://bookshelf.vitalsource.com/books/9780136940043.
#'@examples
#'zBuyingStockIndexFutureAtFairValue(3700,3709,0.0275,0.02,90)
#'zBuyingStockIndexFutureAtFairValue(2900,2910,0.025,0.03,90,hl=0.9995,hu=1.005)
#'@export
zBuyingStockIndexFutureAtFairValue <- function (SIT,SI0,R,d,n,hl=0.9995,hu=1.006,xlab="Stock Index at position closing",ylab="Profit / Loss [PnL] at position closing ($)",main="Long on Stock Index like Micro E mini S & P 500 ", sub="bullishTrader / MaheshP Kumar"){
  FV0 <- SI0*(1+(R-d)*(n/360))
  myData <- data.frame (spot = c((SIT*hl):(SIT*hu)))
  myData$pl <- (myData$spot-FV0)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  points(x=(SIT=FV0), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text(x=(SIT=FV0),1, labels=round((SIT=FV0), digits=2), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = SI0,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
