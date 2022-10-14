#'Calculates Profit and Loss (PnL) per share (or unit of the underlying) and Breakeven (BE) point at expiration for Bull Call Spread and draws its graph in the Plots tab.
#'@description
#'The trading strategy that is implemented with an expectation that the price of the stock or the underlying will rise in future is called a bullish strategy. As explained by Chance (2019), an option is a derivative contract in which one party, the buyer, pays a sum of money to the other party, the seller or writer, and receives the right to either buy (known as call option) or right to sell (known as put option) an underlying asset at a fixed price either on a specific expiration date (European option) or at any time prior to the expiration date (American Option). So the right to buy is one type of option, referred to as a call or call option, whereas the right to sell is another type of option, referred to as a put or put option.\cr
#'Further, it is explained by Chance (2019) that a derivative is a financial instrument that derives its performance from the performance of an underlying asset. Derivatives can be used as insurance that allows for the transfer of risk from one party to another. As everyone knows, insurance is a financial contract that provides protection against loss. The party bearing the risk purchases an insurance policy, which transfers the risk to the other party, the insurer, for a specified period of time. The risk itself does not change, but the party bearing it does. Derivatives allow for this same type of transfer of risk.\cr
#'Derivatives are associated with an underlying asset. As such, the underlying asset is often simply referred to as the underlying, whose value is the source of risk. Derivatives are created in the form of legal contracts. They involve two parties: the buyer and the seller (sometimes known as the writer); each of whom agrees to do something for the other, either now or later. The buyer, who purchases the derivative, is referred to as the long or the holder because he owns (or holds) the derivative and holds a long position. The seller is referred to as the short because he holds a short position (Chance, 2019).\cr
#'@details
#'This is a vertical spread consisting of a long position in a close to ATM (at the money) call option with a strike price XL, and a short position in an OTM (out of the money) call option with a higher strike price XH. This is a net debit (V0Dr) trade as option premium or option price (CH) received on call shorted at higher strike price (XH) is less than premium or price (CL) paid for the call bought at lower strike price (XL). So, V0Dr is equal to CL minus CH. The outlook of the trader (or investor) is bullish and the strategy profits if the stock price rises (Kakushadze & Serur, 2018).\cr
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration and Breakeven(BE) point for Bull Call Spread and draws its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param XH Higher Strike Price or eXercise price.
#'@param XL Lower Strike Price or eXercise price.
#'@param CH Call Premium received for the sold Call at Higher Strike.
#'@param CL Call Premium paid for the bought Call at Lower Strike.
#'@param hl lower bound value for setting lower limit of  X axis displaying spot price.
#'@param hu upper bound value for setting upper limit of  X axis displaying spot price.
#'@param xlab X axis label.
#'@param ylab Y axis label.
#'@param main Title of the Graph.
#'@param sub Subtitle of the Graph.
#'@return returns a profit and loss graph of Bull Call Spread.
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
#'Chance,D.M.(2019). Basics of Derivative Pricing and Valuation. In CFA Program Curriculum 2020 Level I Volumes 1-6. (Vol. 5, pp. 385-453). Wiley Professional Development (P&T). ISBN 9781119593577, https://bookshelf.vitalsource.com/books/9781119593577\cr
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865
#'@examples
#'aBullCallSpread(17,17,15,1.64,0.51)
#'aBullCallSpread(50,50,47,5,3.2,hl=0.7,hu=1.2)
#'aBullCallSpread(1000,1000,998,14.5,13.25,hl=0.98,hu=1.02)
#'@export
aBullCallSpread <- function (ST,XH,XL,CL,CH,hl=0,hu=1.5,xlab="Spot Price ($) on Expiration",ylab="Profit / Loss [ PnL ] at Expiration ($)",main="Bull Call Spread ", sub=" bullishTrader / MaheshP Kumar"){
  V0Dr= CL-CH
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax(0,(myData$spot-XL))-pmax(0,(myData$spot-XH))-V0Dr)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  par(mfrow = c(1,1), bg="azure1",las=1,mai=c(1.12, 0.92, 0.82, 0.82))
  plot(myData$spot, myData$pl, pch=21, bg="cyan1",col="cyan1", xlab = xlab, ylab = ylab,col.lab="springgreen4", cex.lab= 1.1, main = main,col.main="violetred", col.sub="lightsteelblue2", sub=sub,cex.sub=0.8,xaxt="n")
  text (myData$spot, myData$pl, labels = as.character(myData$pl), adj=-0.25,
        pos = NULL, offset = 0.3, vfont = NULL,
        cex = 0.5, col = "red", font = NULL )
  points(x=(XL+CL-CH), y=0,cex = 2, pch = 23, col ="red",bg="gold")
  text((XL+CL-CH),0.25, labels=(XL+CL-CH), adj = 0.1,col="goldenrod3")
  abline(h = 0,col = "gray")
  abline(v = XH,col = "gray",lty=5,lwd=1.25)
  abline(v = XL,col = "gray",lty=5,lwd=1.25)
  legend("topleft", legend = c("PnL Point","BE Point"),text.col ="snow",  bg ="darkorchid4", pch=c(16,18), col=c("cyan1","gold"),cex = 1)
  lines(myData$spot,myData$pl,col = "blue")
  axis(4, at=myData$pl,labels=myData$pl, col.axis="red", las=2,cex.axis=0.8,col.ticks ="lightsteelblue2")
  axis(1, at=myData$spot,labels=myData$spot,col.axis="blue",las=1,cex.axis=0.7,col.ticks = "lavenderblush2")
}
