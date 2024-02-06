#Func I made to make logit plots to test for logistic regression

#Explanation of the more abstract function vars:
#predictor is the var to put in the x-axis
#label1 is the string to put in for the x-axis of the scatterplot
#colorNum is the index for the colorsT vector; Should be a num between 1 & 8

makeLogitPlots <- function(model, theData, predictor, label, colorNum){
  
  #Model
  probs <-  predict(model, data = theData, type = "response")
  logit<- log(probs/(1-probs))
  
  xLimit <- range(pretty(range(predictor)))
  yLimit <- range(pretty(range(logit)))
  
  #Graph 1
  plot(x=NULL, y=NULL, xlim = xLimit, ylim = yLimit, axes = FALSE, ann = FALSE)
  
  axis(1, at = pretty(xLimit), tck = -.015, lwd = 1, labels = FALSE)
  axis(2, at = pretty(yLimit), tck = -.015, lwd = 1, labels = FALSE)
  
  mtext(pretty(xLimit),1, line = .3, at = pretty(xLimit), las = 1, cex= .75)
  mtext(pretty(yLimit),2, line = .3, at = pretty(yLimit), las =1, cex= .75)
  
  mtext(label,1, font=2, line = 1.5)
  mtext("Logit",2, font=2 ,line=2)
  
  
  point
