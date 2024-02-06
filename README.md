# breast_cancer_ID_model
An ML model a team &amp; I made in R that predicts whether or not a person has breast cancer. This is the .rmd file I made in RStudio along with some notable functions from that .rmd.

<h1 style="text-align:center">Summary</h1>
<p>
The data set is data on breast cancer patients and various aspects of their breast cell nuclei.It was originally collected by the University of California Irvine(UCI). It was made available/donated/collected on 11/01/1995. <br>
The data set contains 32 columns but these columns hold various summary stats(Specifically the mean, standard error, & worst) of the 10 measurements:
<ul>
<li>Radius</li>
<li>Texture</li>
<li>Perimeter</li>
<li>Area</li>
<li>Smoothness</li>
<li>Compactness </li>
<li>Concavity </li>
<li>Concave Points </li>
<li>Symmetry</li>
<li>Fractal Dimension </li>
</ul>

There are 569 entries in the data set.There are no null values.<br>
ID is an int value but can be ignored. The diagnosis tab is our target value and it contains two char values: M & B(M is short for malignant, B short for benign). All other tabs are num values which are the mean, standard error, and worst values of the terms of the list above. We will separate the mean,standard error, & worst values into their own data frames(We will focus on the mean first but want to keep the std or worst/bottoms of the distribution in case we need it later).<br>
<ul>
<li>Null: There is NO relationship between the size characteristics of a tumor and it’s concavity points</li>
<li>Alt: There IS relationship between the size characteristics of a tumor and it’s concavity points</li>
</ul>
</p>
<br>
<h1 style="text-align:center">Data Sample</h1>
<p>
Below I quickly take a look at the dataset. I show the first five rows of the dataset, as well as the types of each column, and what the two chars in the diagnosis column are.
</p>

```{r}
bc = read.csv("C:/Users/Benny/Downloads/BreastCancer.csv")
library(addAlpha)
library(corrplot, quietly = TRUE)
library(car, quietly = TRUE)
library(ltm)
library(RColorBrewer)

colors <- c("#6D7696", "#CC5543", "#FFB4BC", "#ba7da0", "#EDB579", "#DBE6AF", "#6D76B9", "#594810")
colorsT <- add.alpha(colors)
```

```{r}
corrCol <- brewer.pal(8, "RdPu")
```


```{r}
head(bc,5)
```

```{r}
str(bc)
```
```{r}
unique(bc$diagnosis)
```

<h1 style="text-align:center">Data Wrangling</h1>
<p>
In the following code block we separate the data into more manageable objects. 
<ol>
<li>We change the diagnosis column to binary for easier use later. Malignant = 1 & Benign = 0.</li>
<li>We separate the dataset into three respective dataframes based on the stats collected for.</li>
</ol>
</p>

```{r}
#This code is to make 0s & 1s of the diagnosis col to allow for use in models
bc$diagnosis <- ifelse(bc$diagnosis == "M", 1, 0)

#This is the code that separates the data frame into their own based on the stats collected
mean_bc = subset(bc, select = c(diagnosis,radius_mean:fractal_dimension_mean))
std_bc = subset(bc, select = c(diagnosis,radius_se:fractal_dimension_se))
worst_bc = subset(bc, select = c(diagnosis,radius_worst:fractal_dimension_worst))

str(mean_bc)
```

<h1 style="text-align:center">Making A Model</h1>
<p>
For the purposes of our experiment we want to make a logistic regression, however we don't want to use all features given to avoid overfitting. In order to do so we figure out which variables correlate the best with point bi-serial correlation. From these variables we can make models of all the possible permutations of said models.
</p>

```{r}
#This formula makes
pbc <- function(target, predictor){
  #Sample size
  n1 <- length(target)
  n2 <- length(predictor)
  
  N <- n1 + n2
  
  #Std
  s1 <- sd(target)
  s2 <- sd(predictor)
  
  
  thetaPool <- sqrt((((n1-1)*s1**2)+((n2-1)*s2**2))/(n1+n2-2))
  #Degrees of freedom within a group
  dfw <- N-2
  
  #Hedge's G
  g <- (mean(target) - mean(predictor))/thetaPool
  
  #Point biserial Corr
  r <- g/sqrt(g**2+dfw*((1/n1)+(1/n2)))
}
```

```{r}
#Puts the results of our pcb function into a matrix
corr <- matrix( ,nrow = length(colnames(mean_bc)), ncol = length(colnames(mean_bc)))

for(i in 1:length(colnames(mean_bc))) {
  for(j in 1:length(colnames(mean_bc))){
    corr[i,j] <- pbc(mean_bc[,i],mean_bc[,j])
  }
}

colnames(corr) <- c("Diagnosis","Radius","Texture","Perimeter","Area","Smoothness","Compactness","Concavity","Concave.points",
                    "Symmetry","Fractal Dim")
rownames(corr) <- c("Diagnosis","Radius","Texture","Perimeter","Area","Smoothness","Compactness","Concavity","Concave.points",
                    "Symmetry","Fractal Dim")
corr
```
```{r, fig.align='center', fig.height = 10, fig.width=14}
#This makes a corrplot for the results of our corr matrix
corrplot(corr,
         method = "color",
         type = "upper",
         diag = FALSE,
         col = corrCol,
         outline = "black",
         addCoef.col = "black",
         number.cex = 1.5,
         number.digits = 1,
         tl.col = "black",
         cl.cex = 1,)
```
<p>
Our results are a little inconclusive so we'll start by making a model that includes all variables and narrow it down using our corrplot to make as many models as possible (As a preliminary check for multicolinearity we will drop any vars that have higher than .5 correlation with another var).  
</p>

```{r}
#Model that includes all vars
allModel <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")

#Model for Radius & Texture
trModel <- glm(diagnosis ~ radius_mean + texture_mean, data = mean_bc, family = "binomial")

#Models based off of Smoothness
s2cModel <- glm(diagnosis ~ smoothness_mean + compactness_mean + concavity_mean , data = mean_bc, family = "binomial")
scModel1 <- glm(diagnosis ~ smoothness_mean + concavity_mean, data = mean_bc, family = "binomial")
scModel2 <- glm(diagnosis ~ smoothness_mean + compactness_mean, data = mean_bc, family = "binomial")

#Models based off of Compactness
ccfModel <- glm(diagnosis ~ compactness_mean + concavity_mean + concave.points_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")
ccfModel1 <- glm(diagnosis ~ compactness_mean + concavity_mean + concave.points_mean, data = mean_bc, family = "binomial")
ccfModel2 <- glm(diagnosis ~ compactness_mean + concavity_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")
ccfModel3 <- glm(diagnosis ~ compactness_mean + concave.points_mean + fractal_dimension_mean, data = mean_bc, family = "binomial") #-------------
ccfModel4 <- glm(diagnosis ~ compactness_mean + concavity_mean, data = mean_bc, family = "binomial")
ccfModel5 <- glm(diagnosis ~ compactness_mean + concave.points_mean, data = mean_bc, family = "binomial")
ccfModel6 <- glm(diagnosis ~ compactness_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")

#compactness_mean         concavity_mean fractal_dimension_mean
#Models based off of Concavity
cfModel <- glm(diagnosis ~ concavity_mean + concave.points_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")
cfModel1 <- glm(diagnosis ~ concavity_mean + concave.points_mean, data = mean_bc, family = "binomial")
cfModel2 <- glm(diagnosis ~ concavity_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")

#Model based off of Concave Points
cpfModel <- glm(diagnosis ~ concave.points_mean + fractal_dimension_mean, data = mean_bc, family = "binomial")

models <- list(allModel, trModel, s2cModel, scModel1, scModel2, ccfModel, ccfModel1, ccfModel2, ccfModel3, ccfModel4, ccfModel5, ccfModel6,
               cfModel, cfModel1, cfModel2, cpfModel)
```
<p>
Now we separate our data into training and testing sets. We set a seed in order to insure that whoever runs this program gets similar results. 
</p>

```{r}
set.seed(42)
train.IDX <- sample(c(TRUE,FALSE), size = dim(mean_bc)[1], replace = TRUE, prob = c(.75,.25))
dataTrain <- mean_bc[train.IDX, ]
dataTest <- mean_bc[!train.IDX, ]
```

<h3>Assumptions of Logistic Regression</h3>
<p>
Next we check to make sure the various assumptions of logistic regression are met. As a reminder they are:
<ul>
  <li>The response variable is binary</li>
  <li>The observations are independent</li>
  <li>There is no multicollinearity among your predictor variables (We did this already)</li>
  <li>There are no extreme values or extreme outliers</li>
  <li>There is a linear association between quantitative variables and the logit</li>
  <li>Large sample</li>
</ul>
From the code block below we can see that our variable is binary, the inputs are independent, and the sample is large enough.We then conduct the multicolinearity test; We find that the first model we made affirms the findings of our corrplot and we can now eliminate it from consideration but all the other models pass so we can continue with them. We then use the CookD function to test for extreme values, which none of the models contain.
</p>

```{r}
cat("Is it binary?\n",unique(mean_bc$diagnosis),
    "Yes\nAre the inputs independent?\n",length(unique(bc$id)) == length(bc$id),
    "Yes\nIs the sample is large enough?\n",dim(dataTrain)[1],"Yes")
```

```{r}
unique(mean_bc$diagnosis)
length(unique(bc$id)) == length(bc$id)
dim(dataTrain)[1]
```
```{r}
j=0
for (i in models) {
  j=j+1
  cat("Model",j,":\n")
  print(vif(i))
  cat("\n")
}
#Removing models that violate multicolinearity
models <- models[-c(1,6,9)]
```


```{r}
#Are there any extreme values? No
nParam <- 10
N <- dim(mean_bc)[1]
cutoff <- qf(0.5, df1 = nParam, df2 = N-nParam)

j=0
for (i in models) {
  cookD <- cooks.distance(i)
  print(sum(cookD > cutoff))
}
```

```{r}
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
  
  
  points(x = predictor, y = logit, col=colorsT[colorNum], pch = 19, cex = 3)
}
```

```{r, fig.align='center', fig.height = 10, fig.width=14}
############################################################################################
#Update mfrow with the correct dimensions
#I might need to make different functions and pars for the total amount of variables in each model

par(mfrow = c(1,2), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))

#Model 1
makeLogitPlots(trModel, mean_bc, mean_bc$radius_mean, "Radius", 1)
makeLogitPlots(trModel, mean_bc, mean_bc$texture_mean, "Texture", 2)

#Model 2
par(mfrow = c(1,3), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))
makeLogitPlots(s2cModel, mean_bc, mean_bc$smoothness_mean, "Smoothness", 3)
makeLogitPlots(s2cModel, mean_bc, mean_bc$compactness_mean, "Compactness", 4)
makeLogitPlots(s2cModel, mean_bc, mean_bc$concavity_mean, "Concavity", 5)

#Model 3&4 --- Drop Model 3&4
par(mfrow = c(2,2), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))
makeLogitPlots(scModel1, mean_bc, mean_bc$smoothness_mean, "Smoothness", 6)
makeLogitPlots(scModel1, mean_bc, mean_bc$compactness_mean, "Compactness", 7)
#---
makeLogitPlots(scModel2, mean_bc, mean_bc$smoothness_mean, "Smoothness", 8)
makeLogitPlots(scModel2, mean_bc, mean_bc$concavity_mean, "Concavity", 1)

#Model 5&6 --- Drop model 6
par(mfrow = c(2,3), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))
makeLogitPlots(ccfModel1, mean_bc, mean_bc$compactness_mean, "Compactness", 2)
makeLogitPlots(ccfModel1, mean_bc, mean_bc$concavity_mean, "Concavity", 3)
makeLogitPlots(ccfModel1, mean_bc, mean_bc$concave.points_mean, "Concave.points", 4)
#---
makeLogitPlots(ccfModel2, mean_bc, mean_bc$smoothness_mean, "Smoothness", 5)
makeLogitPlots(ccfModel2, mean_bc, mean_bc$compactness_mean, "Compactness", 6)
makeLogitPlots(ccfModel2, mean_bc, mean_bc$fractal_dimension_mean, "Fractal Dim", 7)

#Model 7&8
par(mfrow = c(2,2), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))
makeLogitPlots(ccfModel4, mean_bc, mean_bc$compactness_mean, "Compactness", 8)
makeLogitPlots(ccfModel4, mean_bc, mean_bc$concavity_mean, "Concavity", 1)
#---
makeLogitPlots(ccfModel5, mean_bc, mean_bc$compactness_mean, "Smoothness", 2)
makeLogitPlots(ccfModel5, mean_bc, mean_bc$concave.points_mean, "Concave.points", 3)

#Model 9&10 --- Drop 10
par(mfrow = c(2,3), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))
makeLogitPlots(ccfModel1, mean_bc, mean_bc$compactness_mean, "Compactness", 2)
makeLogitPlots(ccfModel1, mean_bc, mean_bc$concavity_mean, "Concavity", 3)
makeLogitPlots(ccfModel1, mean_bc, mean_bc$concave.points_mean, "Concave.points", 4)
#---
makeLogitPlots(ccfModel2, mean_bc, mean_bc$smoothness_mean, "Smoothness", 5)
makeLogitPlots(ccfModel2, mean_bc, mean_bc$compactness_mean, "Compactness", 6)
makeLogitPlots(ccfModel2, mean_bc, mean_bc$fractal_dimension_mean, "Fractal Dim", 7)

#Model 11,12&13 --- Drop 12 & 13
par(mfrow = c(3,2), mai = c(1,1,0,0), omi = c(.1,.1,.1,.1))
makeLogitPlots(ccfModel1, mean_bc, mean_bc$concavity_mean, "Concavity", 8)
makeLogitPlots(ccfModel1, mean_bc, mean_bc$concave.points_mean, "Concave.points", 1)
#---
makeLogitPlots(ccfModel2, mean_bc, mean_bc$concavity_mean, "Concavity", 2)
makeLogitPlots(ccfModel2, mean_bc, mean_bc$fractal_dimension_mean, "Fractal Dim", 3)
#---
makeLogitPlots(ccfModel2, mean_bc, mean_bc$concave.points_mean, "Concave.points", 4)
makeLogitPlots(ccfModel2, mean_bc, mean_bc$fractal_dimension_mean, "Fractal Dim", 5)

############################################################################################

#Drops models we believe violate logit association
models <- models[-c(3,4,6,10,12,13)]
```

```{r}
lapply(models, vif)
```


<h3>Results of Models</h3>

```{r}
j=0
modelPerf <- matrix( ,nrow = length(models), ncol = 4)
predicts <- matrix( ,nrow = dim(dataTest)[1], ncol = length(models))
predictDF <- data.frame(predicts)

for(i in models){
  j=j+1
  yPred <- predict(i, newdata = dataTest, type = "response")
  dataTest$Predict <- yPred > 0.5
  predictDF[,j] <- dataTest$Predict
  
  accuracy <- sum(dataTest$Predict == dataTest$diagnosis)/length(dataTest$diagnosis)
  falsePos <- sum(dataTest$Predict == 1 & dataTest$diagnosis == 0)/sum(dataTest$diagnosis == 1)
  falseNeg <- sum(dataTest$Predict == 0 & dataTest$diagnosis == 1)/sum(dataTest$diagnosis == 0)
  posPredict <-  sum(dataTest$Predict == TRUE & dataTest$diagnosis == 1) / sum(dataTest$Predict == TRUE)
  temp <- c(accuracy, falsePos, falseNeg, posPredict)
  
  for (k in 1:length(temp)) {
    modelPerf[j,k] <- temp[k]
  }
  cat("\n\nModel",j,"\nAccuracy:\n",(100*round(accuracy,3)),"%",
      "\nFalse Positive Rate:\n",(100*round(falsePos,3)),"%",
      "\nFalse Negative Rate:\n",(100*round(falseNeg,3)),"%",
      "\nPositive Predict Rate:\n", (100*round(posPredict,3)),"%")
  temp <- temp[-c(1:4)]
} 
```

```{r, fig.align='center', fig.height = 8, fig.width=16}
modelNames <- c("Model 1","Model 2","Model 3", "Model 4","Model 5","Model 6","Model 7")
xpos<- barplot(t(modelPerf) ,
        space = c(0.1,1),
        beside = TRUE, 
        axes = FALSE, 
        ylim = c(0,1),
        col = colorsT[0:4],
        border = colors[0:4])

axis(2, at = pretty(c(0,1)), tck = -.015, lwd = 2, labels = FALSE)
mtext(pretty(c(0,1)), 2, at = pretty(c(0,1)), line = .6, las = 1)

mtext("Model Performance", 1.5, font = 2, line = 2.5)
mtext(modelNames, 1, at = xpos[3,],line = 1.5)

legend(33,1.02, legend = c("Accuracy","False Positive","False Negative","Positive Prediction"), fill = colorsT[0:4], border = colors[0:4], bty = "n")
```

```{r}
predictDF$Diagnosis <- dataTest$diagnosis

predictM <- matrix(,nrow = 8, ncol = 2)

predictM[1,1] <- sum(predictDF$Diagnosis == 1)/length(predictDF$Diagnosis)
predictM[1,2] <- sum(predictDF$Diagnosis == 0)/length(predictDF$Diagnosis)

predictM[2,1] <- sum(predictDF$X1 == TRUE)/length(predictDF$X1)
predictM[2,2] <- sum(predictDF$X1 == FALSE)/length(predictDF$X1)

predictM[3,1] <- sum(predictDF$X2 == TRUE)/length(predictDF$X3)
predictM[3,2] <- sum(predictDF$X2 == FALSE)/length(predictDF$X3)

predictM[4,1] <- sum(predictDF$X3 == TRUE)/length(predictDF$X3)
predictM[4,2] <- sum(predictDF$X3 == FALSE)/length(predictDF$X3)

predictM[5,1] <- sum(predictDF$X4 == TRUE)/length(predictDF$X4)
predictM[5,2] <- sum(predictDF$X4 == FALSE)/length(predictDF$X4)

predictM[6,1] <- sum(predictDF$X5 == TRUE)/length(predictDF$X5)
predictM[6,2] <- sum(predictDF$X5 == FALSE)/length(predictDF$X5)

predictM[7,1] <- sum(predictDF$X6 == TRUE)/length(predictDF$X6)
predictM[7,2] <- sum(predictDF$X6 == FALSE)/length(predictDF$X6)

predictM[8,1] <- sum(predictDF$X7 == TRUE)/length(predictDF$X6)
predictM[8,2] <- sum(predictDF$X7 == FALSE)/length(predictDF$X6)
```


```{r, fig.align='center', fig.height = 6, fig.width=16}
xpos<- barplot( t(predictM),
        space = c(0.2,1),
        beside = TRUE, 
        axes = FALSE, 
        ylim = c(0,1),
        col = colorsT[c(4,3)],
        border = colors[c(4,3)])

axis(2, at = pretty(c(0,1)), tck = -.015, lwd = 2, labels = FALSE)
mtext(abs(pretty(c(0,1))), 2, at = pretty(c(0,1)), line = .4, las = 1)

mtext("Proportion of Actual Diagnosis vs Prediction", 1.5, font = 2, line = 2)
mtext(c("Actual",modelNames), 1, at = xpos[1,], line = .3)

legend("topright", legend = c("Malignant","Benign"), fill = c(colorsT[4], colorsT[3]), border = colors[c(4, 3)], bty = "n")
```

```{r}
for (i in models) {
  print(summary(i))
}
```
