# Erik Fretland
# Thesis test work

library(tidyverse)
library(ggplot2)
library(mlr)
library(glmnet)
library(rpart)
library(plyr)
library(dplyr)
library(gmodels)
library(stargazer)



setwd("C:/Users/User/Documents/Thesis Work")

jaxd <- read.csv("jaxdtreeset.csv")

jaxd <- data.frame(jaxd)




# Reorganizing Flaws in Data



attach(jaxd)

jaxd$DefShell.Left[jaxd$DefShell.Left == "C2U"]      <- "C2M"
jaxd$DefShell.Left[jaxd$DefShell.Left == "33ZB"]     <- "ZB"
jaxd$DefShell.Right[jaxd$DefShell.Right == "33ZB"]   <- "ZB"
jaxd$DefShell.Right[jaxd$DefShell.Right == "C0H"]    <- "C0MH"


# Create Situational Variable

jaxd$ThirdAnd <- NA

jaxd$ThirdAnd[X3.VShort == 1]   <- "1VShort"
jaxd$ThirdAnd[X3.Short == 1]    <- "2Short"
jaxd$ThirdAnd[X3.Med == 1]      <- "3Med"
jaxd$ThirdAnd[X3.Long == 1]     <- "4Long"
jaxd$ThirdAnd[X3.VLong == 1]    <- "5VLong"


# Create more general coverage type

jaxd$DefShell <- NA

jaxd$DefShell[Covg.Type == "33ZB"]      <- "Single High"
jaxd$DefShell[Covg.Type == "C1M/C3Z"]   <- "Single High"
jaxd$DefShell[Covg.Type == "C1M8"]      <- "Single High"
jaxd$DefShell[Covg.Type == "C1MF"]      <- "Single High"
jaxd$DefShell[Covg.Type == "C1MH"]      <- "Single High"
jaxd$DefShell[Covg.Type == "C1MR"]      <- "Single High"
jaxd$DefShell[Covg.Type == "C3Z"]       <- "Single High"
jaxd$DefShell[Covg.Type == "C3Z8"]      <- "Single High"
jaxd$DefShell[Covg.Type == "C3ZIsoB"]   <- "Single High"
jaxd$DefShell[Covg.Type == "C3ZIsoL"]   <- "Single High"
jaxd$DefShell[Covg.Type == "C3ZR8"]     <- "Single High"
jaxd$DefShell[Covg.Type == "C1M"]       <- "Single High"


jaxd$DefShell[Covg.Type == "C0"]        <- "Zero High"
jaxd$DefShell[Covg.Type == "C0MH"]      <- "Zero High"

jaxd$DefShell[Covg.Type == "C0/C2M"]    <- "Half High"
jaxd$DefShell[Covg.Type == "C2Z/C0MH"]  <- "Half High"
jaxd$DefShell[Covg.Type == "C2U/C1M"]   <- "Half High"

jaxd$DefShell[Covg.Type == "C2M"]       <- "Two High"
jaxd$DefShell[Covg.Type == "C2Z"]       <- "Two High"
jaxd$DefShell[Covg.Type == "C2Z/C2M"]   <- "Two High"
jaxd$DefShell[Covg.Type == "C2M/C2Z"]   <- "Two High"


jaxd$DefShell[Covg.Type == "C4Z"]       <- "Quarters Type"
jaxd$DefShell[Covg.Type == "C4Z8"]      <- "Quarters Type"
jaxd$DefShell[Covg.Type == "C6Z"]       <- "Quarters Type"
jaxd$DefShell[Covg.Type == "C4Z/C2M"]   <- "Quarters Type"
jaxd$DefShell[Covg.Type == "C2M/C4Z"]   <- "Quarters Type"
jaxd$DefShell[Covg.Type == "42ZB"]      <- "Quarters Type"
jaxd$DefShell[Covg.Type == "C6Z8"]      <- "Quarters Type"



jaxd$DefShell[Covg.Type == "PM"]        <- "Pattern Match"
jaxd$DefShell[Covg.Type == "C6PM"]      <- "Pattern Match"


# Create DVs for Covg Shells - slightly more general

jaxd$DVDefShell.Left  <- jaxd$DefShell.Left
jaxd$DVDefShell.Right <- jaxd$DefShell.Right

jaxd$DVDefShell.Left[jaxd$DVDefShell.Left == "ZB"]          <- "C3Z"
jaxd$DVDefShell.Left[jaxd$DVDefShell.Left == "C1MS"]        <- "C1M"
jaxd$DVDefShell.Left[jaxd$DVDefShell.Left == "C0MH"]        <- "C0"


jaxd$DVDefShell.Right[jaxd$DVDefShell.Right == "ZB"]        <- "C3Z"
jaxd$DVDefShell.Right[jaxd$DVDefShell.Right == "C3ZInvert"] <- "C3Z"
jaxd$DVDefShell.Right[jaxd$DVDefShell.Right == "C1MS"]      <- "C1M"
jaxd$DVDefShell.Right[jaxd$DVDefShell.Right == "C0MH"]      <- "C0"




# Need Formation type to be condensed in order to determine what type of receiver. 




#Gen Form Left

jaxd$Gen.Form.Left[Final.Form.Left == "Iso"]                  <- "Iso"
jaxd$Gen.Form.Left[Final.Form.Left == "Iso Tight Slot"]       <- "Iso"
jaxd$Gen.Form.Left[Final.Form.Left == "Iso Wing"]             <- "Iso"
jaxd$Gen.Form.Left[Final.Form.Left == "Pro"]                  <- "Iso"


jaxd$Gen.Form.Left[Final.Form.Left == "Stack"]                <- "Bunch"
jaxd$Gen.Form.Left[Final.Form.Left == "Tight Bunch"]          <- "Bunch"
jaxd$Gen.Form.Left[Final.Form.Left == "Tight Stack"]          <- "Bunch"
jaxd$Gen.Form.Left[Final.Form.Left == "Tight Stack Wing"]     <- "Bunch"
jaxd$Gen.Form.Left[Final.Form.Left == "Tight Trips"]          <- "Bunch"
jaxd$Gen.Form.Left[Final.Form.Left == "Tight Twins"]          <- "Bunch"

jaxd$Gen.Form.Left[Final.Form.Left == "Tight Iso"]            <- "Close"
jaxd$Gen.Form.Left[Final.Form.Left == "TE"]                   <- "Close"

jaxd$Gen.Form.Left[Final.Form.Left == "Twins"]                <- "Twins"

jaxd$Gen.Form.Left[Final.Form.Left == "Iso Slot Stack"]       <- "Trips"
jaxd$Gen.Form.Left[Final.Form.Left == "Trips"]                <- "Trips"
jaxd$Gen.Form.Left[Final.Form.Left == "Trips TE"]             <- "Trips"
jaxd$Gen.Form.Left[Final.Form.Left == "Twins TE"]             <- "Trips"
jaxd$Gen.Form.Left[Final.Form.Left == "Twins Wing"]           <- "Trips"

jaxd$Gen.Form.Left[is.na(Final.Form.Left)]           <- NA

# Gen.Form.Right

jaxd$Gen.Form.Right[Final.Form.Right == "Iso"]                  <- "Iso"
jaxd$Gen.Form.Right[Final.Form.Right == "Iso Tight Slot"]       <- "Iso"
jaxd$Gen.Form.Right[Final.Form.Right== "Iso Wing"]              <- "Iso"
jaxd$Gen.Form.Right[Final.Form.Right == "Pro"]                  <- "Iso"


jaxd$Gen.Form.Right[Final.Form.Right == "Stack"]                <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Bunch"]          <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Stack"]          <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Stack Wing"]     <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Trips"]          <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Twins"]          <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Twins Wing"]     <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Quads"]                <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Iso Wing"]       <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Double Wing"]          <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Tight Pro"]            <- "Bunch"
jaxd$Gen.Form.Right[Final.Form.Right == "Pro Tight"]            <- "Bunch"

jaxd$Gen.Form.Right[Final.Form.Right == "Tight Iso"]            <- "Close"
jaxd$Gen.Form.Right[Final.Form.Right == "TE"]                   <- "Close"

jaxd$Gen.Form.Right[Final.Form.Right == "Twins"]                <- "Twins"

jaxd$Gen.Form.Right[Final.Form.Right == "Iso Slot Stack"]       <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Trips"]                <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Trips TE"]             <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Twins TE"]             <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Twins Wing"]           <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Trips Jet"]            <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Stack Tight Slot"]     <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Stack Wing"]           <- "Trips"
jaxd$Gen.Form.Right[Final.Form.Right == "Iso Double Wing"]      <- "Trips"





# Need to do the same thing for DBAlign variables
# Create a column for "Matchup", 1 or 0. Turn the DB Align column into simpler sections

jaxd$Matchup[DBAlign == "BoxMU"]          <- 1
jaxd$Matchup[DBAlign == "CloudMU"]        <- 1
jaxd$Matchup[DBAlign == "Matchup"]        <- 1
jaxd$Matchup[DBAlign == "ZeroMU"]         <- 1
jaxd$Matchup[DBAlign == "SlotLP"]         <- 1
jaxd$Matchup[DBAlign == "SlotRP"]         <- 1

jaxd$Matchup[DBAlign == "2SBox"]          <- 0
jaxd$Matchup[DBAlign == "Box"]            <- 0
jaxd$Matchup[DBAlign == "Cloud"]          <- 0
jaxd$Matchup[DBAlign == "Invert"]         <- 0
jaxd$Matchup[DBAlign == "Liz"]            <- 0
jaxd$Matchup[DBAlign == "Mug"]            <- 0
jaxd$Matchup[DBAlign == "Quarters"]       <- 0
jaxd$Matchup[DBAlign == "Rip"]            <- 0
jaxd$Matchup[DBAlign == "SlotL"]          <- 0
jaxd$Matchup[DBAlign == "SlotR"]          <- 0
jaxd$Matchup[DBAlign == "Zero"]           <- 0
jaxd$Matchup[DBAlign == "ZeroMug"]        <- 0

# DBAlign level reduce (Zero, Rotate, Box, Cloud, Quarters, Man)

jaxd$IVDBAlign[DBAlign == "2SBox"]          <- "Zero"
jaxd$IVDBAlign[DBAlign == "Zero"]           <- "Zero"
jaxd$IVDBAlign[DBAlign == "ZeroMU"]         <- "Zero"
jaxd$IVDBAlign[DBAlign == "ZeroMug"]        <- "Zero"

jaxd$IVDBAlign[DBAlign == "Box"]            <- "Box"
jaxd$IVDBAlign[DBAlign == "BoxMU"]          <- "Box"
jaxd$IVDBAlign[DBAlign == "Mug"]            <- "Box"

jaxd$IVDBAlign[DBAlign == "Rip"]            <- "Rotate"
jaxd$IVDBAlign[DBAlign == "Liz"]            <- "Rotate"
jaxd$IVDBAlign[DBAlign == "SlotL"]          <- "Rotate"
jaxd$IVDBAlign[DBAlign == "SlotR"]          <- "Rotate"
jaxd$IVDBAlign[DBAlign == "Invert"]         <- "Rotate"

jaxd$IVDBAlign[DBAlign == "Matchup"]        <- "Man"
jaxd$IVDBAlign[DBAlign == "SlotLP"]         <- "Man"
jaxd$IVDBAlign[DBAlign == "SlotRP"]         <- "Man"

jaxd$IVDBAlign[DBAlign == "Cloud"]          <- "Quarters Type"
jaxd$IVDBAlign[DBAlign == "CloudMU"]        <- "Quarters Type"
jaxd$IVDBAlign[DBAlign == "Quarters"]       <- "Quarters Type"

jaxd$IVDBAlign <- as.factor(jaxd$IVDBAlign)



# Changing Coverage Type over leftmost and rightmost receiver

jaxd$CovgLWR[Covg.Over.1 == "M"]          <- "Man"
jaxd$CovgLWR[Covg.Over.1 == "HLZ"]        <- "Zone"
jaxd$CovgLWR[Covg.Over.1 == "IOZ"]        <- "Zone"
jaxd$CovgLWR[Covg.Over.1 == "PM"]         <- "Man"
jaxd$CovgLWR[Covg.Over.1 == "SM"]         <- "Man"
jaxd$CovgLWR[Covg.Over.1 == "DBL"]        <- "Man"

jaxd$CovgLWR <- as.factor(jaxd$CovgLWR)

jaxd$CovgRWR[Covg.Over.5 == "M"]          <- "Man"
jaxd$CovgRWR[Covg.Over.5 == "HLZ"]        <- "Zone"
jaxd$CovgRWR[Covg.Over.5 == "IOZ"]        <- "Zone"
jaxd$CovgRWR[Covg.Over.5 == "PM"]         <- "Man"
jaxd$CovgRWR[Covg.Over.5 == "SM"]         <- "Man"
jaxd$CovgRWR[Covg.Over.5 == "DBL"]        <- "Man"

jaxd$CovgRWR <- as.factor(jaxd$CovgRWR)

# *Note* Pattern Match often looks presnap like zone but functions like man,
# so PM was converted to man coverage.

summary(jaxd$CovgLWR)
summary(jaxd$CovgRWR)

# As you can see, our baseline numbers are similar for proportion of coverage
# types, although it's important to note that not all plays had the same type
# of coverage for both the leftmost and rightmost receiving threat. 




# Relationship Exploration ------------------------------------------------------

# Primary Variables of interest


# Dist      - man seems likely to be more prevalent in 3rd and short situations

# YdLine    - distance from endzone/redzone may affect how aggressive/conservative
# defensive playcalls are

# MOFOPre   - indicative of coverage shell, Open may indicate a Cover 2 look

# LCBLev, Depth, Body - relative positions of respective CB are highly indicative
# of the technique they are playing. Press implies Man, Open with outside leverage
# implies Zone

# DeepestAtSnap - depth of primary deep defender may indicate their area of 
# responsibility

# Gen.Form.Left/Right - different type of offensive formation (i.e. bunch formation 
# vs. isolated receiver) can dictate how defenses play and align in man vs. zone

# Matchup   - if specific positional players are obviously matched up on specific 
# offensive players, highly indicative of man- i.e., if a fullback is split out 
# wide and a linebacker is lined up at cornerback ostensibly to cover him, then
# this is most likely man coverage


# IVDBAlign - certain overall defensive alignments carry specific responsibilities
# which can be used to deduce their actions on each play





# Predictor Exploration

# Distance
DistanceDensity <- ggplot(jaxd, aes(x=jaxd$Dist, fill=CovgRWR)) + geom_density(alpha = 0.4) + 
  labs(title="Coverage by Distance", x="Yards to Go", y = "Density") 
DistanceDensity
ggsave("DistanceDensity.png")
# Man is called more frequently within 5 yards

CrossTable(jaxd$CovgRWR, jaxd$ThirdAnd, digits = 2, prop.r = TRUE, prop.c = FALSE, 
           prop.chisq = FALSE)

# Values in CrossTable are count, proportion of row, proportion of table respectively



# YdLine Density Plot

YdLineDensity <- ggplot(jaxd, aes(x=jaxd$YdLine, fill=CovgRWR)) + geom_density(alpha = 0.4)+ 
  labs(title="Coverage by Yard Line", x="Yard Line", y = "Density")
# Seems Zone is more common in defense's redzone due to constricted space
YdLineDensity
ggsave("YdLineDensity.png")

# LCB Positions (Lev, Depth, Body)

# CrossTable for Body Position
CrossTable(jaxd$CovgRWR, jaxd$LCBBody, digits = 2, prop.r = TRUE, prop.c = FALSE,
           prop.chisq = FALSE)
# For Body - 64/69 Press snaps were man. 32/46 Open snaps were Zone. 

# Density for LCB Leverage
ggplot(jaxd, aes(x=jaxd$LCBLev, fill=CovgRWR)) + geom_density(alpha = 0.4)
# Inside alignment had a higher likelihood of being Man, head up alignment had a very
# high likelihood of being man, and outside alignment was more likely zone. 

CBPosDF <- data.frame(jaxd$LCBLev, jaxd$LCBDepth, jaxd$RCBLev, jaxd$RCBDepth)



# DeepestAtSnap
ggplot(jaxd, aes(x=jaxd$DeepestAtSnap, fill=CovgRWR)) + geom_density(alpha = 0.4) 
# Mode of Man coverage is around 16 whereas mode of Zone appears to be around 14. 

# MOFOPresnap
CrossTable(jaxd$CovgRWR, jaxd$MOFOPre, digits = 2, prop.chisq = FALSE, prop.c = FALSE)
# 78% of all MOFClosed snaps were Man, 61% of all MOFOpen snaps were Zone.

# GenForm.Right
CrossTable(jaxd$CovgRWR, jaxd$Gen.Form.Right, digits = 2, prop.chisq = FALSE, prop.c = FALSE)
# Although there are some interesting and possibly significant differences in group means,
# we will exclude this variable from the model. To include it would imply that there is
# some element of defensive playcalling that occurs after the offense lines up, which, while
# probably true, is outside the scope of this study.


# Matchup
CrossTable(jaxd$CovgRWR, jaxd$Matchup, digits = 2, prop.chisq = FALSE, prop.c = FALSE)
# If the defensive backs align in a Matchup alignment, this is HEAVILY indicative of 
# Man coverage. 


#IVDBAlign
CrossTable(jaxd$CovgRWR, jaxd$IVDBAlign, digits = 2, prop.chisq = FALSE, prop.c = FALSE)
# While there are some noticeable differences in group means, this is too similar to
# the matchup binary variable, and thus will also be excluded. 



# LCB Alignment Plot

LCBalignment <- ggplot(jaxd, aes(x=jaxd$LCBLev, y=jaxd$LCBDepth, color=jaxd$CovgRWR, shape = 
                    LCBBody)) + geom_jitter(size = 2) + 
  labs(title="LCB Alignment", x="Horizontal Alignment", y = "Depth") 
LCBalignment
ggsave("LCBalignment.png")













  
  

  
#----------- MLR Package -----------

# 8 Variables : LCBLev, LCBDepth, Matchup, LCBBody, Dist, YdLine, DeepestAtSnap, MOFOPre 

treeRWRDF <- data.frame(jaxd$LCBLev, jaxd$LCBDepth, jaxd$Matchup,  jaxd$LCBBody,
                          jaxd$Dist, jaxd$YdLine, jaxd$DeepestAtSnap, jaxd$MOFOPre,
                        jaxd$CovgRWR)


n            	    <- nrow(treeRWRDF)
trainCovgRWR        <- sample(n, size = .8*n)
testCovgRWR         <- setdiff(1:n, trainCovgRWR)
jaxd.trainCovgRWR 	<- treeRWRDF[trainCovgRWR,]
jaxd.testCovgRWR    <- treeRWRDF[testCovgRWR, ]


CovgRTask <- makeClassifTask(data = jaxd.trainCovgRWR, target = "jaxd.CovgRWR")

resample.stratRWR <- makeResampleDesc(method = "CV", iters = 3)


tune.methodRWR <- makeTuneControlRandom(maxit = 10L)

tree.algRWR <- makeLearner("classif.rpart", predict.type = "response")


params.treeRWR <- makeParamSet(makeIntegerParam("minsplit",lower = 5, upper = 50), 
                              makeIntegerParam("minbucket", lower = 10, upper = 80), 
                              makeNumericParam("cp", lower = 0.001, upper = 0.2))


tuned.treeRWR <- tuneParams(learner = tree.algRWR,
                           task = CovgRTask,
                           resampling = resample.stratRWR,
                           measures = list(acc, ber), 
                           par.set = params.treeRWR,
                           control = tune.methodRWR,
                           show.info = TRUE)

tree.algRWR <- setHyperPars(learner = tree.algRWR, par.vals = tuned.treeRWR$x)


resample.treeRWR <- resample(learner = tree.algRWR, task = CovgRTask, resampling = resample.stratRWR, measures = list(acc,ber))

final.treeRWR <- train(learner = tree.algRWR, task = CovgRTask)

prediction.treeRWR <- predict(final.treeRWR, newdata = jaxd.testCovgRWR)



# Model Evaluation (1 iteration)


# Naive Assumption (classifying all coverages as "Man"), correctness would be 25/38 for the  
# test set.

25/38 


performance(prediction.treeRWR, measures = list(acc,ber))

# Accuracy is 84.21%, or an improvement of almost 19% accuracy over the naive assumption.
# Run 10 times, the average correct prediction rate was 82.63%, which would be an almost
# 18% improvement over the Naive prediction, which would have a 64.74% accuracy rate if 
# Man was predicted for all 190 instances in the sample size. 
# However, this jump in accuracy comes with a far more valuable jump in specific 
# play-by-play prediction.

# Balanced Error Rate, or the mean of each coverage's misclassification rate, is 12.00%. 
# The fact that it is lower than the overall misclassification rate tells us that the 
# model more frequently incorrectly predicts Man to be Zone than Zone to be Man. We can 
# utilize this in on-field recognition through being more confident if it predicts Man,
# that it is actually Man.





treeRWRresults <- data.frame(prediction.treeRWR$data)

treeRWRresults$TruthDummy[treeRWRresults$truth == "Man"]          <- 1
treeRWRresults$ResponseDummy[treeRWRresults$response == "Man"]    <- 1
treeRWRresults$TruthDummy[treeRWRresults$truth == "Zone"]         <- 0
treeRWRresults$ResponseDummy[treeRWRresults$response == "Zone"]   <- 0


treeRWRresults$Correct[treeRWRresults$TruthDummy == 1 & treeRWRresults$ResponseDummy == 1] <- 1
treeRWRresults$Correct[treeRWRresults$TruthDummy == 0 & treeRWRresults$ResponseDummy == 0] <- 1
treeRWRresults$Correct[treeRWRresults$TruthDummy == 0 & treeRWRresults$ResponseDummy == 1] <- 0
treeRWRresults$Correct[treeRWRresults$TruthDummy == 1 & treeRWRresults$ResponseDummy == 0] <- 0

summary(treeRWRresults)

# Ultimately, we should not be satisfied with simply having an improvement over the naive
# prediction. It's important to recognize that much of this prediction is available to the
# quarterback simply by scanning the defense in the seconds that precede the snap. For this
# reason, a 84.21% success rate is useful, but still not enough have certainty.
# Another approach to this type of assistive analysis could have been determining the 
# degree of correlation between observable characteristics and the type of coverage, which
# in turn could be used to tell the quarterback what he should be keying on and reading
# in order to correctly diagnose the coverage.
  



