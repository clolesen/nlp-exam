##### DATA ####

# Make data frame combining true and artificial results for plotting and analysis
plotData = data.frame(
  Diversity = newResults$Diversity,
  Surprise = c(subset(Results[[1]], minPrevPub > 1 & nCollab %in% 2:4)$Surprise, newResults$Surprise),
  Condition = factor(c(rep("True", 663), rep("Artificial", 663)), levels = c("True", "Artificial")),
  nCollab = newResults$nCollab,
  docTime = newResults$docTime
)

# Make column with centered surprise values
plotData$SurpriseCentered[plotData$Condition=="True"] = scale(plotData$Surprise[plotData$Condition=="True"],scale = F)
plotData$SurpriseCentered[plotData$Condition=="Artificial"] = scale(plotData$Surprise[plotData$Condition=="Artificial"],scale = F)

# Make collumns with categorical diversity, surprise and time variables
for(i in 1:length(plotData[,1])){
  div = plotData[i,1]
  sur = plotData[i,2]
  time = plotData[i,"docTime"]
  
  if (div < 1) divClass = "low"
  if (div >= 1 & div < 2) divClass = "mid low"
  if (div >= 2 & div < 3) divClass = "mid high"
  if (div >= 3) divClass = "high"
  
  if (sur < 1) surClass = "low"
  if (sur >= 1 & sur < 2) surClass = "mid low"
  if (sur >= 2 & sur < 3) surClass = "mid high"
  if (sur >= 3) surClass = "high"
  
  if (time < 5) timeClass = "0-5"
  if (time >= 5 & time < 10) timeClass = "5-10"
  if (time >= 10 & time < 15) timeClass = "10-15"
  if (time >= 15 & time < 20) timeClass = "15-20"
  if (time >= 20 & time < 25) timeClass = "20-25"
  
  plotData$DiversityClass[i] = divClass
  plotData$SurpriseClass[i] = surClass
  plotData$timeClass[i] = timeClass
}

# Make right factor levels
plotData$timeClass = factor(plotData$timeClass, levels = c("0-5","5-10","10-15","15-20","20-25"))


# Load Agriculture and finacne results
load("data/R_objects/agriculture/mainResults.Rdata")
mainResults_agri = mainResults

load("data/R_objects/finance/mainResults.Rdata")
mainResults_fin = mainResults


# Define colors
green = "#009F40"
blue = "#56B6E9"
orange = "#E69F00"
yellow = "#F0E442"
pink = "#CC79A7"

##### PLOTS ####

# Plots used in paper
mainPlot = ggplot(subset(plotData, Condition == "True"), aes(x = Diversity, y = Surprise)) +
  geom_point(size = 1,  alpha = .3, shape = 21, fill = green, color = "black") +
  geom_smooth(method='lm', color = "black", fill = "#888888", size = 2) +
  geom_smooth(method='lm', fill = F, color = green, size = 1.4) +
  theme_minimal() +
  theme(text=element_text(face = "bold")) 

ggsave("plots/mainPlot.png", mainPlot, width = 4, height = 4)


intPlot = ggplot(plotData, aes(x = Diversity, y = SurpriseCentered, color = Condition)) +
  geom_smooth(method = "lm", size = 2, aes(fill = Condition), color = "black") +
  geom_smooth(method = "lm", fill = F, size = 1.4) +
  theme_minimal() +
  theme(text=element_text(family="Helvetica", face = "bold"),legend.position = "none") +
  scale_color_manual(values=c(green,orange)) +
  scale_fill_manual(values=c("#888888","#888888")) +
  ylab("Surprise Centered")

ggsave("plots/intPlot.png", intPlot, width = 4, height = 4)



SurDivTimePlot = ggplot(subset(plotData, Condition == "True"), aes(x = Diversity, y = Surprise, color = timeClass)) +
  geom_point(size = 1,  alpha = 1, shape = 1, fill = "white") +
  geom_smooth(method='lm', color = "black", size = 2, aes(fill = timeClass)) +
  geom_smooth(method='lm', fill = F, size = 1.4) +
  theme_minimal() +
  labs(color = "Intervals", fill =  "Intervals") +
  theme(text=element_text(face = "bold")) +
  scale_color_manual(values=c(green,orange,blue,yellow,pink)) +
  scale_fill_manual(values=c(F,F,F,F,F))

ggsave("plots/SurDivTimePlot.png", SurDivTimePlot, width = 4, height = 4)



densTimePlot = ggplot(subset(Results[[2]], nColllab %in% 1:4), aes(x = time)) +
  geom_density(adjust = .5, fill = "#EEEEEE", size = 2) +
  geom_density(adjust = .5, fill = F, color = green, size = 1.4) +
  facet_wrap(~nColllab) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(text=element_text(face = "bold")) +
  labs(x = "Time", y = "Density")

ggsave("plots/densTimePlot.png", densTimePlot, width = 4, height = 4)


#### STATS ####

# LDA Model stats
mean(Results[[3]]$coherence)
Results[[3]]$r2
textmineR::GetTopTerms(Results[[3]]$phi,6)

#Model 1
summary(lm(Surprise ~ Diversity, subset(Results[[1]], minPrevPub %in% 2:20 & nCollab %in% 2:4)))

# Model 1.1 (testing simulated articles)
summary(lm(Surprise ~ Diversity, newResults))

# Model 2
summary(lm(SurpriseCentered ~ Diversity*Condition, plotData))

# Model 3
summary(lm(Surprise ~ Diversity*docTime, subset(Results[[1]], minPrevPub %in% 2:20 & nCollab %in% 2:4)))



#### OTHER STUFF ####

# How many docs are there in time < 5 after exclusion (the answer is 8)
length(subset(Results[[1]], minPrevPub %in% 2:20 & nCollab %in% 1:4)$docTime[subset(Results[[1]], minPrevPub %in% 2:20 & nCollab %in% 1:4)$docTime<5])


# Printing top terms in way that can be copied and put in a document
topterms = textmineR::GetTopTerms(Results[[3]]$phi,6)
for (i in 101:200){
  text = paste("Topic",i,
               "\n",topterms[1,i],
               "\n",topterms[2,i],
               "\n",topterms[3,i],
               "\n",topterms[4,i],
               "\n",topterms[5,i],
               "\n",topterms[6,i],
               "\n","\n"
  )
  cat(text)
}

# Testing the mean of suprise to previous publication between solo articles and collaborations
t.test(
  subset(Results[[2]],nColllab==1 & PubNum != 1)$SurToPrev,
  subset(Results[[2]],nColllab>1 & PubNum != 1)$SurToPrev
)

#### OTHER PLOTS ####
ggplot(subset(plotData, Condition == "True"), aes(x = docTime, y = Surprise)) +
  geom_point(size = .5) +
  #geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  #facet_wrap(~DiversityClass) +
  theme_classic()

ggplot(subset(plotData, Condition == "True"), aes(x = docTime, y = Diversity)) +
  geom_point(size = .5) +
  #geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  #facet_wrap(~SurpriseClass) +
  theme_classic()

ggplot(subset(plotData, SurpriseClass == "low" & Condition == "True"), aes(x = docTime, y = Surprise, color = DiversityClass)) +
  geom_point(size = .5) +
  #geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  #geom_density2d() +
  theme_classic()


ggplot(subset(Results[[2]], nColllab %in% 1:4), aes(x = time)) +
  geom_density(adjust = .5, fill = "lightgray") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank())



ggplot(subset(Results[[1]], minPrevPub %in% 2:20 & nCollab %in% 1:4), aes(x = docTime)) +
  geom_density(adjust = .5, fill = "#EEEEEE", size = 2) +
  geom_density(adjust = .5, fill = F, color = green, size = 1.4) +
  facet_wrap(~nCollab) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(text=element_text(face = "bold")) +
  labs(x = "Time", y = "Density")


ggplot(subset(Results[[2]], nColllab %in% 1:4 & PubNum > 1), aes(x = SurToPrev, color = as.factor(nColllab))) +
  geom_density()

ggplot(subset(Results[[2]], nColllab %in% 1:4 & PubNum == MaxPubNum & PubNum %in% 2:17), aes(x = SurToFirst, color = as.factor(PubNum))) +
  geom_density() +
  theme_classic()

ggplot(subset(Results[[2]], nColllab %in% 1:4 & PubNum == MaxPubNum & PubNum %in% 3:10), aes(y = SurToFirst, x = time, color = as.factor(PubNum))) +
  geom_point(size = .1) 

ggplot(subset(Results[[2]], nColllab %in% 1:4 & PubNum == 1 & MaxPubNum %in% 3:10), aes(y = SurToLast, x = time)) +
  geom_point(size = .1) 

ggplot(subset(Results[[2]], nColllab %in% 1:4 & PubNum > 2), aes(y = SurToPrev, x = time, color = as.factor(nColllab))) +
  geom_point(size = .1) +
  theme_classic()

ggplot(subset(Results[[1]], minPrevPub %in% 2:8), aes(x = Diversity, y = Surprise, color = as.factor(minPrevPub) )) +
  geom_point(size = 1,  alpha = .4) +
  geom_smooth(method='lm', fill = green, size=1.5)

ggplot(subset(Results[[1]], minPrevPub > 1), aes(x = Diversity, y = Surprise)) +
  geom_smooth(method='lm', fill = green, size=1.5, color = "red") +
  geom_smooth(data = subset(Results[[1]], minPrevPub > 5), method='lm', fill = green, size=1.5, color = "blue")


