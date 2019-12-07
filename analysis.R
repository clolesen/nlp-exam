##### PLOTS ####


plotData = data.frame(
  Diversity = newResults$Diversity,
  Surprise = c(subset(Results[[1]], minPrevPub > 1 & nCollab %in% 2:4)$Surprise, newResults$Surprise),
  Condition = factor(c(rep("True", 346), rep("Artificial", 346)), levels = c("True", "Artificial")),
  nCollab = newResults$nCollab
)

meanNonCollab = mean(subset(Results[[2]],nColllab==1 & PubNum != 1)$SurToPrev)
sdNonCollab = sd(subset(Results[[2]],nColllab==1 & PubNum != 1)$SurToPrev)

blue = "#6B9BCF"
orange = "#DE8244"
green = "#B2CF94"

mainPlot = ggplot(plotData, aes(x = Diversity, y = Surprise, color = Condition)) +
  geom_point(size = 1,  alpha = .4) +
  geom_smooth(method='lm', fill = green, size=1.5) +
  theme_classic() +
  geom_hline(yintercept=meanNonCollab, size = .7) +
  geom_hline(yintercept=meanNonCollab-sdNonCollab, size = .5, linetype = "dashed") +
  geom_hline(yintercept=meanNonCollab+sdNonCollab, size = .5, linetype = "dashed") +
  theme(text=element_text(family="Helvetica", face = "bold", size = 24),legend.position = "none") +
  scale_color_manual(values=c(blue,orange))

ggsave("mainPlot.png", mainPlot, units = "cm", width = 16, height = 13, dpi = 150)


subPlot = ggplot(plotData, aes(x = Diversity, y = SurpriseCentered, color = Condition)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = "lm", fill = green, size=1.5) +
  facet_wrap(~nCollab) +
  theme_classic() +
  theme(text=element_text(family="Helvetica", face = "bold", size = 24),legend.position = "none") +
  scale_color_manual(values=c(blue,orange)) +
  xlim(0,4) +
  ylab("")

ggsave("subPlot.png", subPlot, units = "cm", width = 32, height = 12, dpi = 150)


plotData$SurpriseCentered[plotData$Condition=="True"] = scale(plotData$Surprise[plotData$Condition=="True"],scale = F)
plotData$SurpriseCentered[plotData$Condition=="Artificial"] = scale(plotData$Surprise[plotData$Condition=="Artificial"],scale = F)

ggplot(plotData, aes(x = Diversity, y = SurpriseCentered, color = Condition)) +
  geom_point(size = 0.3,  alpha = .3) +
  geom_smooth(method='lm') +
  theme_classic()

ggplot(plotData, aes(x = Diversity, y = SurpriseCentered, color = Condition)) +
  geom_point(size = 0.3, alpha = .5) +
  geom_smooth(method = "lm") +
  facet_wrap(~nCollab) +
  theme_classic()


# PLOTS OVER TIME
ggplot(subset(Results[[1]], minPrevPub > 1), aes(x = docTime, y = Surprise)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  theme_classic()

ggplot(subset(Results[[1]], minPrevPub > 1), aes(x = docTime, y = Diversity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  theme_classic()

ggplot(subset(Results[[2]], nColllab %in% 1:6), aes(x = time)) +
  geom_density(adjust = .5, fill = "lightgray") +
  facet_wrap(~nColllab) +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks = element_blank())

ggplot(subset(Results[[1]], minPrevPub %in% 2:20 & docTime > 15), aes(x = Diversity, y = Surprise)) +
  geom_point(size = 0.3) +
  geom_smooth(method='lm') +
  theme_classic()


#### STATS ####
summary(lm(Surprise ~ Diversity*nCollab, subset(Results[[1]], minPrevPub %in% 2:20 & nCollab %in% 2:4)))

summary(lm(Surprise ~ Diversity*nCollab, newResults))

t.test(
  subset(Results[[2]],nColllab==1 & PubNum != 1)$SurToPrev,
  subset(Results[[1]], minPrevPub %in% 2:20)$Surprise
)
