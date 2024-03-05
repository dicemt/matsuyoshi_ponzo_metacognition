## LIBRARIES----------------------
source("subfunctions.R",encoding="UTF-8")
library(tidyverse)
library(R.matlab)
library(HDInterval)
theme_set(theme_void(base_family = "sans"))

## Load data ----
ilsup <- read.csv("data/superiority_data.csv")
ponzometa <- read.csv("data/ponzo.csv")
meanconf <- read.csv("data/meanconf.csv")

# Calculation
ponzometa$d_rel <- ponzometa$d1_1 - ponzometa$d1_0
ponzometa$c_rel <- ponzometa$c1_1 - ponzometa$c1_0
ponzometa$metac_rel <- ponzometa$metac_1 - ponzometa$metac_0
ponzometa$mceff_rel <- ponzometa$mceff_1 - ponzometa$mceff_0
ponzometa$ln_mceff_rel <- ponzometa$ln_mceff_1 - ponzometa$ln_mceff_0

ponzometa$ilsup <- ilsup$value

des <- psych::describe(ponzometa)
print(des,digits=3)

## t-tests ----
# Each condition
t.test(ponzometa$ilsup)
t.test(ponzometa$d1_0)
t.test(ponzometa$d1_0)$p.value
t.test(ponzometa$d1_1)
t.test(ponzometa$c1_0)
t.test(ponzometa$c1_1)
t.test(ponzometa$metac_0)
t.test(ponzometa$metac_1)

cohens_d(ponzometa$ilsup)
cohens_d(ponzometa$d1_0)
cohens_d(ponzometa$d1_1)
cohens_d(ponzometa$c1_0)
cohens_d(ponzometa$c1_1)
cohens_d(ponzometa$metac_0)
cohens_d(ponzometa$metac_1)

# Depth-Control
t.test(ponzometa$d_rel)
t.test(ponzometa$c_rel)
t.test(ponzometa$metac_rel)
cohens_d(ponzometa$d_rel)
cohens_d(ponzometa$c_rel)
cohens_d(ponzometa$metac_rel)

# MAP & HDI (Group-level): Log-Mratio
mcmc0 <- R.matlab::readMat("data/mcmc0.mat")
mcmc0 <- lapply(mcmc0, unlist, use.names=FALSE)
mcmc0 <- as.vector(mcmc0$mcmc0)
mc_density0 <- density(mcmc0,bw=bw_silverman(mcmc0),n=100)
mc_density0$x[match(max(mc_density0$y),mc_density0$y)] # find an index that gives max y
HDInterval::hdi(mcmc0)

mcmc1 <- R.matlab::readMat("data/mcmc1.mat")
mcmc1 <- lapply(mcmc1, unlist, use.names=FALSE)
mcmc1 <- as.vector(mcmc1$mcmc1)
mc_density1 <- density(mcmc1,bw=bw_silverman(mcmc1),n=100)
mc_density1$x[match(max(mc_density1$y),mc_density1$y)] # find an index that gives max y
HDInterval::hdi(mcmc1)

mcmcDiff <- mcmc1-mcmc0
mc_densityDiff <- density(mcmcDiff,bw=bw_silverman(mcmcDiff),n=100)
mc_densityDiff$x[match(max(mc_densityDiff$y),mc_densityDiff$y)] # find an index that gives max y
HDInterval::hdi(mcmcDiff)

# MAP & HDI (Group-level): Mratio 
mcmc0exp <- exp(mcmc0)
mc_density0exp <- density(mcmc0exp,bw=bw_silverman(mcmc0exp),n=100)
mc_density0exp$x[match(max(mc_density0exp$y),mc_density0exp$y)] # find an index that gives max y
HDInterval::hdi(mcmc0exp)

mcmc1exp <- exp(mcmc1)
mc_density1exp <- density(mcmc1exp,bw=bw_silverman(mcmc1exp),n=100)
mc_density1exp$x[match(max(mc_density1exp$y),mc_density1exp$y)] # find an index that gives max y
HDInterval::hdi(mcmc1exp)

mcmcDiffexp <- mcmc1exp-mcmc0exp
mc_densityDiffexp <- density(mcmcDiffexp,bw=bw_silverman(mcmcDiffexp),n=100)
mc_densityDiffexp$x[match(max(mc_densityDiffexp$y),mc_densityDiffexp$y)] # find an index that gives max y
HDInterval::hdi(mcmcDiffexp)

# Mean confidence (metacognitive bias)
t.test(meanconf$depth,meanconf$control,paired=T)
cohens_d(meanconf$depth-meanconf$control)

## Create data frames for ggplot2 ----
# Data frame for ggplot2 (Depth vs Control)
sdt_raw <- ponzometa[,c(1:11)] %>% tidyr::pivot_longer(cols = -c("id"),names_to = c("index"), values_to = "val")
sdt_raw$index <- factor(sdt_raw$index,levels=colnames(ponzometa)[c(11:1)])
sdt_raw_means <- sdt_raw %>% group_by(index) %>% summarize(mean=mean(val), conf_lwr = mean(val) - qt(0.975, df=length(val)-1)*sd(val)/sqrt(length(val)), conf_upr = mean(val) + qt(0.975, df=length(val)-1)*sd(val)/sqrt(length(val)))
# Assign hBayesian estimates
sdt_raw_means[1,2] <- mc_density1$x[match(max(mc_density1$y),mc_density1$y)]
sdt_raw_means[1,3:4] <- t(HDInterval::hdi(mcmc1)[1:2])
sdt_raw_means[2,2] <- mc_density0$x[match(max(mc_density0$y),mc_density0$y)]
sdt_raw_means[2,3:4] <- t(HDInterval::hdi(mcmc0)[1:2])
sdt_raw_means[3,2] <- mc_density1exp$x[match(max(mc_density1exp$y),mc_density1exp$y)]
sdt_raw_means[3,3:4] <- t(HDInterval::hdi(mcmc1exp)[1:2])
sdt_raw_means[4,2] <- mc_density0exp$x[match(max(mc_density0exp$y),mc_density0exp$y)]
sdt_raw_means[4,3:4] <- t(HDInterval::hdi(mcmc0exp)[1:2])
sdt_raw_means # updated

# Data frame for ggplot2 (Diff)
sdt_rel <- ponzometa[,c(1,12:16)] %>% tidyr::pivot_longer(cols = -c("id"),names_to = c("index"), values_to = "val")
sdt_rel$index <- factor(sdt_rel$index,levels=colnames(ponzometa)[c(16:12)])
sdt_rel_means <- sdt_rel %>% group_by(index) %>% summarize(mean=mean(val), conf_lwr = mean(val)-qt(0.975, df=length(val)-1)*sd(val)/sqrt(length(val)), conf_upr = mean(val)+qt(0.975, df=length(val)-1)*sd(val)/sqrt(length(val)) ) 
# Assign hBayesian estimates
sdt_rel_means[1,2] <- mc_densityDiff$x[match(max(mc_densityDiff$y),mc_densityDiff$y)]
sdt_rel_means[1,3:4] <- t(HDInterval::hdi(mcmcDiff)[1:2])
sdt_rel_means[2,2] <- mc_densityDiffexp$x[match(max(mc_densityDiffexp$y),mc_densityDiffexp$y)]
sdt_rel_means[2,3:4] <- t(HDInterval::hdi(mcmcDiffexp))
sdt_rel_means # updated

## Correlations ----
# dprime (rel)
cor_spearman.test(ponzometa$d_rel,ponzometa$ilsup)
# Log M-ratio (rel)
cor_spearman.test(ponzometa$ln_mceff_rel,ponzometa$ilsup)
# dprime vs log M-ratio
cor_spearman.test(ponzometa$d_rel,ponzometa$ln_mceff_rel)

# Partial Correlations (each other)
pcor_ci_spearman.test(ponzometa$ilsup,ponzometa$d_rel,ponzometa$ln_mceff_rel)
pcor_ci_spearman.test(ponzometa$ilsup,ponzometa$ln_mceff_rel,ponzometa$d_rel)
#ppcor::pcor.test(ponzometa$ilsup,ponzometa$d_rel,ponzometa$ln_mceff_rel,"spearman")
#ppcor::pcor.test(ponzometa$ilsup,ponzometa$ln_mceff_rel,ponzometa$d_rel,"spearman")

# Partial Correlations (controlling for age)
pcor_ci_spearman.test(ponzometa$ilsup,ponzometa$d_rel,ilsup$age)
pcor_ci_spearman.test(ponzometa$ilsup,ponzometa$ln_mceff_rel,ilsup$age)
#ppcor::pcor.test(ponzometa$ilsup,ponzometa$d_rel,ilsup$age,"spearman")
#ppcor::pcor.test(ponzometa$ilsup,ponzometa$ln_mceff_rel,ilsup$age,"spearman")

# C (rel)
cor_spearman.test(ponzometa$c_rel,ponzometa$ilsup)

# Meta-C (rel)
cor_spearman.test(ponzometa$metac_rel,ponzometa$ilsup)

# Metacognitive bias (Mean confidence)
cor_spearman.test(meanconf$depth - meanconf$control,ponzometa$ilsup)

# dprime (Control)
cor_spearman.test(ponzometa$d1_0,ponzometa$ilsup)
# C (Control)
cor_spearman.test(ponzometa$c1_0,ponzometa$ilsup)
# Meta-C (Control)
cor_spearman.test(ponzometa$metac_0,ponzometa$ilsup)
# M-ratio (Control) # SAME AS Log M-ratio
#cor_spearman.test(ponzometa$mceff_0,ponzometa$ilsup)
# Log M-ratio (Control)
cor_spearman.test(ponzometa$ln_mceff_0,ponzometa$ilsup)

# Criterion and metacognitive performance
cor_spearman.test(ponzometa$c_rel,ponzometa$ln_mceff_rel)
cor_spearman.test(ponzometa$c_rel,meanconf$depth - meanconf$control)

# hBayes with SI as a covariate ----
simreg <- read.csv("data/reglogMratio.csv")
simreg$ln_mceff_rel <- simreg$ln_mceff_1 - simreg$ln_mceff_0
cor_spearman.test(simreg$ln_mceff_rel,ilsup$value)

## ggplots theme ----
# general theme
theme_set(theme_void(base_family = "sans"))

theme_update(
  axis.text.x = element_text(color = "black", size = 16, 
                             margin = margin(t = 6)),
  axis.text.y = element_text(color = "black", size = 16, hjust = 1, 
                             margin = margin(r = 6)),
  axis.line.x = element_line(color = "black", linewidth = 1),
  panel.grid.major.y = element_line(color = "grey90", linewidth = .6),
  plot.margin = margin(rep(20, 4)),
  axis.title.x = element_text(size=22, lineheight=.9, face = "bold"), 
  axis.title.y = element_text(size=22, lineheight=.9, face = "bold", angle = 90 )
)

# custom colors
my_pal <- c("#0041FF","#FF2800","#9A0079","#FAF500","#35A16B") #

## ggplot illusory superiority [Figure 2a]----
ils_raw <- ilsup[,c(1,4)] %>% tidyr::pivot_longer(cols = -c("id"),names_to = c("index"), values_to = "val")
ils_raw$index <- factor(ils_raw$index,levels=colnames(ponzometa)[16:12])
ils_raw_means <- ils_raw %>% group_by(index) %>% summarize(mean=mean(val), ci = qt(0.975, df=length(val)-1)*sd(val)/sqrt(length(val)))

cl <- wesanderson::wes_palette("Cavalcanti1")

g <- ggplot(ils_raw, aes(x = index, y = val, color = index)) +
  geom_jitter(size = 3, alpha = 0.3, width = 0.2) +
  geom_point(data = ils_raw_means, aes(x=index,y=mean,fill = index), size = 5, colour = "grey30", shape = 21) +
  geom_errorbar(data = ils_raw_means, aes(x = index, y= NULL,ymin= mean - ci, ymax = mean + ci), width=.2) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey30") +
  coord_flip() +
  scale_colour_manual(values = rep(c(cl[2]),5) ) +
  scale_fill_manual(values = rep(c(cl[2]),5) ) +
  scale_y_continuous("Value",breaks = seq(-1,1,by=0.5),limits = c(-1, 1)) +
  scale_x_discrete(labels="Superiority") +
  guides(colour = "none", fill = "none")
g

## ggplot SDT1 [Figure 2b]----
metaLabels <- c("ln Eff (Depth)","ln Eff (Control)",
                "Efficiency (Depth)","Efficiency (Control)",
                "Meta-C (Depth)","Meta-C (Control)",
                "C (Depth)","C (Control)",
                "d (Depth)","d (Control)")

cl <- wesanderson::wes_palette("Cavalcanti1")

g <- ggplot(sdt_raw, aes(x = index, y = val, color = index)) +
  geom_jitter(size = 3, alpha = 0.3, width = 0.2) +
  geom_point(data = sdt_raw_means, aes(x=index,y=mean,fill = index), size = 5, colour = "grey30", shape = 21) +
  geom_errorbar(data = sdt_raw_means, aes(x = index, y= NULL,ymin= conf_lwr, ymax = conf_upr), width=.2) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey30") +
  coord_flip() +
  scale_colour_manual(values = rep(c(cl[5],cl[1]),5) ) +
  scale_fill_manual(values = rep(c(cl[5],cl[1]),5) ) +
  scale_y_continuous("Value",breaks = seq(-6,6,by=2),limits = c(-6, 6),expand=c(0,0)) +
  scale_x_discrete(labels=metaLabels) +
  guides(colour = "none", fill = "none")
g

## ggplot SDT2 Diff [Figure 2c]----
metaLabels2 <- c("ln Eff (Control)",
                 "Efficiency (Control)",
                 "Meta-C",
                 "C",
                 "d")

cl <- wesanderson::wes_palette("Cavalcanti1")

g <- ggplot(sdt_rel, aes(x = index, y = val, color = index)) +
  geom_jitter(size = 3, alpha = 0.3, width = 0.2) +
  geom_point(data = sdt_rel_means, aes(x=index,y=mean,fill = index), size = 5, colour = "grey30", shape = 21) +
  geom_errorbar(data = sdt_rel_means, aes(x = index, y= NULL,ymin= conf_lwr, ymax = conf_upr), width=.2) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey30") +
  coord_flip() +
  scale_colour_manual(values = rep(c(cl[2]),5) ) +
  scale_fill_manual(values = rep(c(cl[2]),5) ) +
  scale_y_continuous("Value",breaks = seq(-6,6,by=2),limits = c(-6, 6),expand=c(0,0)) +
  scale_x_discrete(labels=metaLabels2) +
  guides(colour = "none", fill = "none")
g


## ggscatter  plots (relative) [Figure 3a,b] ----
scaleFUN <- function(x) sprintf("%.1f", x)
p <- ggpubr::ggscatter(ponzometa, x = "ilsup", y = "d_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       ylim = c(-5.8, 0),
                       #ellipse = TRUE,
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) + 
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = 0.0, label.y = 0, r.digits = 7)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

cor_spearman.test(ponzometa$ilsup,ponzometa$d_rel)


p <- ggpubr::ggscatter(ponzometa, x = "ilsup", y = "ln_mceff_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       ylim = c(-0.6, 0.3),
                       #ellipse = TRUE,
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -0.2, label.y = 0.3, r.digits = 7)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

cor_spearman.test(ponzometa$ilsup,ponzometa$ln_mceff_rel)


p <- ggpubr::ggscatter(ponzometa, x = "d_rel", y = "ln_mceff_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       ylim = c(-0.6, 0.3),
                       #ellipse = TRUE,
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -4, label.y = 0.1, r.digits = 7)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

cor_spearman.test(ponzometa$d_rel,ponzometa$ln_mceff_rel)

# remove 2 outliers in log M-ratio
cor_spearman.test(ponzometa$d_rel[abs(ponzometa$ln_mceff_rel)< 0.2],ponzometa$ln_mceff_rel[abs(ponzometa$ln_mceff_rel)< 0.2])

