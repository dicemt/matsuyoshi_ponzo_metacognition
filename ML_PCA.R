## LIBRARIES----------------------
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)

## Loading data variables from basic_stats.R

## Machine learning settings----
seed <- 1
# Setup a grid range of lambda values
lambda <- 10^seq(-3, 3, length = 100)

train_control <- trainControl(method="LOOCV")

# Metrics
MLmet <- "RMSE" # RMSE Rsquared MAE R 

# Load data
MLdp <- ilsup[5:ncol(ilsup)]
MLdp$behav <- ponzometa$d_rel
MLdp <- as.data.frame(scale(MLdp))

MLmr <- ilsup[5:ncol(ilsup)]
MLmr$behav <- ponzometa$ln_mceff_rel
MLmr <- as.data.frame(scale(MLmr))

## d prime: elastic net LOOCV ----
# Build the model
set.seed(seed)
elasticAll <- train(
  behav ~., data = MLdp, method = "glmnet",
  trControl = train_control, metric = MLmet,
  tuneGrid = expand.grid(alpha = 1:10 * 0.1, lambda = lambda)
  #  tuneLength = 10
)
# Model coefficients
elasticAll_coef <- coef(elasticAll$finalModel, elasticAll$bestTune$lambda)
elasticAll_coef[-1] %>% plot(main = "Coefficients (elasticAll net)", xlab="index", ylab = "value")
abline(h=0)
elasticAll_coef

elidx <- intersect(which(elasticAll$pred$alpha %in% elasticAll$bestTune$alpha),which(elasticAll$pred$lambda %in% elasticAll$bestTune$lambda))
cor_spearman.test(elasticAll$pred$pred[elidx],elasticAll$pred$obs[elidx])


# Make predictions on the test data
predictions <- elasticAll %>% predict(MLdp)
# Model performance metrics
preds <- data.frame(
  Label = "elasticAll", stringsAsFactors=F,
  RMSE = RMSE(predictions, MLdp$behav),
  Rsquare = R2(predictions, MLdp$behav),
  MAE = MAE(predictions, MLdp$behav),
  R = sqrt(R2(predictions, MLdp$behav)),
  ratio = RMSE(predictions, MLdp$behav)/MAE(predictions, MLdp$behav)
)
preds

## d prime: Relaxed elastic net LOOCV ----
# Build the model
set.seed(seed)
elasticAllR <- train(
  behav ~., data = MLdp[,c(elasticAll_coef@i[-1],ncol(MLdp))], method = "glmnet",lambda=elasticAll$bestTune$lambda,
  trControl = train_control, metric = MLmet,
  tuneGrid = expand.grid(alpha = 0, lambda = elasticAll$bestTune$lambda)
)
# Model coefficients
elasticAllR_coef <- coef(elasticAllR$finalModel, elasticAllR$bestTune$lambda)
elasticAllR_coef[-1] %>% plot(main = "Coefficients (Relaxed-elasticAll net)", xlab="index", ylab = "value")
abline(h=0)
elasticAllR_coef
dfAllRE <- data.frame(elasticAllR_coef@Dimnames[[1]][-1],elasticAllR_coef@x[-1])
colnames(dfAllRE) <- c("Item","coef")
dfAllRE$Item <- as.integer(gsub("X","",dfAllRE$Item))
words_en <- read.table("words_en.txt")
dfAllRE$Text <- words_en$V1[as.integer(dfAllRE$Item)]
write.table(dfAllRE,"Rexlaxedelasticnet_words_dprime2.txt",sep="\t") # Table 2

scaleFUN <- function(x) sprintf("%.1f", x)
p <- ggpubr::ggscatter(elasticAllR$pred, x = "obs", y = "pred",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-3.0, 3.0),
                       ylim = c(-3.0, 3.0),
                       xlab = "d_rel", ylab="Prediction",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -2.5, label.y = 2.5)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

print(elasticAllR)
sqrt(elasticAllR$results$Rsquared)
cor_spearman.test(elasticAllR$pred$pred,elasticAllR$pred$obs) # Fig4

# Make predictions on the test data
predictions <- elasticAllR %>% predict(MLdp)
# Model performance metrics
preds[nrow(preds)+1,] <- data.frame(
  Label = "elasticAllR", stringsAsFactors=F,
  RMSE = RMSE(predictions, MLdp$behav),
  Rsquare = R2(predictions, MLdp$behav),
  MAE = MAE(predictions, MLdp$behav),
  R = sqrt(R2(predictions, MLdp$behav)),
  ratio = RMSE(predictions, MLdp$behav)/MAE(predictions, MLdp$behav)
)
preds


## log Mratio: Elastic net LOOCV ----
# Build the model
set.seed(seed)
elasticAll_Mratio <- train(
  behav ~., data = MLmr, method = "glmnet",
  trControl = train_control, metric = MLmet,
  tuneGrid = expand.grid(alpha = 1:10 * 0.1, lambda = lambda)
  #  tuneLength = 10
)
# Model coefficients
elasticAll_Mratio_coef <- coef(elasticAll_Mratio$finalModel, elasticAll_Mratio$bestTune$lambda)
elasticAll_Mratio_coef[-1] %>% plot(main = "Coefficients (elasticAll_Mratio net)", xlab="index", ylab = "value")
abline(h=0)
elasticAll_Mratio_coef

elidx <- intersect(which(elasticAll_Mratio$pred$alpha %in% elasticAll_Mratio$bestTune$alpha),which(elasticAll_Mratio$pred$lambda %in% elasticAll_Mratio$bestTune$lambda))
cor.test(rank(elasticAll_Mratio$pred$pred[elidx]),rank(elasticAll_Mratio$pred$obs[elidx]))


# Make predictions on the test data
predictions <- elasticAll_Mratio %>% predict(MLmr)
# Model performance metrics
preds[nrow(preds)+1,] <- data.frame(
  Label = "elasticAll_Mratio", stringsAsFactors=F,
  RMSE = RMSE(predictions, MLmr$behav),
  Rsquare = R2(predictions, MLmr$behav),
  MAE = MAE(predictions, MLmr$behav),
  R = sqrt(R2(predictions, MLmr$behav)),
  ratio = RMSE(predictions, MLmr$behav)/MAE(predictions, MLmr$behav)
)
preds

## log Mratio: Relaxed elastic net LOOCV ----
# Build the model
set.seed(seed)
elasticAll_MratioR <- train(
  behav ~., data = MLmr[,c(elasticAll_Mratio_coef@i[-1],ncol(MLmr))], method = "glmnet",lambda=elasticAll_Mratio$bestTune$lambda,
  trControl = train_control, metric = MLmet,
  tuneGrid = expand.grid(alpha = 0, lambda = elasticAll_Mratio$bestTune$lambda)
)
# Model coefficients
elasticAll_MratioR_coef <- coef(elasticAll_MratioR$finalModel, elasticAll_MratioR$bestTune$lambda)
elasticAll_MratioR_coef[-1] %>% plot(main = "Coefficients (Relaxed-elasticAll_Mratio net)", xlab="index", ylab = "value")
abline(h=0)
elasticAll_MratioR_coef
dfAllMetaRE <- data.frame(elasticAll_MratioR_coef@Dimnames[[1]][-1],elasticAll_MratioR_coef@x[-1])
colnames(dfAllMetaRE) <- c("Item","coef")
dfAllMetaRE$Item <- as.integer(gsub("X","",dfAllMetaRE$Item))
dfAllMetaRE$Text <- words_en$V1[as.integer(dfAllMetaRE$Item)]
write.table(dfAllMetaRE,"Rexlaxedelasticnet_words_Mratio2.txt",sep="\t") # Table 1

print(elasticAll_MratioR)
sqrt(elasticAll_MratioR$results$Rsquared)
cor.test(rank(elasticAll_MratioR$pred$pred),rank(elasticAll_MratioR$pred$obs)) # Fig4
cor_spearman.test(elasticAll_MratioR$pred$pred,elasticAll_MratioR$pred$obs) # Fig4

# Outliers do not affect so much
cor_spearman.test(elasticAll_MratioR$pred$pred[abs(elasticAll_MratioR$pred$obs)<2],elasticAll_MratioR$pred$obs[abs(elasticAll_MratioR$pred$obs)<2])

p <- ggpubr::ggscatter(elasticAll_MratioR$pred, x = "obs", y = "pred",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-3.0, 3.0),
                       ylim = c(-3.0, 3.0),
                       xlab = "Mratio_rel", ylab="Prediction",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -2.5, label.y = 2.5, r.digits = 7)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

# Make predictions on the test data
predictions <- elasticAll_MratioR %>% predict(MLmr)
# Model performance metrics
preds[nrow(preds)+1,] <- data.frame(
  Label = "elasticAll_MratioR", stringsAsFactors=F,
  RMSE = RMSE(predictions, MLmr$behav),
  Rsquare = R2(predictions, MLmr$behav),
  MAE = MAE(predictions, MLmr$behav),
  R = sqrt(R2(predictions, MLmr$behav)),
  ratio = RMSE(predictions, MLmr$behav)/MAE(predictions, MLmr$behav)
)
preds

## PCA [Table 2] ----
library(readr)
library(psych)
pca <- prcomp(ilsup[,5:ncol(ilsup)])
summary(pca)
loadings <- sweep(pca$rotation,MARGIN=2,pca$sdev,FUN="*")
psych::fa.parallel(ilsup[,5:ncol(ilsup)],fm="minres",fa="pc")
readr::write_csv(as.data.frame(loadings),'pca_loadings.csv')
readr::write_csv(as.data.frame(pca$rotation), "pca_rotation.csv")

# Total Feature Importance [Figure 5a] ----
tmp_d1 <- as.numeric(rep("NA",52))
tmp_d1[dfAllRE$Item] <- dfAllRE$coef
d1_pca_res <- apply(tmp_d1*loadings,2,sum,na.rm = TRUE)

tmp_d2 <- as.numeric(rep("NA",52))
tmp_d2[dfAllMetaRE$Item] <- dfAllMetaRE$coef
d2_pca_res <- apply(tmp_d2*loadings,2,sum,na.rm = TRUE)

df_pc123 <- as.data.frame(rbind(d1_pca_res[1:3],d2_pca_res[1:3]))
df_pc123$id <- as.factor(c(1:2))
df_pc123 <- pivot_longer(df_pc123,cols = c(-id), names_to = "PC", values_to = "Loading")
ggpubr::ggbarplot(df_pc123, "PC", "Loading",
                  fill = "id", color = "id", palette = "Paired",
                  ylim = c(-0.25,0.1),
                  label = TRUE, lab.col = "black", lab.size = 4, lab.nb.digits = 3,
                  position = position_dodge(0.7)) +
  scale_y_continuous(labels=scaleFUN) +
  scale_colour_manual(values = c("#7F7F7F","#FF2800") ) +
  scale_fill_manual(values = c("#7F7F7F","#FF2800") )

## Correlation between PCs and SDT measures [Figure 5b] ----
cor_spearman.test(pca$x[,1],ponzometa$d_rel)
cor_spearman.test(pca$x[,2],ponzometa$d_rel)
cor_spearman.test(pca$x[,3],ponzometa$d_rel)
cor_spearman.test(pca$x[,1],ponzometa$ln_mceff_rel)
cor_spearman.test(pca$x[,2],ponzometa$ln_mceff_rel)
cor_spearman.test(pca$x[,3],ponzometa$ln_mceff_rel)

## PC1 dprime ggplot ----
pcabehavdf <- data.frame(dplyr::select(data.frame(pca$x),PC1:PC3),ponzometa$d_rel,ponzometa$ln_mceff_rel)
p <- ggpubr::ggscatter(pcabehavdf, x = "PC1", y = "ponzometa.d_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-3.2, 3.2),
                       ylim = c(-6.0, 0.0),
                       xlab = "PC1", ylab="dprime",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -2.5, label.y = -0.5)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")


## PC2 dprime ggplot ----
p <- ggpubr::ggscatter(pcabehavdf, x = "PC2", y = "ponzometa.d_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-2.15, 2.15),
                       ylim = c(-6.0, 0.0),
                       xlab = "PC2", ylab="dprime",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -2.0, label.y = -0.5)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

## PC3 dprime ggplot ----
p <- ggpubr::ggscatter(pcabehavdf, x = "PC3", y = "ponzometa.d_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-1.6, 3),
                       ylim = c(-6.0, 0.0),
                       xlab = "PC3", ylab="dprime",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -1.5, label.y = -0.5)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")

## PC1 ln_mceff ggplot ----
p <- ggpubr::ggscatter(pcabehavdf, x = "PC1", y = "ponzometa.ln_mceff_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-3.2, 3.2),
                       ylim = c(-0.6, 0.3),
                       xlab = "PC1", ylab="mceff",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -2.5, label.y = 0.2)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")


## PC2 ln_mceff ggplot ----
p <- ggpubr::ggscatter(pcabehavdf, x = "PC2", y = "ponzometa.ln_mceff_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-2.15, 2.15),
                       ylim = c(-0.6, 0.3),
                       xlab = "PC2", ylab="mceff",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -2.0, label.y = 0.2)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")


## PC3 ln_mceff ggplot ----
p <- ggpubr::ggscatter(pcabehavdf, x = "PC3", y = "ponzometa.ln_mceff_rel",
                       add = "reg.line",                                 # Add regression line
                       conf.int = TRUE,                                  # Add confidence interval
                       xlim = c(-1.6, 3),
                       ylim = c(-0.6, 0.3),
                       xlab = "PC3", ylab="mceff",
                       add.params = list(color = "blue",
                                         fill = "lightgray")
) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUN) +
  ggpubr::stat_cor(
    method = "spearman", label.x = -1.5, label.y = 0.2)  # Add correlation coefficient
ggExtra::ggMarginal(p, type = "density")
