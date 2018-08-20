raw.data <- read.csv("creditcard.csv")
#calculate ROC (https://en.wikipedia.org/wiki/Receiver_operating_characteristic)
calculate_roc <- function(verset, cost_of_fp, cost_of_fn, n=100) {
  
  tp <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 1)
  }
  
  fp <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0)
  }
  
  tn <- function(verset, threshold) {
    sum(verset$predicted < threshold & verset$Class == 0)
  }
  
  fn <- function(verset, threshold) {
    sum(verset$predicted < threshold & verset$Class == 1)
  }
  
  tpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 1) / sum(verset$Class == 1)
  }
  
  fpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)
  }
  
  cost <- function(verset, threshold, cost_of_fp, cost_of_fn) {
    sum(verset$predicted >= threshold & verset$Class == 0) * cost_of_fp + 
      sum(verset$predicted < threshold & verset$Class == 1) * cost_of_fn
  }
  fpr <- function(verset, threshold) {
    sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)
  }
  
  threshold_round <- function(value, threshold)
  {
    return (as.integer(!(value < threshold)))
  }
  
  ans <- 0

  #calculate AUC (https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve)
  auc_ <- function(verset, threshold) {
    auc(verset$Class, threshold_round(verset$predicted,threshold))
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tp <- sapply(roc$threshold, function(th) tp(verset, th))
  roc$fp <- sapply(roc$threshold, function(th) fp(verset, th))
  roc$tn <- sapply(roc$threshold, function(th) tn(verset, th))
  roc$fn <- sapply(roc$threshold, function(th) fn(verset, th))
  roc$tpr <- sapply(roc$threshold, function(th) tpr(verset, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(verset, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(verset, th, cost_of_fp, cost_of_fn))
  roc$auc <-  sapply(roc$threshold, function(th) auc_(verset, th))
  
  return(roc)
}

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_auc <- ggplot(roc, aes(threshold, auc)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("AUC")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  # 
  grid.arrange(p_roc, p_auc, p_cost, ncol=2,sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}

plot_confusion_matrix <- function(verset, sSubtitle) {
  tst <- data.frame(round(verset$predicted,0), verset$Class)
  opts <-  c("Predicted", "True")
  names(tst) <- opts
  cf <- plyr::count(tst)
  cf[opts][cf[opts]==0] <- "Not Fraud"
  cf[opts][cf[opts]==1] <- "Fraud"
  
  ggplot(data =  cf, mapping = aes(x = True, y = Predicted)) +
    labs(title = "Confusion matrix", subtitle = sSubtitle) +
    geom_tile(aes(fill = freq), colour = "grey") +
    geom_text(aes(label = sprintf("%1.0f", freq)), vjust = 1) +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    theme_bw() + theme(legend.position = "none")
  
}

correlations <- cor(raw.data,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")

nrows <- nrow(raw.data)
set.seed(314)
indexT <- sample(1:nrow(raw.data), 0.7 * nrows)

#separate train and validation set
trainset = raw.data[indexT,]
verset =   raw.data[-indexT,]

n <- names(trainset)
rf.form <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

trainset.rf <- randomForest(rf.form,trainset,ntree=100,importance=T)

varimp <- data.frame(trainset.rf$importance)

vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

vi2 <- ggplot(varimp, aes(x=reorder(rownames(varimp),X.IncMSE), y=X.IncMSE)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (%IncMSE)", x="Variable", y="Variable importance (%IncMSE)")

grid.arrange(vi1, vi2, ncol=2)

verset$predicted <- predict(trainset.rf ,verset)

plot_confusion_matrix(verset, "Random Forest with 100 trees")
ans <- 0
roc <- calculate_roc(verset, 1, 10, n = 100)

mincost <- min(roc$cost)
roc %>%
  mutate(
    auc = ifelse(cost == mincost,
                 cell_spec(sprintf("%.5f", auc), "html", color = "green", background = "lightblue", bold = T),
                 cell_spec(sprintf("%.5f", auc), "html", color = "black", bold = F))
  ) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
  scroll_box(height = "600px")

threshold = 0.3
plot_roc(roc, threshold, 1, 10)
