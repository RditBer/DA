###############################
###   Lab 5: SVM Classification
###   Wine Dataset
###############################

library(caret)
library(e1071)
library(ggplot2)
library(GGally)

# ── Load Wine Data ──────────────────────────────────────────────────────────
# The wine dataset has no header; 14 columns:
#   col 1  = class label (1, 2, 3)
#   cols 2-14 = 13 chemical features
wine_colnames <- c("Class",
                   "Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh",
                   "Magnesium", "TotalPhenols", "Flavanoids",
                   "NonflavanoidPhenols", "Proanthocyanins",
                   "ColorIntensity", "Hue", "OD280_OD315", "Proline")

wine <- read.csv("C:/Users/ardit/Downloads/wine_(2).data",
                 header = FALSE, col.names = wine_colnames)

wine$Class <- as.factor(wine$Class)

cat("Dataset dimensions:", nrow(wine), "rows x", ncol(wine), "cols\n")
cat("Class distribution:\n")
print(table(wine$Class))

# ── Feature Selection ────────────────────────────────────────────────────────
# Based on the wine literature and typical EDA, Flavanoids, Proline,
# ColorIntensity, Alcohol, and OD280_OD315 are the strongest discriminators.
# We use a subset of 5 features.
features <- c("Flavanoids", "Proline", "ColorIntensity",
              "Alcohol", "OD280_OD315")

# Quick pair-plot of selected features
ggpairs(wine[, c(features, "Class")],
        ggplot2::aes(colour = Class),
        title = "Wine Dataset – Selected Features") 

# ── Train / Test Split (70 / 30) ────────────────────────────────────────────
set.seed(42)
N            <- nrow(wine)
train.idx    <- sample(N, 0.7 * N)
train        <- wine[train.idx, ]
test         <- wine[-train.idx, ]

cat("\nTrain size:", nrow(train), "| Test size:", nrow(test), "\n")

# ── Helper: compute per-class precision / recall / F1 from predictions ───────
compute_metrics <- function(actual, predicted, model_label) {
  cm       <- as.matrix(table(Actual = actual, Predicted = predicted))
  n        <- sum(cm)
  diagv    <- diag(cm)
  rowsums  <- apply(cm, 1, sum)
  colsums  <- apply(cm, 2, sum)
  
  accuracy  <- sum(diagv) / n
  recall    <- diagv / rowsums
  precision <- diagv / colsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  cat("\n──────────────────────────────\n")
  cat("Model:", model_label, "\n")
  cat("Confusion Matrix:\n"); print(cm)
  cat("Accuracy:", round(accuracy, 4), "\n")
  
  res <- data.frame(model = model_label, precision, recall, f1)
  print(res)
  return(res)
}

# formula for all 5 features
wine_formula <- as.formula(
  paste("Class ~", paste(features, collapse = " + "))
)

# ═══════════════════════════════════════════════════════════════════════════
#  MODEL 1 – SVM with LINEAR kernel  (tune C)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n=== Tuning Linear SVM ===\n")
tune.linear <- tune.svm(wine_formula,
                        data   = train,
                        kernel = "linear",
                        cost   = c(0.01, 0.1, 1, 5, 10, 50))
cat("Best parameters (linear):\n"); print(tune.linear$best.parameters)

svm.linear <- svm(wine_formula,
                  data   = train,
                  kernel = "linear",
                  cost   = tune.linear$best.parameters$cost)

pred.linear <- predict(svm.linear, test)
res.linear  <- compute_metrics(test$Class, pred.linear, "Linear SVM")

# ═══════════════════════════════════════════════════════════════════════════
#  MODEL 2 – SVM with RBF (Radial) kernel  (tune C & gamma)
# ═══════════════════════════════════════════════════════════════════════════
cat("\n=== Tuning Radial (RBF) SVM ===\n")
tune.radial <- tune.svm(wine_formula,
                        data  = train,
                        kernel = "radial",
                        cost  = c(0.1, 1, 5, 10, 50),
                        gamma = c(0.001, 0.01, 0.1, 0.5, 1))
cat("Best parameters (radial):\n"); print(tune.radial$best.parameters)

svm.radial <- svm(wine_formula,
                  data   = train,
                  kernel = "radial",
                  cost   = tune.radial$best.parameters$cost,
                  gamma  = tune.radial$best.parameters$gamma)

pred.radial <- predict(svm.radial, test)
res.radial  <- compute_metrics(test$Class, pred.radial, "Radial SVM")

# ═══════════════════════════════════════════════════════════════════════════
#  MODEL 3 – Random Forest (alternative classifier, same features)
# ═══════════════════════════════════════════════════════════════════════════
library(randomForest)

set.seed(42)
rf.model <- randomForest(wine_formula,
                         data     = train,
                         ntree    = 500,
                         mtry     = 2,
                         importance = TRUE)

cat("\n=== Random Forest ===\n")
print(rf.model)

pred.rf <- predict(rf.model, test)
res.rf  <- compute_metrics(test$Class, pred.rf, "Random Forest")

# Variable importance plot
varImpPlot(rf.model, main = "Random Forest – Variable Importance")

# ═══════════════════════════════════════════════════════════════════════════
#  COMPARISON TABLE
# ═══════════════════════════════════════════════════════════════════════════
all_results <- rbind(res.linear, res.radial, res.rf)

cat("\n\n╔══════════════════════════════════════════════════════╗\n")
cat("║        FULL MODEL COMPARISON (per class)             ║\n")
cat("╚══════════════════════════════════════════════════════╝\n")
print(all_results, digits = 4)

# Macro-average summary
macro <- aggregate(cbind(precision, recall, f1) ~ model, data = all_results, mean)
cat("\n── Macro-Average Summary ──\n")
print(macro, digits = 4)

# ── Bar-plot: macro F1 by model ──────────────────────────────────────────────
ggplot(macro, aes(x = model, y = f1, fill = model)) +
  geom_bar(stat = "identity", width = 0.5, colour = "black") +
  geom_text(aes(label = round(f1, 3)), vjust = -0.4, size = 4.5) +
  ylim(0, 1.05) +
  labs(title = "Macro-Average F1 Score by Model",
       x = "Model", y = "Macro F1") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
