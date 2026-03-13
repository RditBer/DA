##########################################
### Principal Component Analysis (PCA) ###
##########################################

# ── Libraries ──────────────────────────────────────────────────────────────────
library(ggplot2)
library(ggfortify)
library(class)
library(readr)

# ── Load & prepare data ────────────────────────────────────────────────────────
wine <- read_csv("wine.data", col_names = FALSE)

col_names <- c(
  "Type", "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash",
  "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_Phenols",
  "Proanthocyanins", "Color_Intensity", "Hue", "Od280_Od315", "Proline"
)
names(wine) <- col_names

wine$Type <- as.factor(wine$Type)

X <- wine[, -1]
Y <- wine$Type

# ── Step 1: PCA ────────────────────────────────────────────────────────────────
pca_model <- prcomp(X, scale. = TRUE)
summary(pca_model)

# ── Step 2: Plot PC1 vs PC2 ────────────────────────────────────────────────────
autoplot(pca_model, data = wine, colour = "Type") +
  scale_color_manual(values = c("1" = "#CD2626", "2" = "#BCEE68", "3" = "darkorchid2")) +
  labs(
    title = "PCA – Wine Dataset",
    x     = paste0("PC1 (", round(summary(pca_model)$importance[2, 1] * 100, 1), "%)"),
    y     = paste0("PC2 (", round(summary(pca_model)$importance[2, 2] * 100, 1), "%)")
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.title = element_text(face = "bold"),
    axis.title   = element_text(face = "bold")
  )

# ── Step 3: Top variables contributing to PC1 ─────────────────────────────────
pc1_loadings <- sort(abs(pca_model$rotation[, 1]), decreasing = TRUE)
cat("PC1 variable contributions (|loading|):\n")
print(round(pc1_loadings, 3))

top_vars <- names(pc1_loadings)[1:4]
cat("\nTop 4 variables:", paste(top_vars, collapse = ", "), "\n")

# ── Helper: compute classification metrics from a confusion matrix ─────────────
get_metrics <- function(conf_mat) {
  classes <- rownames(conf_mat)
  
  per_class <- lapply(seq_along(classes), function(i) {
    TP <- conf_mat[i, i]
    FP <- sum(conf_mat[i, ]) - TP
    FN <- sum(conf_mat[, i]) - TP
    
    precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
    recall    <- if ((TP + FN) == 0) NA else TP / (TP + FN)
    f1        <- if (is.na(precision) | is.na(recall) | (precision + recall) == 0) NA
    else 2 * precision * recall / (precision + recall)
    
    c(Precision = precision, Recall = recall, F1 = f1)
  })
  
  metrics_df <- as.data.frame(do.call(rbind, per_class))
  metrics_df <- round(metrics_df, 3)
  metrics_df <- cbind(Class = classes, metrics_df)
  
  list(
    Accuracy         = round(sum(diag(conf_mat)) / sum(conf_mat), 3),
    Metrics_by_Class = metrics_df,
    Macro_Precision  = round(mean(metrics_df$Precision, na.rm = TRUE), 3),
    Macro_Recall     = round(mean(metrics_df$Recall,    na.rm = TRUE), 3),
    Macro_F1         = round(mean(metrics_df$F1,        na.rm = TRUE), 3)
  )
}

# ── Train / test split (shared across both models) ────────────────────────────
set.seed(123)
train_idx <- sample(seq_len(nrow(wine)), size = floor(0.7 * nrow(wine)))

# ── Step 4: kNN on top 4 original variables ───────────────────────────────────
wine_subset  <- wine[, top_vars]

train_X_raw  <- wine_subset[ train_idx, ]
test_X_raw   <- wine_subset[-train_idx, ]

# Scale using training statistics only (avoids data leakage)
train_scaled <- scale(train_X_raw)
test_scaled  <- scale(test_X_raw,
                      center = attr(train_scaled, "scaled:center"),
                      scale  = attr(train_scaled, "scaled:scale"))

pred_knn_orig <- knn(train = train_scaled,
                     test  = test_scaled,
                     cl    = Y[train_idx],
                     k     = 5)

confusion_orig <- table(Predicted = pred_knn_orig, Actual = Y[-train_idx])

# ── Step 5: kNN on first 2 PC scores ──────────────────────────────────────────
# PC scores from prcomp are already zero-centred & unit-variance — no extra scaling needed
pc_scores <- as.data.frame(pca_model$x[, 1:2])

pred_knn_pca <- knn(train = pc_scores[ train_idx, ],
                    test  = pc_scores[-train_idx, ],
                    cl    = Y[train_idx],
                    k     = 5)

confusion_pca <- table(Predicted = pred_knn_pca, Actual = Y[-train_idx])

# ── Step 6: Print results ──────────────────────────────────────────────────────
print_results <- function(label, conf_mat, metrics) {
  cat(strrep("=", 45), "\n")
  cat(label, "\n")
  cat(strrep("=", 45), "\n")
  print(conf_mat)
  cat("\nAccuracy:", metrics$Accuracy, "\n\n")
  cat("Per-class metrics:\n")
  print(metrics$Metrics_by_Class, row.names = FALSE)
  cat("\nMacro Precision:", metrics$Macro_Precision,
      " | Macro Recall:", metrics$Macro_Recall,
      " | Macro F1:", metrics$Macro_F1, "\n\n")
}

print_results("Model 1 – kNN on top 4 original variables",
              confusion_orig, get_metrics(confusion_orig))

print_results("Model 2 – kNN on first 2 PC scores",
              confusion_pca, get_metrics(confusion_pca))
