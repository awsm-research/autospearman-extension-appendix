# Load libraries
library(Rnalytica)
library(caret)

# Load Eclipse Platform 2 defect dataset
Data <- loadDefectDataset('eclipse-2.0')
dataset <- Data$data
indep <- Data$indep
dep <- Data$dep
depFactor <- factor(dataset[, dep], levels = c(T, F))
dataset <- cbind(dataset, depFactor)

# Generate a bootstrap training sample
set.seed(1)
indices <- sample(nrow(dataset), replace = T)

# Apply Recursive Feature Elimination (Logistic Regression) with 2 different random seeds, stimulating two uncontrolled experiments - Metrics are sorted in alphabetical order to make it easier to compare
# Recursive Feature Elimination for Logistic Regression (RFE-LR)
lrFuncs_AUC <- lrFuncs
lrFuncs_AUC$summary <- twoClassSummary
control <-
  rfeControl(
    functions = lrFuncs_AUC,
    method = "boot",
    number = 100,
    verbose = T
  )

set.seed(1)
results.lr.1 <-
  rfe(
    dataset[indices, indep],
    dataset[indices, 'depFactor'],
    sizes = c(1:length(indep)),
    rfeControl = control,
    metric = 'ROC'
  )
rfe.lr.metric.subset.1 <- sort(predictors(results.lr.1))

set.seed(2)
results.lr.2 <-
  rfe(
    dataset[indices, indep],
    dataset[indices, 'depFactor'],
    sizes = c(1:length(indep)),
    rfeControl = control,
    metric = 'ROC'
  )
rfe.lr.metric.subset.2 <- sort(predictors(results.lr.2))

# Apply Recursive Feature Elimination (Random Forest) with 2 different random seeds, stimulating two uncontrolled experiments - Metrics are sorted in alphabetical order to make it easier to compare
# Recursive Feature Elimination for Random Forest (RFE-RF)
rfFuncs_AUC <- rfFuncs
rfFuncs_AUC$summary <- twoClassSummary
control <-
  rfeControl(
    functions = rfFuncs_AUC,
    method = "boot",
    number = 100,
    verbose = T
  )

set.seed(1)
results.rf.1 <-
  rfe(
    dataset[indices, indep],
    dataset[indices, 'depFactor'],
    sizes = c(1:length(indep)),
    rfeControl = control,
    metric = 'ROC'
  )
rfe.rf.metric.subset.1 <- sort(predictors(results.rf.1))

set.seed(2)
results.rf.2 <-
  rfe(
    dataset[indices, indep],
    dataset[indices, 'depFactor'],
    sizes = c(1:length(indep)),
    rfeControl = control,
    metric = 'ROC'
  )
rfe.rf.metric.subset.2 <- sort(predictors(results.rf.2))


saveRDS(list(rfe.lr.1 = rfe.lr.metric.subset.1, rfe.lr.2 = rfe.lr.metric.subset.2, rfe.rf.1 = rfe.rf.metric.subset.1, rfe.rf.2 = rfe.rf.metric.subset.2), 'IE3_ConsistencyAcrossRandomSeeds.rds')

# Analysis ####
all.metric.subsets <- readRDS('IE3_ConsistencyAcrossRandomSeeds.rds')

# Consistency across random seeds
consistent.metrics <- intersect(all.metric.subsets$rfe.lr.1, all.metric.subsets$rfe.lr.2)

# Latex export
paste0(paste0(ifelse(all.metric.subsets$rfe.lr.1 %in% consistent.metrics, '\\green{', '\\red{'), sort(all.metric.subsets$rfe.lr.1), '}'), collapse=', ')
paste0(paste0(ifelse(all.metric.subsets$rfe.lr.2 %in% consistent.metrics, '\\green{', '\\red{'), sort(all.metric.subsets$rfe.lr.2), '}'), collapse=', ')