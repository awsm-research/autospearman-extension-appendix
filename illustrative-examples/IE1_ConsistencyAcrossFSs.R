# Load libraries
library(Rnalytica)
library(FSelector)
library(caret)

# Load Eclipse Platform 2 defect dataset
Data <- loadDefectDataset('eclipse-2.0')
dataset <- Data$data
indep <- Data$indep
dep <- Data$dep
depFactor <- factor(dataset[, dep], levels = c(T, F))
dataset <- cbind(dataset, depFactor)

# Generate a bootstrap training samples
set.seed(1)
indices <- sample(nrow(dataset), replace = T)

# Preparation ####
# Apply all studied feature selection techniques on the same generated bootstrap training sample - Metrics are sorted in alphabetical order to make it easier to compare

# Correlation-based feature selection technique (CFS)
cfs.metric.subset <- sort(cfs(as.formula(paste(dep, '~', paste0(
  indep, collapse = '+'
))), dataset[indices, ]))

# Information Gain feature selection technique (IG)
ig.weights <- information.gain(as.formula(paste(dep, '~', paste0(indep, collapse = '+'))), dataset[indices, ])
ig.metric.subset <- sort(cutoff.k(ig.weights, log2(length(indep))))

# Chi-squared-based feature selection technique (Chisq)
chisq.weights <- chi.squared(as.formula(paste(dep, '~', paste0(indep, collapse = '+'))), dataset[indices, ])
chisq.metric.subset <- sort(cutoff.k(chisq.weights, log2(length(indep))))

# Consistency-based feature selection technique (CON)
con.metric.subset <-
  sort(consistency(as.formula(paste(dep, '~', paste0(indep, collapse = '+'))), dataset[indices, ]))

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
results <-
  rfe(
    dataset[indices, indep],
    dataset[indices, 'depFactor'],
    sizes = c(1:length(indep)),
    rfeControl = control,
    metric = 'ROC'
  )
rfe.lr.metric.subset <- sort(predictors(results))

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
# run the RFE algorithm
set.seed(1)
results <-
  rfe(
    dataset[indices, indep],
    dataset[indices, 'depFactor'],
    sizes = c(1:length(indep)),
    rfeControl = control,
    metric = 'ROC'
  )
rfe.rf.metric.subset <- sort(predictors(results))

# Stepwise Regression
# Define null and full models prior performing Stepwise Regression
# While the null model is simply y~1, the full model is y~all metrics.
null.model <- glm(
  formula = as.formula(paste0(dep,
                              '~1')),
  data = dataset[indices, ],
  family = binomial()
)
full.model <- glm(
  formula = as.formula(paste0(dep,
                              '~',
                              paste0(indep, collapse = '+'))),
  data = dataset[indices, ],
  family = binomial()
)


# Forward Direction
output.fwd <-
  step(
    null.model,
    scope = list(lower = null.model, upper = full.model),
    direction = 'forward',
    trace = T
  )

# Backward Direction
output.bwd <-
  step(full.model,
       data = dataset[indices,c(indep, dep)],
       direction = 'backward',
       trace = F)

# Both Directions
output.both <-
  step(
    null.model,
    scope = list(upper = full.model),
    data = dataset[indices, c(indep, dep)],
    direction = "both",
    trace = T
  )

step.fwd.metric.subset <- sort(names(output.fwd$coefficients)[-1])
step.bwd.metric.subset <- sort(names(output.bwd$coefficients)[-1])
step.both.metric.subset <- sort(names(output.both$coefficients)[-1])

# findCorrelation
correlation.matrix <- rcorr(as.matrix(dataset[indices, indep]), type = 'spearman')$r
correlated.metrics <- findCorrelation(correlation.matrix, cutoff = 0.7, exact = T)
findCorrelation.metric.subset <- sort(indep[-correlated.metrics])

# AutoSpearman
AutoSpearman.metrics.subset <- sort(AutoSpearman(dataset[indices,], indep))

# Summary - All FSs
all.metric.subsets <- list(CFS=cfs.metric.subset, IG=ig.metric.subset, Chisq = chisq.metric.subset, CON = con.metric.subset, RFE.LR = rfe.lr.metric.subset, RFE.RF = rfe.rf.metric.subset, STEP.FWD = step.fwd.metric.subset, STEP.BWD = step.bwd.metric.subset, STEP.BOTH = step.both.metric.subset, findCorrelation = findCorrelation.metric.subset, AutoSpearman = AutoSpearman.metrics.subset)

# Export
saveRDS(all.metric.subsets, 'IE1_output.rds')

# Analysis ####

# Consistency across FSs
consistent.metrics <- Reduce(intersect, all.metric.subsets)
all.metrics <- Reduce(union, all.metric.subsets)

# Latex export
export.table <- do.call(rbind, lapply(seq_along(all.metric.subsets), function(x, y) { data.frame(metrics = paste0(sort(sapply(x[[y]], function(z) paste0('\texttt{', z, '}'))), collapse = ', '))}, x = all.metric.subsets))
row.names(export.table) <- names(all.metric.subsets)
latex(export.table, file = "")

# Flipped latex
all.metrics$findCorrelation <- findCorrelation.metric.subset
all.metrics$AutoSpearman <- AutoSpearman.metrics.subset
sorted.indep <- sort(indep)
all.metrics <- all.metrics[c(1:4, 10, 5:9, 11)]
export.df <- {}
for(i in seq_along(all.metrics)){
  export.df <- rbind(export.df, data.frame(t(sorted.indep %in% all.metrics[[i]])))
}
names(export.df) <- sapply(sorted.indep, function(x) paste0('\\texttt{', x, '}'))
row.names(export.df) <- names(all.metrics)
transposed.df <- data.table::transpose(export.df)
row.names(transposed.df) <- names(export.df)
names(transposed.df) <- row.names(export.df)
latex(export.df, file = "")
latex(transposed.df, file = '')
