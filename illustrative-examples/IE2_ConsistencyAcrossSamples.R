# Load libraries
library(Rnalytica)
library(FSelector)

# Load Eclipse Platform 2 defect dataset
Data <- loadDefectDataset('eclipse-2.0')
dataset <- Data$data
indep <- Data$indep
dep <- Data$dep

# Generate 2 bootstrap training samples
set.seed(1)
indices.1 <- sample(nrow(dataset), replace = T)
set.seed(2)
indices.2 <- sample(nrow(dataset), replace = T)

# Apply CFS on 2 generated bootstrap training samples - Metrics are sorted in alphabetical order to make it easier to compare
cfs.metric.subset.1 <- sort(cfs(as.formula(paste(dep, '~', paste0(
  indep, collapse = '+'
))), dataset[indices.1, ]))
cfs.metric.subset.2 <- sort(cfs(as.formula(paste(dep, '~', paste0(
  indep, collapse = '+'
))), dataset[indices.2, ]))



