# Load libraries
library(Rnalytica)

# Load Eclipse PDE dataset
Data <- loadDefectDataset('pde')
Data <- loadDefectDataset('eclipse34_swt')
dataset <- Data$data
indep <- Data$indep
dep <- Data$dep

set.seed(1)
indices <- sample(nrow(dataset), replace = T)

# Define Stepwise Regression function
step.fwd <- function(dataset, indep, dep){
  
  # Define null and full models prior performing Stepwise Regression
  # While the null model is simply y~1, the full model is y~all metrics.
  null.model <- glm(
    formula = as.formula(paste0(dep,
                                '~1')),
    data = dataset,
    family = binomial()
  )
  full.model <- glm(
    formula = as.formula(paste0(dep,
                                '~',
                                paste0(indep, collapse = '+'))),
    data = dataset,
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
  
  step.fwd.metric.subset <- sort(names(output.fwd$coefficients)[-1])
  return(step.fwd.metric.subset)
}

# Apply Stepwise Regression with 2 different model specifications - Metrics are sorted in alphabetical order to make it easier to compare
model.specification.1 <- indep
set.seed(2)
model.specification.2 <- sample(indep)

step.fwd.metric.subset.1 <- step.fwd(dataset[indices,], model.specification.1, dep)
step.fwd.metric.subset.2 <- step.fwd(dataset[indices,], model.specification.2, dep)

saveRDS(list(step.fwd.1 = step.fwd.metric.subset.1, step.fwd.2 = step.fwd.metric.subset.2), 'IE4_ConsistencyAcrossModelSpecifications.rds')

# Export latex
t <- sapply(step.fwd.metric.subset.1, function(x) paste0('\\green{', x, '}'))
names(t) <- NA
t <- paste0(as.character(t), collapse= ', ')
t <- sapply(step.fwd.metric.subset.2, function(x) paste0('\\green{', x, '}'))
names(t) <- NA
t <- paste0(as.character(t), collapse= ', ')
