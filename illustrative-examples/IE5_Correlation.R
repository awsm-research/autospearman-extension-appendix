# Load libraries
library(Rnalytica)
library(FSelector)

# Load Eclipse Platform 2 defect dataset
Data <- loadDefectDataset('eclipse-2.0')
dataset <- Data$data
indep <- Data$indep
dep <- Data$dep

# Generate a bootstrap training sample
set.seed(1)
indices <- sample(nrow(dataset), replace = T)

# Apply CFS on a generated bootstrap training sample - Metrics are sorted in alphabetical order to make it easier to compare
cfs.metric.subset <- sort(cfs(as.formula(paste(dep, '~', paste0(
  indep, collapse = '+'
))), dataset[indices, ]))

# Apply findCorrelation on a generated bootstrap training sample - Metrics are sorted in alphabetical order to make it easier to compare
get.find.correlation <- function(dataset, indep){
  correlation.matrix = rcorr(as.matrix(dataset[, indep]), type = 'spearman')$r
  correlated.metrics = findCorrelation(correlation.matrix, cutoff = 0.7, exact = T)
  return(indep[-correlated.metrics])
}
findCorrelation.metric.subset <- sort(get.find.correlation(dataset[indices, ], indep))

# Apply AutoSpearman on a generated bootstrap training sample - Metrics are sorted in alphabetical order to make it easier to compare
AutoSpearman.part1.metric.subset <- sort(get.automated.spearman(dataset[indices,], indep, spearman.threshold = 0.7))
AutoSpearman.metric.subset <- sort(AutoSpearman(dataset[indices,], indep, spearman.threshold = 0.7))

# A hierarchical clustering view of the correlation analysis of all metrics in the Eclipse Platform 2 dataset
pdf('IE2_All_VarClusPlot.pdf',
    width=5,height=8,paper='special') 
plotVarClus(dataset[indices,], indep)
dev.off()

# Define function
get.correlation.plot <- function(pairwise.correlation, correlation.threshold = 0.7){
  plot.data <- pairwise.correlation
  plot.data$metrics <- names(plot.data)
  plot.data <- melt(plot.data)
  plot.data$value[plot.data$value == 1] <- NA
  plot.data$correlated <- ifelse(abs(plot.data$value) >= correlation.threshold, 1, 0)
  plot.data$correlated[is.na(plot.data$value)] <- NA
  
  ggplot(plot.data, aes(metrics, variable, fill=factor(correlated, levels = 0:1))) +
    geom_tile(height=0.8, width=0.8) +
    # scale_fill_gradient2(low="blue", mid="white", high="red") +
    scale_fill_manual(values = rev(c( '#fc8d59',
                                      '#99d594')), breaks = c(0, 1), labels = c('Non-correlated metrics', 'Correlated metrics')) +
    theme_minimal() +
    coord_equal() +
    geom_text(aes_string("metrics", "variable", label = round(plot.data$value, 2))) +
    labs(x="",y="",fill="") +
    theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                   margin=ggplot2::margin(t = -3)),
          axis.text.y=element_text(size=13, margin= ggplot2::margin(r = -3)),
          legend.text = element_text(size = 13),
          legend.position = "top",
          panel.grid.major=element_blank()) 
}
get.correlation.plot <- function(dataset, metrics, correlation.threshold = 0.7){
  pairwise.correlation <- as.data.frame(rcorr(as.matrix(dataset[, metrics]), type = 'spearman')$r)
  plot.data <- pairwise.correlation
  plot.data$metrics <- names(plot.data)
  plot.data <- melt(plot.data)
  plot.data$value[plot.data$value == 1] <- NA
  plot.data$correlated <- ifelse(abs(plot.data$value) >= correlation.threshold, 1, 0)
  plot.data$correlated[is.na(plot.data$value)] <- NA
  
  ggplot(plot.data, aes(metrics, variable, fill=factor(correlated, levels = 0:1))) +
    geom_tile(height=0.8, width=0.8) +
    # scale_fill_gradient2(low="blue", mid="white", high="red") +
    scale_fill_manual(values = rev(c( '#fc8d59',
                                      '#99d594')), breaks = c(0, 1), labels = c('Non-correlated metrics', 'Correlated metrics')) +
    theme_minimal() +
    coord_equal() +
    geom_text(aes_string("metrics", "variable", label = round(plot.data$value, 2))) +
    labs(x="",y="",fill="") +
    theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                   margin=ggplot2::margin(t = -3)),
          axis.text.y=element_text(size=13, margin= ggplot2::margin(r = -3)),
          legend.text = element_text(size = 13),
          legend.position = "top",
          panel.grid.major=element_blank()) 
}
get.vif.scores <- function(dataset, indep){
  dataset$dummy <- rnorm(nrow(dataset))
  vif.scores <- sort(vif(lm(as.formula(paste0("dummy~", paste0(indep, 
                                                                   collapse = "+"))), data = dataset)), decreasing = T)
  return(vif.scores)
}

studied.metric.subsets <- list(indep, cfs.metric.subset, findCorrelation.metric.subset, AutoSpearman.part1.metric.subset, AutoSpearman.metric.subset)
studied.metric.labels <- c('ALL', 'CFS', 'findCorrelation', 'ASPart1', 'AutoSpearman')
all.vif.scores <- list()
for(i in seq_along(studied.metric.labels)){
  
  # Step 1 - Analyse collinearity (Spearman rank correlation test)
  get.correlation.plot(dataset[indices,], studied.metric.subsets[[i]])
  ggsave(
    filename = paste0('IE2_', studied.metric.labels[i],'_CorrelationPlot.pdf'),
    width = 6,
    height = 6
  )

  # Step 2 - Analyse multicollinearity (Variance Inflation Factor analysis)
  all.vif.scores[[i]] <- list(metrics = studied.metric.labels[i], vif.scores = t(get.vif.scores(dataset[indices,], studied.metric.subsets[[i]])))

}

# Export latex table 
export.table.2 <- data.frame(cbind(names(vif.scores), vif.scores))
names(export.table.2) <- c('Software Metric', 'VIF score')
export.table.2$`Software Metric` <- paste0('\texttt{', as.character(export.table.2$`Software Metric`), '}')
export.table.2$`VIF score` <- sapply(as.numeric(levels(export.table.2$`VIF score`)[export.table.2$`VIF score`]), function(x) as.character(format(round(x, 2), nsmall = 2)))
row.names(export.table.2) <- NULL
latex(export.table.2, file = "", rowname = NULL)

