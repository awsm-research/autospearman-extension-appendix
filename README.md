# The Impact of Automated Feature Selection Techniques on the Interpretation of Defect Models

### Abstract
---
> The interpretation of defect models heavily relies on software metrics that are used to construct them.
> Prior work often uses feature selection techniques to remove metrics that are correlated and irrelevant in order to improve model performance.
> Yet, conclusions that are derived from defect models may be inconsistent if the selected metrics are inconsistent and correlated.
> In this paper, we systematically investigate 11 automated feature selection techniques with respect to the consistency, correlation, performance, and computational cost dimensions.
> Through an empirical investigation of 13 publicly-available defect datasets, we find that (1) 90-100\% of the selected metrics are inconsistent among the studied techniques; (2) 31-94\% of the selected metrics are inconsistent among training samples; (3) 0-62\% of the selected metrics are inconsistent when the feature selection techniques are applied repeatedly; and (4) 3-100\% of the produced subsets of metrics contain highly correlated metrics.
> Since we find that the subsets of metrics produced by the commonly-used feature selection techniques (except for **AutoSpearman**) are often inconsistent and correlated, these techniques should be avoided when interpreting defect models. 
> In addition to introducing **AutoSpearman** which mitigates correlated metrics better than commonly-used feature selection techniques, this paper opens up new research avenues in the automated selection of features for defect models to optimise for interpretability as well as performance.
---

### Experimental Results

The repository consists of:

1. **Supplementary experimental results** - Subsets of metrics that are produced by 10 studied feature selection techniques and our own contribution, **AutoSpearman**, for all studied defect datasets.
2. **A collection of R code snippets for illustrative examples** - Illustrative examples for RQs 1, 2, 3, 4, and 5.
