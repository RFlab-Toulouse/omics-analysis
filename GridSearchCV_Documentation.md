# GridSearchCV Hyperparameter Tuning - Documentation

## Overview

This Shiny application now supports advanced hyperparameter tuning using **GridSearchCV** from the {superml} R package. This feature provides more comprehensive and systematic hyperparameter optimization compared to traditional tuning methods.

## What is GridSearchCV?

GridSearchCV (Grid Search Cross-Validation) is an exhaustive search method that:
- Tests multiple combinations of hyperparameters in a systematic grid
- Uses cross-validation to evaluate each combination
- Automatically selects the best performing combination based on scoring metrics
- Provides more extensive parameter exploration than traditional tuning methods

## Supported Models

GridSearchCV is available for the following machine learning models:

### 1. **Random Forest**
- **Traditional method**: `tuneRF()` - only tunes `mtry` parameter
- **GridSearchCV**: Tunes multiple parameters simultaneously
  - `n_estimators` (ntree): Number of trees [100, 500, 1000]
  - `max_features` (mtry): Variables per split ["sqrt", "log2"]
  - `min_samples_split` (nodesize): Minimum samples to split a node [2, 5, 10]

**Benefits**: More comprehensive tuning beyond just mtry, can optimize tree depth and sample splitting

### 2. **XGBoost**
- **Traditional method**: `xgb.cv()` - limited parameter grid
- **GridSearchCV**: Comprehensive hyperparameter optimization
  - `n_estimators` (nrounds): Number of boosting rounds [50, 100, 200]
  - `max_depth`: Maximum tree depth [3, 6, 9]
  - `learning_rate` (eta): Learning rate [0.01, 0.1, 0.3]
  - `gamma`: Minimum loss reduction [0, 0.1, 0.5]
  - `subsample`: Subsample ratio [0.8, 1.0]

**Benefits**: Explores more hyperparameters including gamma and subsample for better regularization

### 3. **Naive Bayes**
- **Traditional method**: No hyperparameter tuning
- **GridSearchCV**: Tunes smoothing parameter
  - `laplace`: Laplace smoothing parameter [0, 0.5, 1, 2, 5]

**Benefits**: Optimizes smoothing to handle zero probabilities and improve generalization

### 4. **K-Nearest Neighbors (KNN)**
- **Traditional method**: Manual cross-validation for k only
- **GridSearchCV**: More comprehensive neighbor selection
  - `n_neighbors` (k): Number of neighbors [3, 5, 7, ..., up to sqrt(n)]

**Benefits**: More systematic search through possible k values with robust cross-validation

### 5. **Elastic Net (Logistic Regression)**
- **Traditional method**: `cv.glmnet()` - tunes lambda for fixed alpha
- **GridSearchCV**: Joint optimization of regularization parameters
  - `alpha`: L1/L2 mixing parameter [0, 0.25, 0.5, 0.75, 1.0]
    - 0 = Ridge (L2 only)
    - 1 = Lasso (L1 only)
    - 0.5 = Elastic Net (balanced L1/L2)
  - `lambda`: Regularization strength [0.001, 0.01, 0.1, 1.0]

**Benefits**: Finds optimal balance between L1 and L2 regularization automatically

## How to Use GridSearchCV in the Shiny App

### Step 1: Enable GridSearchCV
1. Navigate to the **"Model"** tab in the Shiny application
2. Select your desired model type (Random Forest, XGBoost, Naive Bayes, KNN, or Elastic Net)
3. Check the box **"Use GridSearchCV (superml) for hyperparameter tuning"**
4. Ensure the model-specific automatic tuning is also enabled:
   - Random Forest: Check "Automatic mtry tuning (tuneRF)"
   - XGBoost: Check "Automatic hyperparameter tuning (CV)"
   - KNN: Check "Automatic k tuning (CV)"
   - Elastic Net: Check "Automatic lambda selection (CV)"

### Step 2: Train Your Model
1. Make sure you have completed the previous steps:
   - Data import
   - Data selection
   - Data transformation (optional)
   - Statistical testing (optional)
2. Click the model tab and your model will automatically train with GridSearchCV

### Step 3: Review Results
- GridSearchCV will print the best parameters found to the console
- The model will be trained with the optimal parameters
- Model performance metrics will be displayed as usual

## Comparison: Traditional vs GridSearchCV

| Model | Traditional Method | GridSearchCV Method | Additional Parameters Tuned |
|-------|-------------------|---------------------|----------------------------|
| Random Forest | tuneRF (mtry only) | Comprehensive grid search | ntree, nodesize, max_depth |
| XGBoost | xgb.cv (basic) | Full grid search | gamma, subsample, colsample |
| Naive Bayes | None | Laplace smoothing | laplace |
| KNN | Manual CV (k only) | Systematic grid search | Better k selection |
| Elastic Net | cv.glmnet (lambda) | Joint alpha/lambda | alpha (L1/L2 mixing) |

## Performance Metrics

GridSearchCV uses the following metrics to evaluate models:
- **AUC** (Area Under ROC Curve) - Primary metric
- **Accuracy** - Secondary metric

The best parameter combination is selected based on the highest AUC score.

## Default Parameter Grids

### Random Forest
```r
n_estimators: c(100, 500, 1000)
max_features: c("sqrt", "log2")
min_samples_split: c(2, 5, 10)
```
**Total combinations**: 6

### XGBoost
```r
n_estimators: c(50, 100, 200)
max_depth: c(3, 6, 9)
learning_rate: c(0.01, 0.1, 0.3)
gamma: c(0, 0.1, 0.5)
subsample: c(0.8, 1.0)
```
**Total combinations**: 162 (can be reduced by customizing grid)

### Naive Bayes
```r
laplace: c(0, 0.5, 1, 2, 5)
```
**Total combinations**: 5

### KNN
```r
n_neighbors: seq(3, sqrt(n), by=2)
```
**Total combinations**: Varies based on sample size

### Elastic Net
```r
alpha: c(0, 0.25, 0.5, 0.75, 1.0)
lambda: c(0.001, 0.01, 0.1, 1.0)
```
**Total combinations**: 20

## Cross-Validation Strategy

- **Number of folds**: 5 (default)
- **Scoring metrics**: AUC (primary), Accuracy (secondary)
- **Selection criterion**: Maximum AUC score

## Installation Requirements

To use GridSearchCV, ensure you have the following R packages installed:

```r
install.packages("superml")
install.packages("randomForest")
install.packages("xgboost")
install.packages("e1071")  # For Naive Bayes and SVM
install.packages("class")  # For KNN
install.packages("glmnet") # For Elastic Net
```

## Advantages of GridSearchCV

1. **Comprehensive Search**: Tests multiple parameter combinations systematically
2. **Automatic Optimization**: No manual parameter tuning required
3. **Cross-Validation**: Robust evaluation using k-fold CV
4. **Multiple Metrics**: Can optimize for different scoring metrics
5. **Consistent Interface**: Same API across different model types
6. **Better Generalization**: More likely to find globally optimal parameters

## When to Use GridSearchCV

**Use GridSearchCV when**:
- You want the best possible model performance
- You have sufficient computational resources (GridSearchCV is more intensive)
- You're working with complex datasets where hyperparameter choice is critical
- You want to explore multiple hyperparameters simultaneously

**Use traditional methods when**:
- You need faster model training
- Your dataset is very large (GridSearchCV can be slow)
- You have good intuition about reasonable parameter values
- You're doing exploratory analysis

## Computational Considerations

- **Training time**: GridSearchCV can take significantly longer than traditional methods due to exhaustive search
- **Memory**: Requires sufficient memory for cross-validation folds
- **Recommendation**: For large datasets (>10,000 samples), consider:
  - Using smaller parameter grids
  - Reducing number of CV folds
  - Using traditional tuning methods first

## Troubleshooting

### GridSearchCV fails or throws errors
The implementation includes automatic fallback to traditional tuning methods if GridSearchCV fails. Check the console for error messages.

### Models take too long to train
- Reduce the parameter grid size
- Use fewer cross-validation folds
- Consider using traditional tuning methods for initial exploration

### Results seem unstable
- Increase number of CV folds (requires modifying code)
- Check for data leakage or overfitting
- Ensure sufficient sample size for cross-validation

## Advanced Customization

To customize parameter grids, modify the parameter grid definitions in `global.R`:

For Random Forest (lines 1156-1160):
```r
param_grid <- list(
  n_estimators = c(100, 500, 1000),  # Customize these values
  max_features = c("sqrt", "log2"),
  min_samples_split = c(2, 5, 10)
)
```

Similarly for other models in their respective sections.

## Technical Implementation

The GridSearchCV implementation:
1. **Wrapper functions** (`tune_*_gridsearch`): Interface between superml and native R implementations
2. **Parameter conversion**: Maps superml parameter names to native R parameter names
3. **Fallback mechanism**: Automatically reverts to traditional methods if GridSearchCV fails
4. **Result extraction**: Extracts best parameters and applies them to final model

## References

- superml package: https://CRAN.R-project.org/package=superml
- GridSearchCV documentation: See superml package documentation
- Original implementation: Based on scikit-learn's GridSearchCV

## Support

For issues or questions about GridSearchCV:
1. Check console output for error messages
2. Verify all required packages are installed
3. Review this documentation
4. Check the implementation in `global.R` (lines 906-1125)

---

**Last Updated**: 2025-11-18
**Author**: Implementation by Claude
**Version**: 1.0
