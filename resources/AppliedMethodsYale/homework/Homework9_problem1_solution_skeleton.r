## Problem 1



library(GenericML)
### 1. Data Generation ----
set.seed(31684591)
D <- as.numeric(data_jpal$treatment) # random treatment assignment
varnames_Z <- c("ever_export_b", "log_weight_sidda_b", "qual_corners_b", "qual_waviness_b", "qual_weight_b", "qual_touch_b", "qual_sidda_packed_b")
# varnames_Z <- c("ever_export_b", "log_weight_sidda_b", "qual_corners_b")
Z <- as.matrix(data_jpal[, varnames_Z]) # design matrix
colnames(Z) <- varnames_Z # column names
Y <- as.numeric(data_jpal$ever_export) # observed outcome
n <- length(Y)
### 2. Prepare the arguments for GenericML() ----

# quantile cutoffs for the GATES grouping of the estimated CATEs
quantile_cutoffs <- c(0.25, 0.5, 0.75) # 25%, 50%, and 75% quantiles

# specify the learner of the propensity score (non-penalized logistic regression here). Propensity scores can also directly be supplied.
learner_propensity_score <- "constant"

# specify the considered learners of the BCA and the CATE (here: lasso, random forest, and SVM)
learners_GenericML <- c("mlr3::lrn('ranger', num.trees = 100)", "mlr3::lrn('svm')")
# "mlr3::lrn('ranger', num.trees = 100)", "mlr3::lrn('svm')")

# specify the data that shall be used for the CLAN
# here, we use all variables of Z and uniformly distributed random noise
Z_CLAN <- cbind(Z)

# specify the number of splits (many to rule out seed-dependence of results)
num_splits <- 1000

# specify if a HT transformation shall be used when estimating BLP and GATES
HT <- FALSE

# A list controlling the variables that shall be used in the matrix X1 for the BLP and GATES regressions.
X1_BLP <- setup_X1()
X1_GATES <- setup_X1()

# consider differences between group K (most affected) with groups 1, 2, and 3, respectively.
diff_GATES <- setup_diff(
    subtract_from = "most",
    subtracted = 1:3
)
diff_CLAN <- setup_diff(
    subtract_from = "most",
    subtracted = 1:3
)

# specify the significance level
significance_level <- 0.05

# specify minimum variation of predictions before Gaussian noise with variance var(Y)/20 is added.
min_variation <- 1e-05

# specify which estimator of the error covariance matrix shall be used in BLP and GATES (standard OLS covariance matrix estimator here)
vcov_BLP <- setup_vcov()
vcov_GATES <- setup_vcov()

# specify whether of not it should be assumed that the group variances of the most and least affected groups are equal in CLAN.
equal_variances_CLAN <- FALSE

# specify the proportion of samples that shall be selected in the auxiliary set
prop_aux <- 0.5

# specify sampling strategy (possibly stratified). Here ordinary random sampling is used.
stratify <- setup_stratify()

# specify whether or not the splits and auxiliary results of the learners shall be stored
store_splits <- TRUE
store_learners <- FALSE # to save memory

# parallelization options
parallel <- TRUE
num_cores <- 4 # 8 cores
seed <- 123456
# Note that the number of cores as well as your type of operating system (Unix vs. Windows) influences the random number stream. Thus, different choices of `num_cores` may lead to different results. Results of parallel processes are reproducible across all Unix systems, but might deviate on Windows systems.

x <- GenericML(
    Z = Z, D = D, Y = Y,
    learner_propensity_score = "constant",
    learners_GenericML = learners_GenericML,
    num_splits = num_splits,
    Z_CLAN = Z_CLAN,
    HT = HT,
    X1_BLP = X1_BLP,
    X1_GATES = X1_GATES,
    vcov_BLP = vcov_BLP,
    vcov_GATES = vcov_GATES,
    quantile_cutoffs = quantile_cutoffs,
    diff_GATES = diff_GATES,
    diff_CLAN = diff_CLAN,
    equal_variances_CLAN = equal_variances_CLAN,
    prop_aux = prop_aux,
    stratify = stratify,
    significance_level = significance_level,
    min_variation = min_variation,
    parallel = parallel,
    num_cores = num_cores,
    seed = seed,
    store_splits = store_splits,
    store_learners = store_learners
)
