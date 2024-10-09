setwd("D:/projects/SMU/causal-inference")
library(readstata13)
library(grf)
library(policytree)
library(dplyr)
library("htmltools")

source("ATE_with_Pvalue.R")
source("rule_policy_tree_functions.R")
source("rules_policy_tree_plot.R")

data <- read.dta13("data/online_follow-ups_data.dta")

######################################################
# ONLINE FOLLOW-UPS TO OFFLINE DEMAND
######################################################
covariances <- c("l_online_pay_ln", "l_review_ln", "l_rating_cum", "l_article_ln", "month_12", "year")

outcome <- "on2off_ln"

filtering_data <- function(data) {
    # only use data from 2012 to 2017
    data <- data[which(data$year >= 2012 & data$year <= 2017), ]

    data$month_12 <- as.factor(data$month_12)
    data$year <- as.factor(data$year)

    variables_to_check <- c(outcome, "doc_adoption", covariances)

    # ensure that code can run as missing value not allowed in causal forest.
    df_clean <- data[complete.cases(data[variables_to_check]), ]

    Y <- df_clean$outcome
    W <- df_clean$doc_adoption
    X <- as.data.frame(sapply(df_clean[, covariances], as.numeric))

    return(list(X = X, Y = Y, W = W))
}

result <- filtering_data(data)
X <- result$X
Y <- result$Y
W <- result$W

c.forest <- causal_forest(X, Y, W, num.trees = 10000, honesty = TRUE, seed = 1)


# Step 1
ATE <- average_treatment_effect_w_ss(c.forest, target.sample = "overlap")
print("====ATE====")
print(ATE)

# Step 2
BLP <- best_linear_projection(c.forest, X, target.sample = "overlap")
print("===Best Linear Projection===")
print(BLP)

# all outputs written here
results <- list()
var_list <- list()

var_list[[paste0("ATE")]] <- ATE
var_list[[paste0("best_lin")]] <- BLP
results[[paste0("result_", outcome)]] <- var_list

# Step 3
# path of output tree
path <- "D:/projects/SMU/causal-inference/output/"

dr.scores <- double_robust_scores(c.forest)

## depth 1
dep <- 1
tree <- policy_tree(X, dr.scores, depth = dep)
save_as_csv_and_html(tree, path, outcome, dep)

## depth 2
dep <- 2
tree <- policy_tree(X, dr.scores, depth = dep)
save_as_csv_and_html(tree, path, outcome, dep)


######################################################
# ONLINE FOLLOW-UPS TO ONLINE DEMAND
######################################################
covariances <- c("l_online_pay_doc_num_m_ln", "l_online_pay_doc_length_m_ln", "l_online_pay_speed_p", "l_on2off_ln", "l_review_ln", "l_rating_cum", "l_article_ln", "month_12", "year")

outcome <- "online_pay_ln"

filtering_data <- function(data) {
    # only use data from 2012 to 2017
    data <- data[which(data$year >= 2012 & data$year <= 2017), ]

    data$month_12 <- as.factor(data$month_12)
    data$year <- as.factor(data$year)

    variables_to_check <- c(outcome, "doc_adoption", covariances)

    # ensure that code can run as missing value not allowed in causal forest.
    df_clean <- data[complete.cases(data[variables_to_check]), ]

    Y <- df_clean$online_pay_ln
    W <- df_clean$doc_adoption
    X <- as.data.frame(sapply(df_clean[, covariances], as.numeric))

    return(list(X = X, Y = Y, W = W))
}

result <- filtering_data(data)
X <- result$X
Y <- result$Y
W <- result$W

c.forest <- causal_forest(X, Y, W, num.trees = 10000, honesty = TRUE, seed = 1)


# Step 1
ATE <- average_treatment_effect_w_ss(c.forest, target.sample = "overlap")
print("====ATE====")
print(ATE)

# Step 2
BLP <- best_linear_projection(c.forest, X, target.sample = "overlap")
print("===Best Linear Projection===")
print(BLP)

# all outputs written here
results <- list()
var_list <- list()

var_list[[paste0("ATE")]] <- ATE
var_list[[paste0("best_lin")]] <- BLP
results[[paste0("result_", outcome)]] <- var_list

# Step 3
# path of output tree
path <- "D:/projects/SMU/causal-inference/output/"

dr.scores <- double_robust_scores(c.forest)

## depth 1
dep <- 1
tree <- policy_tree(X, dr.scores, depth = dep)
save_as_csv_and_html(tree, path, outcome, dep)

## depth 2
dep <- 2
tree <- policy_tree(X, dr.scores, depth = dep)
save_as_csv_and_html(tree, path, outcome, dep)
