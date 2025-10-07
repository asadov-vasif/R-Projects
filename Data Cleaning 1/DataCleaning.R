
library(tidyverse)
library(naniar)
library(patchwork)
library(data.table)
library(vcd)
library(grid)

install.packages('grid')

setwd('C:/Users/asado/Desktop/Projects/R + Python + Tableau')

getwd()

netflix_data <- read.csv('netflix_customer_churn.csv')

netflix_data %>% View()

df <- netflix_data


# 1. Explore the columns of the dataframe

df %>% colnames()

# 2. Drop the unnecessary columns

df <- df %>% select(-customer_id)


# 3. Clean the column names
colnames(df) <- trimws(colnames(df))
  
colnames(df) <- tolower(colnames(df))

colnames(df) <- gsub("[^[:alnum:]_]", "_", colnames(df))

# data types of the column names
colnames(df)


# 4. Clean the variables
unique(df$gender)
unique(df$subscription_type)
unique(df$region)
unique(df$device)
unique(df$payment_method)
unique(df$favorite_genre)

# 5. Check missing values 
sum(is.na(df))

gg_miss_var(df)
gg_miss_upset(df)
vis_miss(df)


# 6. Check duplicates 
sum(duplicated(df))


# 7. Check distributions with histogram
numeric_cols <- df %>% 
  select(where(is.numeric)) %>% colnames()



plots <- lapply(numeric_cols, function(col){
  
  ggplot(df, aes(x = .data[[col]])) +
    
  geom_histogram(bins = 10, fill = 'steelblue', color = 'white') +
  
  labs(title = paste("Histogram of", col)) + 
    
  theme_minimal()
})

wrap_plots(plots)



dt <- as.data.table(df)

long_dt <- melt(
  dt, 
  measure.vars = numeric_cols,
  variable.name = 'variable', 
  value.name = 'value'
) 

long_dt %>% View()

ggplot(
  long_dt, 
  aes(x = value)
) + 
  geom_histogram(
    bins = 10, fill = 'skyblue', color = 'grey'
  ) +
  facet_grid(~ variable, scales = 'free') + 
  theme_minimal()




# 8. Check distributions/outliers with box-plots

numeric_cols = names(df)[sapply(df, is.numeric)]


dt <- as.data.table(df)
long_dt <- melt(dt, 
                measure.vars = numeric_cols,
                variable.name = 'variable',
                value.name = 'value')

ggplot(
  long_dt,
  aes(x = variable, y = value, fill = variable)
) + 
  geom_boxplot(
    box.colour = 'black',
    box.linewidth = 1.2,
    whisker.colour = 'black', 
    whisker.linetype = 1.2,
    outlier.colour = 'steelblue'
  ) +
  theme_minimal()




# 9. Remove the outliers by clipping 

# find numeric features
numeric_cols <- names(df)[sapply(df, is.numeric)]


# find the upper and lower limits (fences) and store them inside the list
iqr_limits <- lapply(
  
  df[numeric_cols], 
  function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR = Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR 
    list(lower = lower, upper = upper)
  }
)

# for each numeric column, clip the lower and upper bounds
for (col in numeric_cols){
  lower <- iqr_limits[[col]]$lower
  upper <- iqr_limits[[col]]$upper
  
  df[[col]] <- pmin(pmax(df[[col]], lower),upper)
}




# show outliers as a table

numeric_cols <- names(df)[sapply(df, is.numeric)]

iqr_limits <- lapply(
  df[numeric_cols],
  function(x) {
    Q1 <-  quantile(x, 0.25, na.rm = TRUE) 
    Q3 <-  quantile(x, 0.75, na.rm = TRUE)
    IQR_ <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_
    upper <- Q3 + 1.5 * IQR_
    
    outliers <- x[x < lower | x > upper]
    
    list(
      lower_fence = lower, 
      upper_fence = upper, 
      outliers = outliers,
      n_outliers = length(outliers)
    )
  }
)

outliers_summary <- data.frame(
  variable = numeric_cols, 
  n_outliers = sapply(iqr_limits, function(x) x$n_outliers)
)

outliers_summary %>% View()



# 10. Check the frequency of categorical variables with bar charts and word counts

categoric_cols <- names(df)[sapply(df, is.character)]
categoric_cols


dt <- data.table(df)
long_dt <- melt(
  dt, 
  measure.vars = categoric_cols,
  variable.name = 'variable', 
  value.name = 'value'
)


freq_dt <- long_dt[, .N, by = .(variable, value)]
setorder(freq_dt, variable, -N)

ggplot(
  freq_dt, 
  aes(x = reorder(value, N), y = N, fill = variable)
) + 
  geom_col() + 
  geom_text(
    aes(label = N), hjust = -0.2, size = 3
  ) + 
  facet_wrap(~ variable, scales = 'free_y') + 
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = 'none') + 
  labs(x = 'Value', y = 'Frequency') 




# 11. Target is binary category. Visualize the correlations between numerical
# features and target category. 

dt <- as.data.table(df)

target_col <- 'churned'

numeric_cols <- setdiff(names(df)[sapply(df, is.numeric)], target_col)

# run anova for each numeric column
anova_results <- 
  lapply(numeric_cols, function(col) {
    formula <- as.formula(paste(col, "~", target_col))
    aov_model <- aov(formula, data = dt)
    
    summary_res <- summary(aov_model)[[1]]
    
    data.table(
      feature = col, 
      F_value = summary_res$`F value`[1], 
      p_value = summary_res$`Pr(>F)`[1]
    )
  })

anova_results <- rbindlist(anova_results)
setorder(anova_results, -F_value)
 
anova_results %>% View()


ggplot(
  anova_results, 
  aes(x = reorder(feature, F_value), y = F_value)
) + 
  geom_col(fill = 'darkcyan') + 
  coord_flip() + 
  geom_text(
    aes(label = round(p_value,3)), color = 'darkgreen', hjust = -0.5
  )+
  theme_minimal()





# 12. Target is binary category. Visualize the correlations between categorical
# features and target category. Chi square test

dt <- as.data.table(df)


categoric_cols = names(df)[sapply(df, is.character)]

target_col = 'churned'

results <- lapply(categoric_cols, function(col){
  contingency_table <- table(df[[col]], df$churned)
  
  chi <- chisq.test(contingency_table)
  cramers_v <- assocstats(contingency_table)$cramer
  
  data.table(
    feature = col,
    chi2 = chi$statistics,
    p_value = chi$p.value,
    cramers_v = cramers_v
  )
})

results <- rbindlist(results)
results <- results[order(- cramers_v)]


results[, significance := ifelse(p_value<0.05, 'Significant', 'Not significant')]

ggplot(
  results,
  aes(x = reorder(feature, cramers_v), y = cramers_v, fill = significance)
) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_manual(values = c("Significant" = "darkblue", "Not significant" = "grey")) +
  geom_text(
    aes(label = round(cramers_v,3)), hjust = -0.5, size = 4, 
    color = ifelse(results$significance=='Significant', 'darkblue', 'darkgrey')
  )
  theme_minimal()





# 13. Correlation and multicollinearity check for numerical columns.

dt <- data.table(df)
  
numeric_cols = names(df)[sapply(df, is.numeric)]
  
  
correlation_matrix <- cor(dt[, ..numeric_cols], use = 'pairwise.complete.obs')


correlation_matrix <- as.data.table(correlation_matrix, keep.rownames = 'Var1')
  

correlation_matrix <- melt(correlation_matrix, id.vars = 'Var1', 
                           variable.name = 'Var2', value.name = 'Correlation')
  

  
ggplot(
  correlation_matrix,
  aes(x = Var1, y = Var2, fill = Correlation)
) + 
  geom_tile(color = 'black') + 
  geom_text(
    aes(label = round(Correlation,2))
  )+
  scale_fill_gradient2(low = "#440154FF", mid = "gray", high = "#2A788EFF", midpoint = 0) +
  theme_minimal() 
  
  
  
  
  


# 14. SHAP Analysis - Feature Importance

# Ensure df is a data.table
dt <- as.data.table(df)

# -------------------------
# 1. Prepare Data
# -------------------------
target <- "churned"

# Split categorical and numeric features
categoric_cols <- names(dt)[sapply(df, is.character)]
numeric_cols   <- names(dt)[sapply(df, is.numeric) & names(df) != target]

# One-hot encode categoricals using data.table::model.matrix
# This creates dummy variables for factors/characters
X <- model.matrix(~ . -1, data = dt[, ..categoric_cols])

# Combine with numeric features
X <- cbind(dt[, ..numeric_cols], X)

# Convert to numeric matrix for XGBoost
X <- as.matrix(X)

# Target vector
y <- dt[[target]]

# -------------------------
# 2. Train XGBoost Model
# -------------------------
xgb_model <- xgboost(
  data = X,
  label = y,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "logloss",
  verbose = 0
)

# -------------------------
# 3. Compute SHAP Values
# -------------------------
shap_values <- shap.values(xgb_model, X_train = X)

# Long-format SHAP
shap_long <- shap.prep(
  shap_contrib = shap_values$shap_score,
  X_train = X
)

# 4. Visualizations
shap.plot.summary(shap_long)

# Feature Importance
shap_importance <- shap_long[, .(mean_abs_shap = mean(abs(value))), by = variable]

ggplot(shap_importance,
       aes(x = reorder(variable, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Features",
    y = "Mean |SHAP value|",
    title = "Feature Importance (SHAP)"
  ) +
  theme_minimal()





# 15. Encode character columns

character_cols <- names(df)[sapply(df, is.character)]
character_cols


# Create dummy variables
dummy_df <- model.matrix(~ gender + subscription_type + region + device + 
                           payment_method + favorite_genre - 1, data = df)

# Convert to data.frame
dummy_df <- as.data.frame(dummy_df)

# View column names then addthem into the main function
colnames(dummy_df)

encoded_df <- cbind(df[, !names(df) %in% character_cols], dummy_df)
encoded_df %>% View()




# 17. Export cleaned data.frame into the csv file
write.csv(encoded_df, 'cleaned_netflix_data.csv', row.names = F)


encoded <- read.csv('cleaned_netflix_data.csv')
encoded %>% View()









