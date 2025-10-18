
library(tidyverse)
library(Hmisc)
library(naniar)
library(forcats)
library(naniar)


df <- read.csv('German_credit.csv')

# DF columns
df %>% colnames()


##---------------------UNDERSTANDING THE DATA-----------------------------------

# Question 1.1: Check dataset structure. 

str(df) # Structure of data

dim(df) # rows and columns: [1000, 21]

summary(df)

sapply(df, class) 


# Question 1.2: View first and last few rows
head(df, 5) %>% View()



# Question 1.3: Summary statistics


# Identify numeric columns
num_cols <- sapply(df, is.numeric)

# Identify categorical columns (character or factor)
cat_cols <- sapply(df, function(x) is.factor(x) | is.character(x))

# Summary statistics for the numerical columns
num_summary <- data.frame(
  Variable = names(df)[num_cols],
  Min = sapply(df[num_cols], min, na.rm = TRUE),
  Max = sapply(df[num_cols], max, na.rm = TRUE),
  Mean = sapply(df[num_cols], mean, na.rm = TRUE),
  Median = sapply(df[num_cols], median, na.rm = TRUE),
  SD = sapply(df[num_cols], sd, na.rm = TRUE)
)

num_summary %>% View()


# Categoric columns
cat_summary <- data.frame(
  Variable = names(df)[cat_cols],
  Unique_Values = sapply(df[cat_cols], function(x) length(unique(x))),
  Most_Common = sapply(df[cat_cols], function(x) names(which.max(table(x)))),
  Most_Common_Count = sapply(df[cat_cols], function(x) max(table(x))),
  stringsAsFactors = FALSE
)

cat_summary %>% View()



# Convert target variable into numeric column
df$class <- ifelse(df$class == 'good', 1, 0)
df %>% View()




# -------------------2. Exploratory Data Analysis (EDA)-------------------------


# ----------Distributions of numeric columns------------------------------------

# Identify numeric columns
numeric_cols <- df %>%
  select(where(is.numeric)) %>%
  colnames()

# Set up plotting area
n <- length(numeric_cols)
nrows <- ceiling(n / 3)

par(mfrow = c(nrows, 3), mar = c(3, 3, 2, 1))

# Plot histograms and density curves
for (col in numeric_cols) {
  hist(
    df[[col]],
    breaks = 30,
    main = paste("Histogram of", col),
    xlab = col,
    col = "lightblue",
    border = "white"
  )
  lines(
    density(df[[col]], na.rm = TRUE),
    col = "red",
    lwd = 2
  )
}


# Box-plots of numerical variables

df %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = "", y = value, fill = variable)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.2, alpha = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Boxplots of Numerical Variables (Independent Scales)",
    x = "",
    y = "Value"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  )





#-----------------------Stats of categorical columns------------------------------


# Count plots of unique values inside categorical features
df %>%
  select(where(is.character) | where(is.factor)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "category") %>%
  count(variable, category, sort = TRUE) %>%
  group_by(variable) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = fct_reorder(category, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  labs(
    title = "Top 10 Categories per Categorical Variable",
    x = "Category", y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


df %>%
  mutate(class = factor(class)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = class, y = value, fill = class)) +
  geom_boxplot(alpha = 0.8, outlier.color = "black") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of Numerical Variables by Class",
    x = "Class (0 = Bad, 1 = Good)",
    y = "Value"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold")
  )



# MISSING VALUES INSPECTION
naniar::miss_var_summary(df)

# There is no missing values in the data.


# OUTLIERS

num_cols <- names(df)[sapply(df, is.numeric)]
outliers_iqr <- df %>%
  select(all_of(num_cols)) %>%
  summarise(across(everything(), ~ sum(. < (quantile(., 0.25) - 1.5 * IQR(.)) |
                                         . > (quantile(., 0.75) + 1.5 * IQR(.)))))

transpose(outliers_iqr)


# clip the outliers 

# Function to clip a single column
clip_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

# Apply to all numeric columns
df_clipped <- df %>%
  mutate(across(where(is.numeric), clip_iqr))


df <- df_clipped

View(df)
# Check the box-plots of distributions
df %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = "", y = value, fill = variable)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.2, alpha = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Boxplots of Numerical Variables (Independent Scales)",
    x = "",
    y = "Value"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  )




#-------------------Scaling the numerical variables----------------------------

num_cols <- df %>% select(where(is.numeric), -class) %>% names()
num_cols


df <- df %>% select(-num_dependents) # 1 unique value, therefore, unnecessary

df %>% select(where(is.numeric)) %>% View()

df <- df %>%
  mutate(across(all_of(num_cols), ~ as.numeric(scale(.))))


df %>% View()



#----------------ENCODE CATEGORICAL VARIABLES-----------------------------

cat_cols <- df %>% select(where(is.character) | where(is.factor)) %>% names()

cat_cols

df <- cbind(
  df %>% select(-all_of(cat_cols)),                   # keep numeric columns + target
  as.data.frame(model.matrix(~ . - 1, data = df[, cat_cols]))  # one-hot categorical columns
)

df %>% View()


df <- df %>%
  select(class, everything())

df %>% View()



#--------------------------EXPORT TO CSV----------------------------------------

write.csv(df, "processed_german_credit_data.csv", row.names = FALSE)












