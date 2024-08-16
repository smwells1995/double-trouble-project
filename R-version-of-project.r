# Set up the environment with packages
library(tidyverse)
library(tidymodels)
library(psych)
library(corrplot)
library(DAAG)

# Import, preview, and explore data
url <- "https://raw.githubusercontent.com/smwells1995/double-trouble-project/main/ais.csv"
ais_data <- read.csv(url)
head(ais_data)

# 1: Visualize the distribution of red blood cell count (rcc) by sex (sex)
# Boxplot
ggplot(data = select(ais_data, rcc, sex), aes(x = sex, y = rcc)) +
    geom_boxplot(width = 0.5,
                 fill = "skyblue", 
                 color = "black",
                 outlier.color = "orange") +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) +
    xlab("Sex") +
    ylab("Red Blood Cell Count (rcc)") +
    ggtitle("Distribution of Red Blood Cell Count by Sex")

ggsave("rcc_dist_boxplot.png", plot = last_plot())

# Histogram
iqr_value <- IQR(select(ais_data, rcc, sex)$rcc)
n <- length(select(ais_data, rcc, sex)$rcc)
binwidth_fd <- 2 * iqr_value / (n^(1/3))

ggplot(data = select(ais_data, rcc, sex), aes(x = rcc)) +
    geom_histogram(binwidth = binwidth_fd, 
                   fill = "skyblue", 
                   color = "black") +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) +
    facet_grid(~sex) +
    xlab("Sex") +
    ylab("Red Blood Cell Count (rcc)") +
    ggtitle("Distribution of Red Blood Cell Count by Sex")

ggsave("rcc_dist_hist.png", plot = last_plot())

#2: Is there a significant difference in red blood cell count between the two groups of sex?

t.test(rcc ~ sex, data = select(ais_data, rcc, sex))
#Yes, there is evidence of significant difference; p < 0.05

#3: Produce a correlation matrix of the relevant variables in this dataset
cor_matrix <- ais_data %>%
  select(-c(sex, sport)) %>%
  cor()

png("correlation_circle.png", width = 800, height = 800)
corrplot(cor_matrix, method = "circle",
        tl.cex = 1.2,
        cl.cex = 1.2)
dev.off()

png("correlation_num.png", width = 800, height = 800)
corrplot(cor_matrix, method = "number",
        tl.cex = 1.2,
        cl.cex = 1.2)
dev.off()

#4: Visualize the relationship of height (ht) and weight (wt)
#5: Regress ht on wt. Find the equation of the fit regression line. 
#Is there a significant relationship? What percentage of the variance in ht is explained by wt?

ggplot(data = select(ais_data, ht, wt), aes(x = wt, y = ht)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", formula = y ~ x, color = "orange", se = FALSE) +
  theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) +
  xlab("Weight (lb)") +
  ylab("Height (in)") +
  ggtitle("Relationship between Height and Weight")

ggsave("ht_wt_regression.png", plot = last_plot())

ais_regress <- lm(ht ~ wt, data = select(ais_data, ht, wt))
summary(ais_regress)

#p < 0.05, sufficient evidence of significant relationship between ht and wt
#R-squared value = 0.6079 means 60.79% of variance in ht that can be explained by wt

#6: Split your regression model into training and testing subsets
#What is the R-squared and RMSE on your test model?

set.seed(1234)
ais_split <- initial_split(ais_data)
ais_train <- training(ais_split)
ais_test <- testing(ais_split)

dim(ais_train)
dim(ais_test)

lm_spec <- linear_reg()
lm_fit <- lm_spec %>% 
  fit(ht~wt, data = ais_train)

ais_results <- predict(lm_fit, new_data = ais_test) %>% 
  bind_cols(ais_test)

rsq_tib <- rsq(data = ais_results, truth = ht, estimate = .pred)
rsq_val <- rsq_tib$.estimate
cat("Rounded to 3 places, the R-squared value is", round(rsq_val, 3), ".\n")

rmse_tib <- rmse(data = ais_results, truth = ht, estimate = .pred)
rmse_val <- rmse_tib$.estimate
cat("Rounded to 3 places, the RMSE value is", round(rmse_val, 3), ".\n")