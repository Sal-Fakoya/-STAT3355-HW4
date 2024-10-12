
# Question 1

path <- "C:\\Users\\fakoy\\OneDrive - Houston Community College\\UTD_Courses\\Fall2024\\Data_Analysis_withR\\HW4"
setwd(path)

library(ggplot2)
library(tidyr)
library(UsingR)
library(readr)
library(dplyr)

df <- read.csv(file = "./HW4_Data/train.csv", header = T)
head(df)

# 1a: scatterplot between battery_power vs ram
df$price_range <- factor(df$price_range)
base_plot <- df %>% 
  ggplot(data = ., 
         mapping = 
           aes(x = ram, color = price_range))

scatter_plot <- base_plot +
  geom_point(mapping = aes( y = battery_power))

scatter_plot

# 1b:
scatter_plot_w_trend <- scatter_plot +
  geom_smooth(mapping = aes( y = battery_power),
              method = 'loess', 
              formula = 'y ~ x',
              fill = NA) +
  coord_cartesian(ylim= 
                    c(round(min(df$battery_power), 2),
                      round(max(df$battery_power), 2))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c("top", "right"),
        legend.justification.inside = c(1, 1),
        legend.location = "panel") 
scatter_plot_w_trend

# 1c: density curves in one plot
density_curve <- base_plot + 
  geom_density()

?theme
density_curve
?aes

#1d: boxplots in one plot
boxplots <- base_plot +
  geom_boxplot()
boxplots

# 1e: violin plot
violin <- base_plot + 
  geom_violin(mapping = aes(x = price_range, y = ram)) +
  xlab("Price Range") +
  ylab("RAM") + 
  ggtitle("RAM vs Price Range") +
  theme(plot.title = element_text(hjust = 0.5))

# 1f: stacked bar plot
bar_plot <- base_plot +
  geom_bar(mapping = 
             aes(x = round(log(ram, 2)),
                 fill = price_range),
           position = "stack")


# -------------------------------------------------------
# Problem 2:
df <- UScereal


# 2a: replace levels of the factor variable mfr to their full names
levels(df$mfr) <- c("General Mills", 
                    "Kellogs", 
                    "Nabisco", 
                    "Post", 
                    "Quaker Oats",
                    "Ralston Purina")

# 2b: turn variable shelf to a factor variable

df <- df %>% 
  mutate(shelf = factor(shelf))


# 2c: Create new variable Product for the product name
rows <- rownames(df)
df <- df %>% 
  mutate(product = rows)

# Check str(df)
str(df)

# d: Calculate the Pearson Correlation coefficient between calories and each seven nutrition facts

pearson <- lapply(df[, c("protein", 
                         "fat",
                         "sodium",
                         "fibre",
                         "carbo",
                         "sugars",
                         "potassium")], 
                  FUN = cor, 
                  x = df$calories,
                  method = "pearson") %>% 
  data.frame()
rownames(pearson) <- "calories"

# 2e: make a bar plot of the resulting correlations in (a) and arrange the nutrition facts in decreasing order in terms of their correlation with calories
sorted_pearson <- pearson %>% 
  t() %>% 
  data.frame() %>% 
  arrange(desc(calories))


# which nutrition fact has the highest values: ans: carbo
max_pearson <- c(pearson[which(max(pearson) == pearson)])
pearson
max_pearson

# 2f: scatter plot where y represents calories and x represents the nutrition fact with the largest pearson correlation coefficient to calories

scatter_plot_w_trend <- ggplot(data = df,
                               mapping = aes(x = df[, names(max_pearson)],
                                     y = calories)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = "y ~ x",
              fill = NA)

scatter_plot_w_trend





































