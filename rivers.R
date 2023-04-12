# Load required packages
library(readxl)
library(ggplot2)
library(dplyr)
library(rcartocolor)
library(outliers)
library(xlsx)
library(stats)
library(stargazer)
library(texreg)

### Take in data

# Read in Excel spreadsheet data
df <- read_excel("C:\\Users\\chris\\Downloads\\delaware_river_trenton_nj last_five_years clean 04-01-23 0614pm.xlsx")

# Create a new variable "year" that extracts the year from the datetime variable
df <- df %>% mutate(year = as.integer(format(datetime, "%Y")))

# Remove data after 2020
df_filtered <- df %>% filter(year <= 2020)

### Check summary of nitrate
summary(df_filtered$nitrate_mg_per_liter)

### Calculuate z-scores, modified z-scores
df_filtered$nitrate_z_scores <- (df_filtered$nitrate_mg_per_liter - mean(df_filtered$nitrate_mg_per_liter, na.rm=TRUE)) / sd(df_filtered$nitrate_mg_per_liter, na.rm=TRUE)
df_filtered$water_z_scores <- (df_filtered$temp_of_water_celsius - mean(df_filtered$temp_of_water_celsius, na.rm=TRUE)) / sd(df_filtered$temp_of_water_celsius, na.rm=TRUE)

df_filtered$abs_nitrate_z_scores <- abs(df_filtered$nitrate_z_scores)
df_filtered$abs_water_z_scores <- abs(df_filtered$water_z_scores)

nitrate_modified_z_scores <- (0.6745 * (df_filtered$nitrate_mg_per_liter - median(df_filtered$nitrate_mg_per_liter, na.rm = TRUE))) / mad(df_filtered$nitrate_mg_per_liter, na.rm = TRUE)
water_modified_z_scores <- (0.6745 * (df_filtered$temp_of_water_celsius - median(df_filtered$temp_of_water_celsius, na.rm = TRUE))) / mad(df_filtered$temp_of_water_celsius, na.rm = TRUE)

df_filtered$nitrate_modified_z_scores <- nitrate_modified_z_scores
df_filtered$water_modified_z_scores <- water_modified_z_scores

df_filtered$abs_nitrate_modified_z_scores <- abs(df_filtered$nitrate_modified_z_scores)
df_filtered$abs_water_modified_z_scores <- abs(df_filtered$water_modified_z_scores)

df_filtered$water_z_scores_rank <- percent_rank(df_filtered$abs_water_z_scores)
df_filtered$water_modified_z_scores_rank <- percent_rank(df_filtered$abs_water_modified_z_scores)

df_filtered$nitrate_z_scores_rank <- percent_rank(df_filtered$abs_nitrate_z_scores)
df_filtered$nitrate_modified_z_scores_rank <- percent_rank(df_filtered$abs_nitrate_modified_z_scores)

### Check for normal distribution

# Calculate mean and standard deviation of the data
mean_nitrate <- mean(df_filtered$nitrate_mg_per_liter, na.rm = TRUE)
sd_nitrate <- sd(df_filtered$nitrate_mg_per_liter, na.rm = TRUE)

# Create the histogram and density plot
geyser <- carto_pal(7, "Geyser")

nitrate_histogram <- ggplot(df_filtered, aes(x = nitrate_mg_per_liter)) +
  geom_histogram(aes(y = ..density..), bins = 24, color = geyser[3], fill = "white", size = .8) +
  stat_function(fun = dnorm, args = list(mean = mean_nitrate, sd = sd_nitrate),
                color = geyser[5], size = .9) +
  labs(x = "Nitrate (mg/L)", y = "Density",
       title = "Distribution of nitrate concentrations")

ggsave("nitrate_hist.png", width=3.13, height=5.54, nitrate_histogram)

# Filter the dataset for data from 2018 to 2020
df_filtered_2018 <- df_filtered %>% filter(year == 2018)
df_filtered_2019 <- df_filtered %>% filter(year == 2019)
df_filtered_2020 <- df_filtered %>% filter(year == 2020)

# Add season column (every 3 months)
df_filtered <- df_filtered %>%
  mutate(season = cut(datetime, breaks = "3 months", labels = c("Spring", "Summer", "Fall", "Winter","Spring", "Summer", "Fall", "Winter","Spring", "Summer", "Fall", "Winter"), include.lowest = TRUE))

### Perform Grubbs' test
grubbs.test(df_filtered$nitrate_mg_per_liter)

which.max(df_filtered$nitrate_mg_per_liter)

test1 = df_filtered[56919,]
test = df_filtered[56900:57000,]

df_filtered_2020_summer <- df_filtered %>% filter(year == 2020 & season == 'Summer')

which.max(df_filtered_2020_summer$nitrate_mg_per_liter)

grubbs.test(df_filtered_2020_summer$nitrate_mg_per_liter)

### Nitrate concentration by year (facetplot example)

darkmint <- carto_pal(7, "DarkMint")

df_yearly <- df %>% filter(year %in% c(2018, 2019, 2020))
year_colors <- darkmint[2:4]

yearly_nitrate <- ggplot(df_yearly, aes(x = factor(year), y = nitrate_mg_per_liter)) +
  geom_boxplot(aes(color = factor(year))) +
  labs(y = "Nitrate (mg/L)", title = "Nitrate concentration by year") +
  facet_grid(. ~ year, scales = "free_x", space = "free_x") +
  xlab("Year") +
  scale_color_manual(values = year_colors) +
  theme(legend.position = "none")

ggsave("yearly_nitrate.png", yearly_nitrate)

### Nitrate concentration by season (one graph, four dodged boxplots)

# Convert datetime to date-time object
df_filtered$datetime <- as.POSIXct(df_filtered$datetime)

# Create seasonal boxplot
seasonal_nitrate <- ggplot(df_filtered, aes(x = season, y = nitrate_mg_per_liter, fill = as.factor(year))) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_carto_d(name = "year", palette = "TealRose") +
  labs(x = "Season", y = "Nitrate (mg/L)", title = "Nitrate concentration by season") +
  # Label the most extreme outliers with their corresponding dates
  geom_text(data = df_filtered %>% 
              group_by(season, year) %>% 
              top_n(n = 1, wt = nitrate_mg_per_liter) %>% 
              ungroup(),
            aes(x = season, y = nitrate_mg_per_liter, label = format(datetime, "%m/%d/%Y")),
            color = "black", size = 3, vjust = -1.5, hjust = 0.5, 
            position = position_dodge(width = 0.8)) +
  geom_text(data = df_filtered %>% 
              group_by(season, year) %>% 
              slice(which.min(nitrate_mg_per_liter)) %>% 
              ungroup(),
            aes(x = season, y = nitrate_mg_per_liter, label = format(datetime, "%m/%d/%Y")),
            color = "black", size = 3, vjust = 1.5, hjust = 0.5, 
            position = position_dodge(width = 0.8))

ggsave("seasonal_nitrate.png", seasonal_nitrate, width=8.84,height=5.75)

# Print out table for blog post

z_scores_table <- df_filtered_2020_summer[, c("datetime","nitrate_mg_per_liter", "nitrate_z_scores", "nitrate_modified_z_scores")]
z_scores_table <- na.omit(z_scores_table[7020:7034,])

stargazer(z_scores_table, summary=FALSE)

# This is an alternate way to output a table:
# library("grid")
# library("gtable")
# library("gridExtra")
#
# grid_table <- tableGrob(z_scores_table, theme = ttheme_minimal())
#
# png("summary_table.png", width = 8, height = 5, units = "in", res = 300)
# grid.draw(grid_table)
# dev.off()

#### Multivariate: water against nitrate

### Let's try a few different algorithms, first KNN.

## Let's visually explore the data and set up a good dataset to input into different algorithms
fall <- carto_pal(7, "Fall")

water_to_nitrate <- ggplot(df_filtered, aes(x = temp_of_water_celsius, y = nitrate_mg_per_liter)) +
  labs(x = "Water temperature (Celsius)", y = "Nitrate (mg/L)", title = "Nitrate concentration to water temperature") +
  geom_point(color=fall[1], shape=1)

ggsave("water_to_nitrate.png", water_to_nitrate, width = 5.79, height = 4.13)

algo_df <- df_filtered[, c("datetime","temp_of_water_celsius", "nitrate_mg_per_liter", "nitrate_z_scores_rank", "water_z_scores_rank", "nitrate_modified_z_scores_rank", "water_modified_z_scores_rank")]
algo_df$temp_of_water_celsius <- as.numeric(algo_df$temp_of_water_celsius)
algo_df$nitrate_mg_per_liter <- as.numeric(algo_df$nitrate_mg_per_liter)

algo_df = na.omit(algo_df)

df_for_scaling <- algo_df[, c("temp_of_water_celsius", "nitrate_mg_per_liter")]

## Let's explore KNN first

library(FNN)

knn_df_scaled <- scale(df_for_scaling)
knn_matrices <- get.knn(knn_df_scaled, 20)

head(knn_matrices$nn.dist, 3)
final_df <- algo_df

# Larger distances are more likely to be anomalous
final_df$knn_score <- rowMeans(knn_matrices$nn.dist)

# which.max(knn_score)

# plot(nitrate_mg_per_liter ~ temp_of_water_celsius, data = final_df, cex = (rowMeans(knn_matrices$nn.dist))*2, pch = 17)

knn_score_water_to_nitrate <- ggplot(final_df, aes(x = temp_of_water_celsius, y = nitrate_mg_per_liter)) +
    labs(x = "Water temperature (Celsius)", y = "Nitrate (mg/L)", title = "KNN anomaly scores") +
  geom_point(color=fall[1], shape = 17, size = (rowMeans(knn_matrices$nn.dist))*2)

ggsave("knn_score_water_to_nitrate.png", knn_score_water_to_nitrate, width = 5.79, height = 4.13)

# Print table for blog post
knn_scores_table <- final_df[, c("datetime","nitrate_mg_per_liter", "knn_score", "nitrate_z_scores_rank", "nitrate_modified_z_scores_rank")]
knn_scores_table_1 <- na.omit(knn_scores_table[17282:17286,])

stargazer(knn_scores_table_1, summary=FALSE)

# Top five knn score by distinct date
knn_scores_table$date <- as.Date(knn_scores_table$datetime) # Add a 'date' column based on the 'datetime' column

knn_scores_table_2 <- knn_scores_table %>%
  group_by(date) %>%
  arrange(desc(knn_score)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(desc(knn_score)) %>%
  slice_head(n = 5)

knn_scores_table_2 <- knn_scores_table_2 %>%
  arrange(desc(knn_score)) %>%
  head(5)

knn_scores_table_2 <- knn_scores_table_2[, c("datetime","nitrate_mg_per_liter", "knn_score", "nitrate_modified_z_scores_rank")]

stargazer(knn_scores_table_2, summary=FALSE)


## Now let's explore local outlier factor
library(dbscan)
lof_score <- lof(scale(df_for_scaling), minPts=20)

# Scores greater than one are more likely to be anomalous
# Scores less than one are less likely to be anomalous
final_df$lof_score <- lof_score

final_df$knn_score_rank <- rank(final_df$knn_score, ties.method = "min")
final_df$lof_score_rank <- rank(final_df$lof_score, ties.method = "min")

final_df$knn_percent_rank <- percent_rank(final_df$knn_score)
final_df$lof_percent_rank <- percent_rank(final_df$lof_score)

lof_score_water_to_nitrate <- ggplot(final_df, aes(x = temp_of_water_celsius, y = nitrate_mg_per_liter)) +
    labs(x = "Water temperature (Celsius)", y = "Nitrate (mg/L)", title = "LOF anomaly scores") +
  geom_point(color=fall[1], shape = 17, size = lof_score)

ggsave("lof_score_water_to_nitrate.png", lof_score_water_to_nitrate, width = 5.79, height = 4.13)

# plot(nitrate_mg_per_liter ~ temp_of_water_celsius, data = final_df, cex = lof_score)

### Isolated forests
# You'll want these commands:
# library(devtools)
# install_github("gravesee/isofor")
library(isofor)

## Isolated forest
# nt is the number of trees. phi is the sampling space
nitrate_forest <- iForest(df_for_scaling, nt = 100, phi = 100)

isoforest_score <- predict(nitrate_forest, newdata = df_for_scaling)

final_df$isofor_score_forest <-isoforest_score
final_df$isofor_score_forest_rank <- percent_rank(final_df$isofor_score_forest)

# Isolated forest scores are between 0 and 1
# scores near one indicate anomalies (small path length)

# Need to manually remove the rows with an LOF ratio of infinity
write.xlsx(final_df, file = "C:\\Users\\chris\\OneDrive\\Documents\\r_rivers 04-07-23 0702pm.xlsx")

# Visualize isolated forest score

n_seq <- seq(min(final_df$nitrate_mg_per_liter), max(final_df$nitrate_mg_per_liter), length.out = 20)
t_seq <- seq(min(final_df$temp_of_water_celsius), max(final_df$temp_of_water_celsius), length.out = 20)

isofor_grid <- expand.grid(temp_of_water_celsius = t_seq, nitrate_mg_per_liter = n_seq)

isofor_grid$score <- predict(nitrate_forest, isofor_grid)

library(lattice)

BluYl <- carto_pal(7,"BluYl")

isofor_score_plot <- contourplot(score ~ temp_of_water_celsius + nitrate_mg_per_liter,
                                 data = isofor_grid, region = TRUE,
                                 col.regions = BluYl,
                                 cuts = 6,
                                 xlab ="Water temperature (Celsius)",
                                 ylab ="Nitrate (mg/L)",
                                 main = "Isolation forest anomaly scores")

png(file = "C:\\Users\\chris\\OneDrive\\Documents\\isofor_score_plot.png",
    width=719,
height=418,
    res=75)
print(isofor_score_plot)
dev.off()

### Plot final results
final_df_plot <- final_df %>% mutate(year = as.integer(format(datetime, "%Y")))
final_df_plot <- final_df_plot %>% filter(year == 2020)

# Define the score columns and their corresponding pch and colors
score_columns <- c('nitrate_z_scores_rank', 'nitrate_modified_z_scores_rank', 'knn_score_rank', 'lof_score_rank', 'isofor_score_forest_rank')
pch_values <- c(0, 1, 3, 4, 2)
prism_colors <- carto_pal(5, "Prism")

# Create a new dataframe containing the top 20 points for each score column

top20_points_df <- data.frame()

for (i in seq_along(score_columns)) {
  top20_data <- final_df_plot[order(final_df_plot[[score_columns[i]]], decreasing = TRUE),][1:20,]
  top20_data$score_type <- score_columns[i]
  top20_data$shape <- pch_values[i]
  top20_data$color <- prism_colors[i]
  top20_points_df <- rbind(top20_points_df, top20_data)
}

# Create a new ggplot time series plot with the added legend
time_series_plot_with_all_scores_legend <- ggplot(final_df_plot, aes(x = datetime, y = nitrate_mg_per_liter)) +
  geom_line(color = "lightgrey", size = 0.5) +
  geom_point(data = top20_points_df, aes(x = datetime, y = nitrate_mg_per_liter, color = score_type, shape = score_type), size = 3) +
  scale_shape_manual(values = pch_values, name = "Score Type") +
  scale_color_manual(values = prism_colors, name = "Score Type") +
  labs(x = "Date", y = "Nitrate (mg/L)", title = "Top 20 Anomalies Score Comparison (2020 Time Series for River Nitrate)") +
  theme_minimal() +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1)) +
  scale_x_datetime(labels = scales::date_format("%b %d"), breaks = scales::pretty_breaks(n = 10))

# Print the plot
print(time_series_plot_with_all_scores_legend)

ggsave("time_series_plot_with_all_scores_legend.png", time_series_plot_with_all_scores_legend, width = 10.31, height = 4.12)


# Inspecting isolated forests
# It appears that isolated forests are clustered around May 1, but looking
# at the data reveals that what's happening is that the algorithm gave seven consecutive
# recordings the same score. The isolated forest technique also ranked several points at
# different times quite highly, but the graph isn't really showing that as I selected
# only the top 20 points for each.

# 15865	5/3/2020 5:00:00
# 15866	5/3/2020 6:00:00
# 15867	5/3/2020 7:00:00
# 15868	5/3/2020 8:00:00
# 15869	5/3/2020 9:00:00
# 15870	5/3/2020 10:00:00
# 15871	5/3/2020 11:00:00

# TODO:
# 08-13-2020 7am
# Publish final dataset
# Publish dataset with isolated forest
# Publish code onto Github
# Blog writeup
# Revisit graph for isolated forests - why?
# Why hexoganal bins?
