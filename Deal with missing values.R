# Read data
imdbData <- read.csv("../Data/IMDB Data.csv", header = T)

# Get libraries needed
require(naniar)
require(raster)
require(simputation)

# Search for hidden missing values (not coded as NA)
# Remove leading and trailing spaces
trim(imdbData)
### But how to remove spaces inside characters?(eg."South Korea")
### The reason I want to remove spaces is that when I search hidden missing values,
### the spaces in characters will be recognized as missings

hidden_miss1 <- imdbData %>%
  miss_scan_count(search = list("N/A", "N/a")) # No result
hidden_miss2 <- imdbData %>%
  miss_scan_count(search = list("missing"))# Check "plot_keywords", they are not missings
hidden_miss3 <- imdbData %>%
  miss_scan_count(search = " ") # Many missing values

# Replace them with NA 
replace_with_na_all(imdbData, ~.x == " ")

# Look the number and proportion of missing values in total
n_miss(imdbData) 
prop_miss(imdbData) 

# More details on each variable and observation
miss_each_variable <- miss_var_summary(imdbData)
miss_each_observation <- miss_case_summary(imdbData)

miss_var_table(imdbData)
miss_case_table(imdbData)


# Visualise missing values
# Get a bird's eye view of missing values to see if any pattern
vis_miss(imdbData, cluster = TRUE)

# Look at missing values in variables and observations
gg_miss_var(imdbData)
gg_miss_case(imdbData)

# See common combinations of missings
gg_miss_upset(imdbData)



### Might do some stuff about missings of one variable across another variable
### to see if there is significant difference? But the workload is too 
### heavy if do this to each pair. I am not sure which variables to choose

# For example
imdbData %>%
  bind_shadow() %>%
  group_by(budget_NA) %>%
  summarise(mean = mean(movie_facebook_likes))# Big difference
                                              # budget_NA  mean
                                              # 1 !NA       8060.
                                              # 2  NA       2583.
# Visualise using boxplot
imdbData %>%
  bind_shadow() %>%
  ggplot(aes(x = budget_NA, y = movie_facebook_likes)) +
  geom_boxplot() +
  scale_y_log10()

# Or might visualise missings acorss two variables.
# For example
imdbData %>%
  bind_shadow() %>%
  ggplot(aes(x = imdb_score, y = movie_facebook_likes, color = budget_NA)) +
  geom_point()
  

### Maybe we can do imputation using linear model with impute_lm function
### but we need to decide the linear model first
### For example: budget_imp_lm <- imdbData %>%
###                                 bind_shadow() %>%
###                                 add_label_shadow %>%
###                                 impute_lm(budget ~       )%>%
###                                 impute_lm(gross ~        )

### Track missings using ggplot

### Evaluate imputations and models










