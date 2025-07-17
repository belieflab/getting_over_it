# Experimentally find what the "best" way to interpolate the open face data is

# List of methods
# What does it mean to be the best interpolation method?
# Maybe the "best" method means that over many sampled clusters of full raw data, the correlation between interpolated
# versions of that sample data and the actual data there is highest amongst the different methods

# Variables that affect the correlation between interpolated sample data and the true sample data
# 1) The interpolation method being used
# 2) The size of the chunk being removed to interpolate
# 3) The size of the window outside of the chunk being removed so that interpolation takes effect, so like how many numbers 
# does the interpolation algorithm see before getting to the NA values to interpolate

# 4) The sample size for comparing the methods
# 5) The location of the data being interpolation, theoretically the pattern that the data best follows should change based on
# what the subject is doing (looking around fast vs looking around slowly for example)

# What if the interpolation algorithm changed based on some number of observations before interpolation area begins





# Code for randomly sampling a subject file and then randomly sampling some data

# Randomly select subject data from list of subject file names
list_subjects <- c("SING1005_P1_sing_V2_mask_DG_06-29-2021_L")
df <- read.csv(paste("~/Downloads/", sample(list_subjects, 1), ".csv", sep = ""))

# Find randomly selected window to interpolate with length n, that contains no missing values
sample_window_size <- 10
starting_window_size <- 3

get_full_sample <- function(df) {
  index <- c(1:length(df$frame))
  for (i in 1:1000) {
    sample_start <- sample(index,1)
    sample_indexes <- c(sample_start:(sample_start + (sample_window_size + starting_window_size - 1)))
    # Need to fix bool here bc != returns a list of TRUE/FALSE values when the if statement might need just one
    if (!any(df$sucess[sample_indexes] == 0)) {
      return(df[sample_indexes,])
      break
    } else {
      next
    }
  }
}

# Create list of sampled data
sample_size <- 5
sample_data <- list()
for (i in 1:sample_size) {
  sample_data[[i]] <- get_full_sample(df)
}


# Create window + empty values for gaze data version
empty_sample_data <- sample_data
for (i in 1:sample_size) {
  # [, 6:13] reference the position of the columns for the gaze data
  empty_sample_data[[i]][(starting_window_size + 1):(sample_window_size + starting_window_size), 6:13] <- NA
}

# Code for applying the different methods to the empty values for the gaze data



