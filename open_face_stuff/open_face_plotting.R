# Randomly select subject data from list of subject file names
# "SING1005_P1_sing_V2_mask_DG_06-29-2021_L"
library(polynom)
library(ggplot2)
library(tidyr)
library(dplyr)
list_subjects <- c("SING2008_P1_sing_V7_mask_CB_10-21-2021_L")
df <- read.csv(paste("~/Downloads/", sample(list_subjects, 1), ".csv", sep = ""))
focused_columns_start <- 12 # first column of column set to sample from
focused_columns_end <- 13 # last column of column set to sample from
number_columns <- focused_columns_end - focused_columns_start + 1


# Samples sample_size # of indexes from df without replacement
# *** writes to a dataframe that must be called sampled_data_indexes
get_full_sample <- function(df, sample_window_size, sample_size, extra_fit_values) {
  sample_window_size <<- sample_window_size 
  sample_size <<- sample_size
  extra_fit_values <<- extra_fit_values 
  
  # Create sample index list
  u <- list()
  index <- c(1:length(df$frame))
  for (i in 1:sample_size) {
    for (k in 1:1000) {
      sample_start <- sample(index,1)
      # reduce sample_window_size by 1 to begin at index and still have correct window size
      # sample window is increased by 2 * the # of extra_fit_values 
      sample_indexes <- c((sample_start - extra_fit_values):(sample_start + (sample_window_size - 1) + extra_fit_values))
      if (min(sample_indexes) < 1 || max(sample_indexes) > nrow(df)) next
      # skip current iteration if function has sampled this starting index before
      skip_iteration <- FALSE
      if (i == 1) {
      } else {
        for (m in 1:(i-1)) {
          if (sample_start == u[[m]]$indexes[extra_fit_values + 1]) {
            skip_iteration <- TRUE
            break
          }
        }
      }
      if (skip_iteration) {
        next
      }
      
      # Checks if the extra_fit rows in a column are all the same (would create standard deviation of 0)
      check_list <- c()
      pre_window <- c(sample_indexes[1:extra_fit_values])
      post_window <- c(sample_indexes[(extra_fit_values + sample_window_size + 1):(length(sample_indexes))])
      extra_window <- c(pre_window, post_window)
      for (col_index in focused_columns_start:focused_columns_end) {
        if (all(df[sample_indexes[1], col_index] == df[extra_window, col_index])) {
          check_list[col_index-(focused_columns_start - 1)] <- TRUE
        } else {
          check_list[col_index-(focused_columns_start - 1)] <- FALSE
        }
      }
      # This if statement checks if the sample contains only frames where success = 1 
      # AND if the starting value of a column equals the ending value (messes with linear interpolation)
      if (!any(df$success[sample_indexes] == 0) & !any(check_list)) {
        u[[i]] <- list(indexes = sample_indexes)
        break
      } else {
        next
      }
    }
  }
  return(u)
}
sample_data <- list()
for (i in 1:sample_size) {
  sample_data[[i]] <- df[sampled_data_indexes[[i]]$indexes,(focused_columns_start:focused_columns_end)]
}

linear_interpolation <- function(sample_window_size, sample_size, extra_fit_values, sample_data) {
  # define sample_window_size 
  
  # Create window + empty values for gaze data version
  linear_interp_sample_data <- sample_data
  for (i in 1:sample_size) {
    linear_interp_sample_data[[i]][(extra_fit_values + 1):(extra_fit_values + sample_window_size), 1:number_columns] <- NA
  }
  
  # Code for applying the different methods to the empty values for the gaze data
  
  # Linear interpolation
  linear_interp <- function(a, b, n) {
    seq <- seq(from = a, to = b, length.out = n)
    return(seq)
  }
  
  # apply linear interpolation
  for (i in 1:sample_size) {
    for (j in 1:number_columns) {
      linear_interp_sample_data[[i]][(extra_fit_values):(extra_fit_values + sample_window_size + 1), j] <- linear_interp(linear_interp_sample_data[[i]][extra_fit_values, j], linear_interp_sample_data[[i]][(extra_fit_values + sample_window_size + 1), j], sample_window_size + 2)
    }
  }
  
  # create correlation matrix
  # 8 columns for number of gaze data columns
  corr_matrix <- matrix(NA, nrow = sample_size, ncol = number_columns)
  for (i in 1:sample_size) {
    for (j in 1:number_columns) {
      corr_matrix[i,j] <- cor(sample_data[[i]][,j],linear_interp_sample_data[[i]][,j])
    }
  }
  corr_matrix <- data.frame(corr_matrix)
  colnames(corr_matrix) <- colnames(sample_data[[1]][1:number_columns])
  
  return(corr_matrix)
}

spline_interpolation <- function(sample_window_size, sample_size, extra_fit_values, sample_data) {
  
  # Create window + empty values for gaze data version
  spline_interp_sample_data <- sample_data
  for (i in 1:sample_size) {
    # Referencing the sample_window frames to turn them NA
    spline_interp_sample_data[[i]][(extra_fit_values + 1):(extra_fit_values + sample_window_size), 1:number_columns] <- NA
  }
  
  # Spline interpolate missing values
  for (i in 1:sample_size){
    for (j in 1:number_columns) {
      spline_interp_sample_data[[i]][,j] <- spline(sampled_data_indexes[[i]]$indexes, as.numeric(spline_interp_sample_data[[i]][,j]), xout = head(sampled_data_indexes[[i]]$indexes, n=1):tail(sampled_data_indexes[[i]]$indexes, n=1), method = "fmm")$y
    }
  }    
  
  # create correlation matrix
  # 8 columns for number of gaze data columns
  corr_matrix_spline <- matrix(NA, nrow = sample_size, ncol = number_columns)
  for (i in 1:sample_size) {
    for (j in 1:number_columns) {
      corr_matrix_spline[i,j] <- cor(sample_data[[i]][,j],spline_interp_sample_data[[i]][,j])
    }
  }
  corr_matrix_spline <- data.frame(corr_matrix_spline)
  colnames(corr_matrix_spline) <- colnames(sample_data[[1]][1:number_columns])
  return(corr_matrix_spline)
  
}

polynomial_interpolation <- function(sample_window_size, sample_size, extra_fit_values, sample_data) {
  
  # Create new list
  poly_interp_sample_data <- sample_data
  
  # Use a polynomial to interpolate missing values
  for (i in 1:sample_size){
    for (j in 1:number_columns) {
      # Give position vector for data points that excludes the to be interpolated values
      # and give vector of data points that excludes the to be interpolated values
      # then find the polynomial that goes through those points, evaluate the polynomial at every point in the actual column, then replace
      x <- c(1:extra_fit_values,((extra_fit_values + sample_window_size + 1):length(poly_interp_sample_data[[i]][,j])))
      y <-  c(poly_interp_sample_data[[i]][,j][1:(extra_fit_values)],poly_interp_sample_data[[i]][,j][(extra_fit_values + sample_window_size + 1):(length(poly_interp_sample_data[[i]][,j]))])
      poly_interp_sample_data[[i]][,j] <- predict(poly.calc(x,y),head(x,n=1):tail(x,n=1))
    }
  }  
  
  # create correlation matrix
  # 8 columns for number of gaze data columns
  corr_matrix_poly <- matrix(NA, nrow = sample_size, ncol = number_columns)
  for (i in 1:sample_size) {
    for (j in 1:number_columns) {
      corr_matrix_poly[i,j] <- cor(sample_data[[i]][,j],poly_interp_sample_data[[i]][,j])
    }
  }
  corr_matrix_poly <- data.frame(corr_matrix_poly)
  colnames(corr_matrix_poly) <- colnames(sample_data[[1]][1:number_columns])
  return(corr_matrix_poly)
}

# Graphing window_size to correlation graph for different interpolation methods
focused_columns_start <- 12 # first column of column set to sample from
focused_columns_end <- 13 # last column of column set to sample from
number_columns <- focused_columns_end - focused_columns_start + 1
sample_window_size_c <- c(1:10)
# in simulations, it seems that at sample_size 100 the underlying distribution 
# of the sample mean correlation is normal enough
sample_size <- 1000
extra_fit_values <- 3
for (i in 1:length(sample_window_size_c)) {
  # Get sample indexes for this sample_window_size
  sampled_data_indexes <- get_full_sample(df,sample_window_size_c[i],sample_size, extra_fit_values)
  # Create list of sample_size number of samples for this sample_window_size
  sample_data <- list()
  for (j in 1:sample_size) {
    sample_data[[j]] <- df[sampled_data_indexes[[j]]$indexes,(focused_columns_start:focused_columns_end)]
  }
  tmp <- linear_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sample_data)
  tmp2 <- spline_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sample_data)
  tmp3 <- polynomial_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sample_data)
  current_linear <- data.frame(method = "linear", window_size = sample_window_size_c[i], cor = rowMeans(tmp[,c("gaze_angle_x","gaze_angle_y")]))
  current_spline <- data.frame(method = "spline", window_size = sample_window_size_c[i], cor = rowMeans(tmp2[,c("gaze_angle_x","gaze_angle_y")]))
  current_poly <- data.frame(method = "poly", window_size = sample_window_size_c[i], cor = rowMeans(tmp3[,c("gaze_angle_x","gaze_angle_y")]))
  
  if (i == 1) {
    for_plot <- rbind(current_linear, current_spline, current_poly)
  } else {
    for_plot = rbind(for_plot,rbind(current_linear, current_spline,current_poly))
  }
}

summary_df <- for_plot %>%
  group_by(method, window_size) %>%
  summarise(
    mean_cor = mean(cor, na.rm = TRUE),
    sd_cor = sd(cor, na.rm = TRUE),
    n = n(),
    se = sd_cor / sqrt(n),
    ci_lower = mean_cor - qt(0.975, df = n - 1) * se,
    ci_upper = mean_cor + qt(0.975, df = n - 1) * se,
    .groups = 'drop'
  )


ggplot(summary_df, aes(x = window_size, y = mean_cor, colour = method, fill = method)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, colour = NA) +
  labs(
    y = "Correlation",
    x = "Window Size",
    title = "Interpolation Accuracy by Method with 95% CI"
  ) +
  theme_minimal()



# percent information lost in valid_interp_indexes 
# first graph has y axis as percent info lost and x axis is each participant, lines
# ordered by criterion
# second graph has y axis as average percent info lost and x axis is criterion
info_lost_6 <- read.csv("~/getting_over_it/open_face_stuff/info_lost_6.csv")
info_lost_5 <- read.csv("~/getting_over_it/open_face_stuff/info_lost_5.csv")
info_lost_4 <- read.csv("~/getting_over_it/open_face_stuff/info_lost_4.csv")
info_lost_3 <- read.csv("~/getting_over_it/open_face_stuff/info_lost_3.csv")
info_lost_2 <- read.csv("~/getting_over_it/open_face_stuff/info_lost_2.csv")
info_lost_1 <- read.csv("~/getting_over_it/open_face_stuff/info_lost_1.csv")

df_names <- c("info_lost_1", "info_lost_2","info_lost_3","info_lost_4","info_lost_5","info_lost_6")

vec_means <- c()
for (names in 1:length((df_names))) {
  df <- get(df_names[names])
  vec_means[names] <- mean(df$percent_lost)
}

plot(x = 1:6,y = vec_means, xlab = "criterion", ylab = "mean correlation")


# plotting zoomed in
nrows <- 2*extra_fit_values + sample_window_size
# make dataframe where the columns are the real and interpolated gaze columns for a sample
plotting <- matrix(NA, nrow = nrows, ncol = number_columns*4)
for (i in 1:number_columns) {
  plotting[,i] <- sample_data[[1]][,i]
}
for (i in 1:number_columns) {
  plotting[,(i + number_columns)] <- linear_interp_sample_data[[1]][,i]
}
for (i in 1:number_columns) {
  plotting[,(i + number_columns*2)] <- spline_interp_sample_data[[1]][,i]
}
for (i in 1:number_columns) {
  plotting[,(i + number_columns*3)] <- poly_interp_sample_data[[1]][,i]
}
linear_colnames <- paste("linear_", colnames(sample_data[[1]]), sep = "")
spline_colnames <- paste("spline_", colnames(sample_data[[1]]), sep = "")
poly_colnames <- paste("poly_", colnames(sample_data[[1]]), sep = "")
colnames(plotting) <- c(colnames(sample_data[[1]]), linear_colnames, spline_colnames, poly_colnames)


ggplot(plotting, aes(x = 1:nrows, y = plotting[,7])) + geom_point() + labs(x = "index", y = "gaze_0_x")
ggplot(plotting, aes(x = 1:nrows, y = plotting[,7 + number_columns])) + geom_point() + labs(x = "index", y = "linear_gaze_0_x")
ggplot(plotting, aes(x = 1:nrows, y = plotting[,7 + number_columns*2])) + geom_point() + labs(x = "index", y = "spline_gaze_0_x")
ggplot(plotting, aes(x = 1:nrows, y = plotting[,7 + number_columns*3])) + geom_point() + labs(x = "index", y = "poly_gaze_0_x")