# this script should be run in a cluster RStudio session

# csvs 
file_names <- list.files(path = "/gpfs/gibbs/project/corlett/shared/sing/processed/", pattern = "\\.csv$", full.names = TRUE)

#CURRENT PROBLEM:
# Error in u[[m]] : subscript out of bounds
#Calls: get_full_sample
#Execution halted

library(polynom)
library(ggplot2)
library(tidyr)
library(dplyr)

# Functions
get_full_sample <- function(df, sample_window_size, sample_size, extra_fit_values) {
  
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

# Parameters
focused_columns_start <- 12 # first column of column set to sample from
focused_columns_end <- 13 # last column of column set to sample from
number_columns <- focused_columns_end - focused_columns_start + 1
sample_window_size_c <- c(1:7)
sample_size <- 1000 # in simulations, it seems that at sample_size 100 the underlying distribution of the sample mean correlation is normal
extra_fit_values <- 3
n_sample_means <- 1 # number of sample means computed (each being the mean of sample_size number of sampled correlations)

# Building up the giant mega data frame to run ggplot on
# this data frame will have (3 methods)*(7 windows)*(1000 samples)*(189 participants) = 3969000
# and 5 columns: participant ID, method, window_size, correlation x, correlation y

for (i in 1:length(sample_window_size_c)) {
  sampled_data_indexes <- get_full_sample(df,sample_window_size_c[i],sample_size, extra_fit_values)
  sample_data <- list()
  for (j in 1:sample_size) {
    sample_data[[j]] <- df[sampled_data_indexes[[j]]$indexes,(focused_columns_start:focused_columns_end)]
  }
  tmp_l <- linear_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sample_data)
  tmp_s <- spline_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sample_data)
  tmp_p <- polynomial_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sample_data)
  current_linear <- data.frame(p = file_name, method = "linear", window_size = sample_window_size_c[i], cor_x = tmp_l[,"gaze_angle_x"], cor_y = tmp_l[,"gaze_angle_y"])
  current_spline <- data.frame(p = file_name, method = "spline", window_size = sample_window_size_c[i], cor_x = tmp_s[,"gaze_angle_x"], cor_y = tmp_s[,"gaze_angle_y"])
  current_poly <- data.frame(p = file_name, method = "poly", window_size = sample_window_size_c[i], cor_x = tmp_p[,"gaze_angle_x"], cor_y = tmp_p[,"gaze_angle_y"])
  
  if (i == 1) {
    for_plot <- rbind(current_linear, current_spline, current_poly)
  } else {
    for_plot = rbind(for_plot,rbind(current_linear, current_spline,current_poly))
  }
}

# after the above loop, df of input_csv will yield a for_plot dataframe of 21000 rows and 4 columns
# We can keep adding these dataframes together to one big data frame by storing a csv on the cluster

# Then we need to have a script which runs the summary and ggplot on the big data frame to complete



