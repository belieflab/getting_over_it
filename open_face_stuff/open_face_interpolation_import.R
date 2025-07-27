args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("No input file supplied")

input_csv <- args[1]
df <- read.csv(input_csv)

# type: "sbatch --array=0-199 run_interpolation_all.sh" in cluster to run
# Extract filename without extension
filename <- tools::file_path_sans_ext(basename(input_csv))
# output path needs MODIFICATION
output_png_path <- "~/project/open_face_analysis/plots"

# Randomly select subject data from list of subject file names
library(ggplot2)
library(polynom)
focused_columns_start <- 6 # first column of column set to sample from
focused_columns_end <- 13 # last column of column set to sample from
number_columns <- focused_columns_end - focused_columns_start + 1

# Samples sample_size # of indexes from df without replacement to sampled_data_indexes
get_full_sample <- function(df, sample_window_size, sample_size, extra_fit_values) {
  
  # Create sample index list
  u <- list()
  index <- c(1:length(df$frame))
  for (i in 1:sample_size) {
    for (k in 1:1000) {
      sample_start <- sample(index,1)
      # reduce sample_window_size by 1 to begin at index and still have correct window size
      # sample window is increased by 2 * the # of extra_fit_values for polynomial interpolation
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
      if (!any(df$sucess[sample_indexes] == 0) & !any(check_list)) {
        u[[i]] <- list(indexes = sample_indexes)
        break
      } else {
        next
      }
    }
  }
  return(u)
}

# Function which returns correlations between sampled data (n = sample_size) 
# in chunks of size "sample_window_size" and their linear interpolations
linear_interpolation <- function(sample_window_size, sample_size, extra_fit_values,  sampled_data_indexes) {
  
  # Create list of sample_size number of samples
  sample_data <- list()
  for (i in 1:sample_size) {
    sample_data[[i]] <- df[sampled_data_indexes[[i]]$indexes,(focused_columns_start:focused_columns_end)]
  }
  
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
      corr_matrix[i,j - 5] <- cor(sample_data[[i]][,j],linear_interp_sample_data[[i]][,j])
    }
  }
  corr_matrix <- data.frame(corr_matrix)
  colnames(corr_matrix) <- colnames(sample_data[[1]][1:number_columns])
  
  return(corr_matrix)
}

# Function which returns correlations between sampled data (n = sample_size)
# in chunks of size "sample_window_size" and their spline interpolations
# extra_fit_values gives the number of data points to help with interpolation algorithm outside the interpolation window on each side
spline_interpolation <- function(sample_window_size, sample_size, extra_fit_values,  sampled_data_indexes) {
  
  # Create list of sample_size number of samples
  sample_data <- list()
  for (i in 1:sample_size) {
    sample_data[[i]] <- df[sampled_data_indexes[[i]]$indexes,(focused_columns_start:focused_columns_end)]
  }
  
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
  corr_matrix_spline <- matrix(NA, nrow = sample_size, ncol = 8)
  for (i in 1:sample_size) {
    for (j in 1:number_columns) {
      corr_matrix_spline[i,j - 5] <- cor(sample_data[[i]][,j],spline_interp_sample_data[[i]][,j])
    }
  }
  corr_matrix_spline <- data.frame(corr_matrix_spline)
  colnames(corr_matrix_spline) <- colnames(sample_data[[1]][1:number_columns])
  return(corr_matrix_spline)
  
}

# Function which returns correlations between sampled data (n = sample_size)
# in chunks of size "sample_window_size" and their polynomial interpolations
polynomial_interpolation <- function(sample_window_size, sample_size, extra_fit_values,  sampled_data_indexes) {
  
  # Create list of sample_size number of samples
  sample_data <- list()
  for (i in 1:sample_size) {
    sample_data[[i]] <- df[sampled_data_indexes[[i]]$indexes,(focused_columns_start:focused_columns_end)]
  }
  
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
  corr_matrix_poly <- matrix(NA, nrow = sample_size, ncol = 8)
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
sample_window_size_c <- c(1:20)
sample_size <- 1000
extra_fit_values <- 3
for (i in 1:length(sample_window_size_c)) {
  sampled_data_indexes <- get_full_sample(df,sample_window_size_c[i],sample_size, extra_fit_values)
  tmp <- linear_interpolation(sample_window_size_c[i], sample_size, extra_fit_values,  sampled_data_indexes)
  tmp2 <- spline_interpolation(sample_window_size_c[i], sample_size, extra_fit_values,  sampled_data_indexes)
  tmp3 <- polynomial_interpolation(sample_window_size_c[i], sample_size, extra_fit_values, sampled_data_indexes)
  current_linear <- data.frame(method = "linear", window_size = sample_window_size_c[i], cor = rowMeans(tmp[,c("gaze_angle_x","gaze_angle_y")]))
  current_spline <- data.frame(method = "spline", window_size = sample_window_size_c[i], cor = rowMeans(tmp2[,c("gaze_angle_x","gaze_angle_y")]))
  current_poly <- data.frame(method = "poly", window_size = sample_window_size_c[i], cor = rowMeans(tmp3[,c("gaze_angle_x","gaze_angle_y")]))
  
  if (i == 1) {
    for_plot <- rbind(current_linear, current_spline, current_poly)
  } else {
    for_plot = rbind(for_plot,rbind(current_linear, current_spline,current_poly))
  }
}

p <- ggplot(for_plot, aes(x = window_size, y = cor, colour = method)) + stat_summary(geom = "line")
ggsave(filename = paste0(filename, "_interpolation_plot.png"),plot = p, path = output_png_path)
