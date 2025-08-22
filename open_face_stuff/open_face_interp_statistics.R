library(ggplot2)

a <- c(NA,NA,NA,0,1,0,0,0,1,0,2,3,0,0,NA,NA,0,1,NA)
spline(a, n = length(a))

# What is the shape of the sampling distribution of the sample mean of correlations
# between linearly interpolated data and true data using sample_size = 1000 and window_size = 3?
sample_meanf <- function(df,num_means,sample_window_size,sample_size, extra_fit_values) {
  sample_means <- data.frame(
    col1 = rep(NA, num_means),
    col2 = rep(NA, num_means)
  )
  colnames(sample_means) <- c("gaze_angle_x", "gaze_angle_y")
  for (i in 1:num_means) {
    sampled_data_indexes <- get_full_sample(df,sample_window_size,sample_size,extra_fit_values)
    sample_data <- list()
    for (j in 1:sample_size) {
      sample_data[[j]] <- df[sampled_data_indexes[[j]]$indexes,(focused_columns_start:focused_columns_end)]
    }
    u <- linear_interpolation(sample_window_size,sample_size,extra_fit_values, sample_data)
    sample_means$gaze_angle_x[i] <- mean(u$gaze_angle_x)
    sample_means$gaze_angle_y[i] <- mean(u$gaze_angle_y)
    message(paste("Completed ", i,"th"," number in for loop"),sep="")
  }
  return(sample_means)
}
means <- sample_meanf(df,1000,3,100,3)
hist(means$gaze_angle_x, main = "Sample_size 1000", xlim = c(0.5,1))

# The shape of the distribution that mean linear correlations values are sampled from 
# is approximately normal, as long as the number of sample means is large enough and 
# the "precision" (or correlation values used to produce each sample mean) is large enough

# SO, if we assume that the true distribution of mean linear correlation values contains 
# an incredibly large number of possible sample means, then to assume normality of the statistic
# we just want a large enough sample_size to produce the sample mean 
qqnorm(means$gaze_angle_x, main = "Normal Q-Q Plot of mean gaze_angle_x linear correlation",
       xlab = "Theoretical Quantiles", ylab = "mean gaze_angle_x linear correlation Quantiles")
# Add the reference line
qqline(means$gaze_angle_x, col = "red", lwd = 2)




vec_means_gaze_x <- function() {
  vecs <- matrix(NA, nrow = 1, ncol = 12)
  colnames(vecs) <- c("3_gaze_x_linear","3_gaze_x_spline","3_gaze_x_poly","4_gaze_x_linear","4_gaze_x_spline","4_gaze_x_poly","5_gaze_x_linear","5_gaze_x_spline","5_gaze_x_poly")
  methods <- c("linear","spline","poly")
  for (i in 1:6) {
    for (j in 1:3) {
      name <- paste("montee_carlo_",i,"_gaze_x_", methods[j], sep = "")
      csv <- df_list[[i]]
      assign(name,df_list[[i]][,(c(6*(0:9)+1))])
      unlist(name)
    }
  }


  # Criterion = 3
  monte_carlo_3_gaze_x_linear <- monte_carlo_3[,(c(6*(0:9)+1))]
  names(monte_carlo_3_gaze_x_linear) <- NULL
  lin3_vec <- unlist(monte_carlo_3_gaze_x_linear)
  vecs[1] <- mean(lin3_vec)
  
  monte_carlo_3_gaze_x_spline <- monte_carlo_3[,(c(6*(0:9)+3))]
  names(monte_carlo_3_gaze_x_spline) <- NULL
  spline3_vec <- unlist(monte_carlo_3_gaze_x_spline)
  vecs[2] <- mean(spline3_vec)
  
  monte_carlo_3_gaze_x_poly <- monte_carlo_3[,(c(6*(0:9)+5))]
  names(monte_carlo_3_gaze_x_poly) <- NULL
  poly3_vec <- unlist(monte_carlo_3_gaze_x_poly)
  vecs[3] <- mean(poly3_vec)
  
  
  # Criterion = 4
  monte_carlo_4_gaze_x_linear <- monte_carlo_4[,(c(6*(0:9)+1))]
  names(monte_carlo_4_gaze_x_linear) <- NULL
  lin4_vec <- unlist(monte_carlo_4_gaze_x_linear)
  vecs[4] <- mean(lin4_vec)
  
  monte_carlo_4_gaze_x_spline <- monte_carlo_4[,(c(6*(0:9)+3))]
  names(monte_carlo_4_gaze_x_spline) <- NULL
  spline4_vec <- unlist(monte_carlo_4_gaze_x_spline)
  vecs[5] <- mean(spline4_vec)
  
  monte_carlo_4_gaze_x_poly <- monte_carlo_4[,(c(6*(0:9)+5))]
  names(monte_carlo_4_gaze_x_poly) <- NULL
  poly4_vec <- unlist(monte_carlo_4_gaze_x_poly)
  vecs[6] <- mean(poly4_vec)
  
  
  # Criterion = 5
  monte_carlo_5_gaze_x_linear <- monte_carlo_5[,(c(6*(0:9)+1))]
  names(monte_carlo_5_gaze_x_linear) <- NULL
  lin5_vec <- unlist(monte_carlo_5_gaze_x_linear)
  vecs[7] <- mean(lin5_vec)
  
  monte_carlo_5_gaze_x_spline <- monte_carlo_5[,(c(6*(0:9)+3))]
  names(monte_carlo_5_gaze_x_spline) <- NULL
  spline5_vec <- unlist(monte_carlo_5_gaze_x_spline)
  vecs[8] <- mean(spline5_vec)
  
  monte_carlo_5_gaze_x_poly <- monte_carlo_5[,(c(6*(0:9)+5))]
  names(monte_carlo_5_gaze_x_poly) <- NULL
  poly5_vec <- unlist(  monte_carlo_5_gaze_x_poly)
  vecs[9] <- mean(poly5_vec)
  
  # Criterion = 6
  monte_carlo_6_gaze_x_linear <- monte_carlo_6[,(c(6*(0:9)+1))]
  names(monte_carlo_6_gaze_x_linear) <- NULL
  lin6_vec <- unlist(monte_carlo_6_gaze_x_linear)
  vecs[10] <- mean(lin6_vec)
  
  monte_carlo_6_gaze_x_spline <- monte_carlo_6[,(c(6*(0:9)+3))]
  names(monte_carlo_6_gaze_x_spline) <- NULL
  spline6_vec <- unlist(monte_carlo_6_gaze_x_spline)
  vecs[11] <- mean(spline6_vec)
  
  monte_carlo_6_gaze_x_poly <- monte_carlo_6[,(c(6*(0:9)+5))]
  names(monte_carlo_6_gaze_x_poly) <- NULL
  poly6_vec <- unlist(  monte_carlo_6_gaze_x_poly)
  vecs[12] <- mean(poly6_vec)
  
  return(vecs)
}
vec_means_gaze_x <- vec_means_gaze_x()
  
  # Create the normal Q-Q plot
  qqnorm(lin_vec - spline_vec, main = "Normal Q-Q Plot of lin_vec - spline_vec",
       xlab = "Theoretical Quantiles", ylab = "lin_vec - spline_vec Quantiles")
  # Add the reference line
  qqline(lin_vec - spline_vec, col = "red", lwd = 2)

  # Create the normal Q-Q plot
  qqnorm(lin_vec - poly_vec, main = "Normal Q-Q Plot of lin_vec - poly_vec",
         xlab = "Theoretical Quantiles", ylab = "lin_vec - poly_vec Quantiles")
  # Add the reference line
  qqline(lin_vec - poly_vec, col = "red", lwd = 2)
  
  # Create the normal Q-Q plot
  qqnorm(spline_vec - poly_vec, main = "Normal Q-Q Plot of spline_vec - poly_vec",
         xlab = "Theoretical Quantiles", ylab = "spline_vec - poly_vec Quantiles")
  # Add the reference line
  qqline(spline_vec - poly_vec, col = "red", lwd = 2)
  
  
  

# Non normal statistical test to see if differences between correlation vectors by method are non-zero
  # check for symmetry
hist(lin_vec - spline_vec, breaks = 20)
  # check if different from 0
wilcox.test((lin_vec - spline_vec),mu = 0, alternative = "two.sided")
  # check for symmetry
hist(lin_vec - poly_vec, breaks = 20)
  # check if different from 0
wilcox.test((lin_vec - poly_vec),mu = 0, alternative = "two.sided")
  # check for symmetry
hist(spline_vec - poly_vec, breaks = 20)
  # check if different from 0
wilcox.test((spline_vec - poly_vec),mu = 0, alternative = "two.sided")


