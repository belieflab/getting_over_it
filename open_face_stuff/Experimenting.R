library(ggplot2)


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


ggplot(plotting, aes(x = 1:nrows, y = plotting[,1])) + geom_point() + labs(x = "index", y = "gaze_0_x")
ggplot(plotting, aes(x = 1:nrows, y = plotting[,1 + number_columns])) + geom_point() + labs(x = "index", y = "linear_gaze_0_x")
ggplot(plotting, aes(x = 1:nrows, y = plotting[,1 + number_columns*2])) + geom_point() + labs(x = "index", y = "spline_gaze_0_x")
ggplot(plotting, aes(x = 1:nrows, y = plotting[,1 + number_columns*3])) + geom_point() + labs(x = "index", y = "poly_gaze_0_x")