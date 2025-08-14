args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

# Load CSV
file_name <- read.csv(input_file)

cluster_counter <- 0
length_counter <- 0
length_vector <- c()
for (i in 2:length(file_name$success)) {
  if (file_name$success[i-1] == 0 & file_name$success[i] == 1) {
    cluster_counter <- cluster_counter + 1
  }
  if (file_name$success[i] == 0) {
    length_counter <- length_counter + 1
  } else {
    if (file_name$success[i-1] == 0) {
      length_vector <- append(length_vector, length_counter)
      length_counter <- 0
    } else {
      next
    }
  }
}

# Combine into a single row: filename, cluster count, then each length
result <- c(basename(input_file), cluster_counter, length_vector)

# Write to file
write.table(
  t(result),      # transpose: write as one row
  file = output_file,
  sep = ",",
  row.names = FALSE,
  col.names = FALSE,
  append = TRUE,
  quote = FALSE
)
