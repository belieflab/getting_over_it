# Getting useful data frame from open_face1.csv downloaded from the cluster

input_path <- "~/getting_over_it/open_face1.csv"
output_path <- "~/getting_over_it/open_face1_transposed.csv"


# Read all lines as strings
lines <- readLines(input_path)

# Split each line by comma
split_lines <- strsplit(lines, ",")

# Find the max number of columns in any row
max_len <- max(sapply(split_lines, length))

# Pad each row with NA to reach max_len
padded_lines <- lapply(split_lines, function(row) {
  length(row) <- max_len  # pads with NAs
  row
})

# Combine into a matrix and transpose
matrix_data <- do.call(rbind, padded_lines)
transposed <- t(matrix_data)

# Turn into a data frame
df_transposed <- as.data.frame(transposed, stringsAsFactors = FALSE)

# Make the first row the column names
colnames(df_transposed) <- df_transposed[1, ]
df_transposed <- df_transposed[-1, ]
rownames(df_transposed) <- NULL

# Write to CSV
write.csv(df_transposed, output_path, row.names = FALSE, na = "NA")


# Histogram
numeric_values <- as.numeric(unlist(df_transposed[-1, ]))
numeric_values <- numeric_values[!is.na(numeric_values)]


hist(numeric_values[numeric_values < 30],
     breaks = 50,              # Number of bins
     main = "Frequency Distribution for Cluster Length",
     xlab = "Length of Clusters where SUCCESS = 0",
     ylab = "Frequency",
     col = "skyblue",          # Bar color
     border = "white")         # Bar border color

length(numeric_values[numeric_values < 30])
length(numeric_values[numeric_values > 30])

