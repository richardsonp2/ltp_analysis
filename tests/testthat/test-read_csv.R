# test-read_csv.R

# Create a reproducible example with 10 rows
set.seed(123)  # For reproducibility

sex_vec <- c(rep("Male", 30), rep("Female", 20))
date_vec <- c(rep("25a01", 10), rep("25a02", 10), rep("25a03", 20), rep("25a05", 10))
gt_vec <- c(rep("WT", 10),rep("HET", 10),rep("WT", 20),rep("HET", 10))
id_vec <- c(rep("111999", 10), rep("111998", 10), rep("111997", 20), rep("222888", 10))
cell_num_vec <- c(rep(1, 30), rep(2, 10), rep(1,10))
time_vec <- rep(c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6), times = 5)

dummy_data <- data.frame(
  Key          = rep("LTP_DUMMYDATA", 50),
  Date         = date_vec,
  Sex          = sex_vec,
  GT           = gt_vec,
  animal_id    = id_vec,
  cell_num     = cell_num_vec,
  Time         = time_vec,
  Test         = runif(50, 0.1, 0.5),
  PPR_test     = runif(50, 2, 3),
  control      = runif(50, 0.1, 0.2),
  PPR_control  = runif(50, 3, 6),
  Rs           = runif(50, 30, 35),
  Rin          = runif(50, 45, 55),
  DC           = rnorm(50, mean = 100, sd = 30)
)


# Write dummy_data to a temporary CSV file
temp_csv <- tempfile(fileext = ".csv")
write.csv(dummy_data, temp_csv, row.names = FALSE)

# Now, write tests for the read_csv_function
test_that("read_csv_function reads the CSV and creates the correct number of columns", {
  df <- read_csv_function(temp_csv)

  # Test that the data frame now has 15 columns (14 original + animal_cell_id)
  expect_equal(ncol(df), 15)
})

test_that("animal_cell_id is correctly created", {
    df <- read_csv_function(temp_csv)

    expected_id <- paste(df$animal_id, df$cell_num, sep = "_")

    expect_equal(df$animal_cell_id, expected_id)
})
