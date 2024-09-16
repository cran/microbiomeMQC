#' Calculate 4 key reporting measures
#'
#' Calculate the 4 key reporting measures for the WHO International Reference Reagents for the microbiome
#'
#' @param input Path to the .xlsx or .csv file.e.g. "C:\\Users\\joeblogs\\Desktop\\data.xlsx"
#' @param taxonomic_level The taxonomic level ("strain", "species", or "genus") that you are using.
#' @param output Path to save the output CSV file (file path must end with the file name e.g. "C:\\Users\\joeblogs\\Desktop\\MQC.csv"
#' @return No return value. The function writes the results to the specified CSV file.
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom utils tail
#' @importFrom readxl read_excel
#' @importFrom vegan vegdist
#' @details
#' The 4 key reporting measures (using the example of species) are:
#'
#' Sensitivity: How many species from the reagent are correctly identified.
#'
#' Diversity: The total number of species detected.
#'
#' FPRA: The relative abundance of false positives.
#'
#' Similarity: The Bray-Curtis dissimilarity in composition between the species profile reported and the 'ground truth' profile of the reagent.
#'
#' To see how the data should be formatted do: data(MQC_testdata)
#' FP = false positive i.e. those bacteria that do not appear in the reagent
#'
#' @export
#' @examples
#' input <- system.file("extdata", "MQC_testdata.csv", package = "microbiomeMQC")
#' output <- tempfile(fileext = ".csv")
#' microbiomeMQC(input, "species", output)
microbiomeMQC <- function(input, taxonomic_level, output) {

  # Read data file
  if (grepl("\\.xlsx$", input, ignore.case = TRUE)) {
    input_file <- readxl::read_excel(input)
  } else if (grepl("\\.csv$", input, ignore.case = TRUE)) {
    input_file <- read.csv(input)
  } else {
    stop("Unsupported file format. Only Excel (.xlsx) or CSV (.csv) files are supported.")
  }

  # Ensure the first column is species names and excluded from numeric conversion
  species_names <- input_file[, 1]
  numeric_data <- input_file[, -1]

  # Convert all columns to numeric, handling non-numeric values
  numeric_data <- as.data.frame(lapply(numeric_data, function(x) as.numeric(as.character(x))))

  # Check for NA values introduced during conversion
  if (any(is.na(numeric_data))) {
    na_columns <- colnames(numeric_data)[colSums(is.na(numeric_data)) > 0]
    stop(paste("NA values introduced during numeric conversion in columns:", paste(na_columns, collapse=", ")))
  }

  # Data preprocessing
  t_dataset <- t(numeric_data)

  # Compute Bray Curtis dissimilarity indices
  bc_CS <- vegan::vegdist(t_dataset, method = "bray")

  # Load Bray Curtis dissimilarity matrix from an excel file
  similarity_matrix <- as.matrix(bc_CS)

  # Calculate similarity of all columns relative to the first column
  Similarity <- numeric()
  for (i in 1:ncol(similarity_matrix)) {
    similarity <- (1 - similarity_matrix[i, 1]) * 100
    similarity <- round(similarity)
    Similarity <- c(Similarity, similarity)
  }

  # Print the vector of similarity values
  Similarity

  # Casting- convert table to dataframe
  total_species <- colSums(numeric_data != 0)

  # Split dataframe's every column into individual datasets
  split_df <- lapply(1:(ncol(numeric_data)), function(x) numeric_data[, x])

  # Determine total species present based on taxonomic level
  if (taxonomic_level == "strain") {
    total_species_present <- 20
  } else if (taxonomic_level == "species") {
    total_species_present <- 19
  } else if (taxonomic_level == "genus") {
    total_species_present <- 16
  } else {
    stop("Invalid taxonomic level. Choose from 'strain', 'species', or 'genus'.")
  }

  results <- matrix(nrow = length(split_df), ncol = 3)
  for (i in seq_along(split_df)) {
    # Total species count remains the same
    species_identified <- sum(split_df[[i]][1:total_species_present] != 0)

    # Sensitivity and diversity calculations remain unchanged
    sensitivity <- round((species_identified / total_species_present) * 100, 2)
    diversity <- sum(split_df[[i]] != 0)
    total_abundance <- sum(split_df[[i]])

    # False positive abundance is the sum of values beyond the total_species_present index
    false_positive_abundance <- if (length(split_df[[i]]) > total_species_present) {
      sum(split_df[[i]][(total_species_present + 1):length(split_df[[i]])])
    } else {
      0  # No false positives if there are no additional species
    }

    # Calculate FPRA
    fpra <- if (total_abundance > 0) {
      (false_positive_abundance / total_abundance) * 100
    } else {
      0
    }

    results[i, ] <- c(sensitivity, diversity, fpra)
  }

  row_names <- colnames(numeric_data)
  my_table <- cbind(sample = row_names, Sensitivity = results[,1], Diversity = results[,2], FPRA = results[,3], Similarity)

  # Convert to data frame
  my_table_df <- as.data.frame(my_table)

  # Write output to CSV file
  write.csv(my_table_df, file = output, row.names = FALSE)

  # Print message
  message("Output saved as", output, "\n")
}


