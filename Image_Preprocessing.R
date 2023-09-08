library(imager)
library(jpeg)
library(dplyr)

create_image_dataframe <- function(directory_path) {

  # Function that creates the image dataframe fromt he already preprocesed images

  # Arguments :
  # directory_path : path to the directory containing the preprocesed images (character)

  # Returns :
  # image dataframe (df) each column is a pixel and the last column is the label


  # Get the list of all the files in the directory
  files <- list.files(directory_path, full.names = TRUE)

  # Create an empty list to store the image data
  image_data <- list()

  i <- 1
  for (file in files) {
    # Read the image
    image <- readJPEG(file, native = FALSE)

    # Get the category from the file name
    category <- substr(basename(file), 5, 6)

    # Create a vector of the image and the category
    image_vector <- c(as.vector(image), category)

    # Add the vector to the list
    image_data[[i]] <- image_vector
    i <- i + 1
  }

  # Convert the list to an array
  image_df <- do.call(rbind, image_data)

  # Create column names
  num_pixels <- length(image_data[[1]]) - 1
  colnames(image_df) <- c(paste0("pixel", 1:num_pixels), "category")

  # Convert to a dataframe
  image_df <- as.data.frame(image_df)

  return(image_df)

}


get_subdirectories <- function(directory_path) {

  # Function that returns the list of subdirectories in a directory 

  # Arguments :
  # directory_path : path to the directory (character)

  # Returns :
  # list of subdirectories (list of character)

  # Create an empty list to store the subdirectories
  subdirectories <- list()

  # Get the list of all the items in the directory
  items <- list.files(directory_path, full.names = TRUE)

  # Iterate over the items and add the subdirectories to the list with the correct format
  for (item in items) {

    if (file.info(item)$isdir) {

      subdirectory_path <- gsub("\\\\", "/", item)

      subdirectories <- c(subdirectories, subdirectory_path)
    }
  }

  return(subdirectories)
}


get_files_ending_with_S <- function(directory_path) {

  # Function that returns the list of files ending with S in a directory
  # Theese are the image files facing forward

  # Arguments :
  # directory_path : path to the directory (character)

  # Returns :
  # list of files ending with S (list of character)

  # Create an empty list to store the file paths
  file_paths <- list()

  # Get the list of all the files in the directory and add the ones ending with S to the list
  files <- list.files(directory_path, full.names = TRUE)

    for (file in files) {

    if (grepl("S\\.JPG$", file)) {
      file_paths <- c(file_paths, file)
    }
  }

  return(file_paths)
}


get_file_names <- function(directory_path){

  # Function that returns the list of all the files in  the subdirectories of a directory

  # Arguments :
  # directory_path : path to the directory (character)

  # Returns :
  # list of all the files in the subdirectories of a directory (list of character)

  # Get the list of all the subdirectories
  subdirectories_list <- get_subdirectories(directory_path)

  # Create an empty list to store the file paths
  all_file_paths <- list()

  # Iterate over the subdirectories and add the file paths to the list
  for (directory_path in subdirectories_list) {

      file_paths <- get_files_ending_with_S(directory_path)
      
      all_file_paths <- c(all_file_paths, file_paths)
  }


  return(all_file_paths)
}


crop_image <- function(image_path, dest_path, x1 = 150, y1 = 275, x2 = 270, y2 = 380) {

  # Function that crops an image and saves it in a new directory

  # Arguments :
  # image_path : path to the image (character)
  # x1 : x coordinate of the top left corner of the crop (numeric)
  # y1 : y coordinate of the top left corner of the crop (numeric)
  # x2 : x coordinate of the bottom right corner of the crop (numeric)
  # y2 : y coordinate of the bottom right corner of the crop (numeric)

  # Returns :
  # nothing

  # Load the image
  img <- load.image(image_path)

  # Crop the image
  cropped_img <- imsub(img, x > x1, y > y1, x < x2, y < y2)

  # Gray scale the image
  cropped_img <- grayscale(cropped_img)

  # Resize the image as in the paper
  cropped_img <- resize(cropped_img, 34, 48)

  # Create the path to save the image
  out_pat <- paste0(dest_path, basename(image_path))
  out_pat <- gsub("JPG", "jpg", out_pat)

  # Save the image
  save.image(cropped_img, out_pat)

}

# Initialize the directory path
directory_path <- "KDEF_and_AKDEF\\KDEF"

# Initialize the new directory path
destination_path <- "KDEF_and_AKDEF\\CORRECT_IMAGES\\"

# Get the list of all the file paths
names <- get_file_names(directory_path)

# Crop all the images and save them in a new directory
for (image_path in names) {
  crop_image(image_path, destination_path)
}

# Create the image dataframe
image_dataframe <- create_image_dataframe(directory_path)

# Save the image dataframe
write.csv(image_dataframe, file = "KDEF_and_AKDEF\\image_dataframe.csv", row.names = FALSE)


