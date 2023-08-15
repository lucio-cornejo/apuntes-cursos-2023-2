# Add path for R libraries to be recognized
# Source: https://stackoverflow.com/a/65543754
.libPaths("C:/Users/HP/Documents/R/win-library/4.1")

# Load courses' names
courses_names <- "./courses.json" |>
  jsonlite::read_json() |>
  names() |>
  as.list()

# Use first 3 letters as course alias
names(courses_names) <- courses_names |>
  purrr::map_vec(\(x) substr(x, 1, 3))

# Expected command line arguments:
# course_alias class_number
command_line_args <- commandArgs(trailingOnly = TRUE)

selected_course <- courses_names[command_line_args[1]]
selected_class <- as.numeric(command_line_args[2])

# Copy blank-notes.pdf into directory path,
# with name selected_course#selected_class.pdf .
# Such name will later be used to cut the pdf of notes
# into its respective course path.
file.copy(
  from = "./blank-notes.pdf",
  to = paste0(
    "./", selected_class, "#", selected_course, ".pdf"
  ),
  overwrite = FALSE
)
