# Load courses' aliases
courses_names <- "./courses.json" |>
  jsonlite::read_json() |>
  names()

# Create courses' folders
courses_names |>
  purrr::map(function (course_name) {
    courses_folders <- c(
      "./apuntes/", "./ejercicios/", "./"
    )

    for (course_folder in courses_folders) {
      folder_path <- paste0(course_folder, course_name)
      if (!dir.exists(folder_path))  dir.create(folder_path)
    }
  })