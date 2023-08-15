# Add path for R libraries to be recognized
# Source: https://stackoverflow.com/a/65543754
.libPaths("C:/Users/HP/Documents/R/win-library/4.1")

# Load courses' aliases
courses_names <- "./courses.json" |>
  jsonlite::read_json() |>
  names()

# Create folder for courses
courses_path <- "./courses/"
if (!dir.exists(courses_path))  dir.create(courses_path)

# Create folders for each course
courses_subfolders <- c(
  "/apuntes/", "/ejercicios/", "/libros/"
)
for (course_name in courses_names) {
  # Create course folder
  course_path <- paste0(courses_path, course_name)
  if (!dir.exists(course_path))  dir.create(course_path)
  
  # Create course's subfolders
  for (course_subfolder in courses_subfolders) {
    subfolder_path <- paste0(course_path, course_subfolder)
    if (!dir.exists(subfolder_path))  dir.create(subfolder_path)
  }
}
