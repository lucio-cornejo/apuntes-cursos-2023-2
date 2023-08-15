# Add path for R libraries to be recognized
# Source: https://stackoverflow.com/a/65543754
.libPaths("C:/Users/HP/Documents/R/win-library/4.1")

# Load courses' paths
courses_names <- "./courses.json" |>
  jsonlite::read_json() |>
  names()

courses_paths <- paste0("./courses/", courses_names, "/apuntes/")

# Separate paths per course into a list
for (course_path in courses_paths) {
  classes_pdfs <- list.files(course_path)
  if (length(classes_pdfs) > 0) {
    classes_pdfs_paths <- paste0(course_path, classes_pdfs) 
    tryCatch({
      qpdf::pdf_combine(
        input = classes_pdfs_paths,
        output = sub(
          "apuntes/", "merged.pdf", course_path
        )
      )
    }, error = \(e) print(e)
    )
  }
}
