# Rscript .\custom-scripts\combine-pdfs.R

setwd("D:/PUCP-Files/apuntes-cursos-2023-1")

source("./custom-scripts/get-html-in-_apuntes.R")

# Get unique names of the courses
courses_names <- courses_list |>
  purrr::map(~ 
    stringr::str_split(.x, pattern = "/")[[1]][[1]]
  ) |>
  unlist() |>
  unique()

# Separate paths per course into a list
paths_per_course <- as.list(courses_names)

for (path in courses_list) {
  course <- stringr::str_split(path, pattern = "/")[[1]][[1]]
  course_index <- match(course, courses_names)

  # Add path to slides of class
  paths_per_course[[course_index]] <- 
    append(
      paths_per_course[[course_index]], 
      paste0(path, "index.pdf")
    )
}

# Combine pdfs per course
for (index in 1:length(paths_per_course)) {
  #### 
  paths_per_course[[index]] <- paths_per_course[[index]][-1]
  #### 
  tryCatch({
    qpdf::pdf_combine(
      input = paths_per_course[[index]],
      output = paste0(
        website_path, "courses-pdf/", 
        courses_names[index], ".pdf"
      )
    )
  }, error = function(e)
    print(e)
  )
}

# Copy main pdfs of courses to _site folder
R.utils::copyDirectory(
  "./courses-pdf",
  "./_site/courses-pdf",
  overwrite = TRUE
)
