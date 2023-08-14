# Get all index.html paths from apuntes* folders 
website_path <- 'D:/PUCP-Files/apuntes-cursos-2023-1/'

# Set courses of this semester
courses <- c(
    # "tesis-1",
    "prob-est-1",
    "top-geometria",
    "funcional-analisis"
)

courses_list <- vector(
  mode = 'list',
  length = length(courses)
)
names(courses_list) <- courses

# Filter only folder starting with 'apuntes'
courses <- sapply(
  courses,
  # Filter only '_apuntes*' type folders
  function (course) {
    content <- dir(paste0(website_path, course))
    content <- Filter(
      function(file) {
        grepl("apuntes-", file, fixed = TRUE) &
        # Check for non empty directory
        !identical(
          dir(paste0(
            website_path, course, "/", file
          )), 
          character(0)
        )
      },
      content
    )
    # If there is some 'apuntes*' type folder, 
    # add them to the list of courses
    if(!identical(content, character(0))) {
      courses_list[[course]] <<- paste0(
        course, "/", content, "/"
      )
    }
  }
)

courses_list <- courses_list |> unlist() |> unname()
