courses <- c(
  "optimization",
  "probability-2",
  "partial-diff-eq",
  "stat-learning-1"
)

for (course in courses) {
  num_classes <- 30
  for (class in 1:num_classes) {
    folder <- paste0(
      getwd(),
      "/_apuntes/",
      course, "/",
      ifelse(class < 10,
        paste0("0", class),
        class
      )
    )
  
    if (dir.exists(folder)) { next }
    dir.create(folder)
  }
}
