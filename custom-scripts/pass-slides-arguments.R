## Arguments for this R script: ##
# course_short_name    Example: geo
# class_number_text    Example: 02
# number_of_slides     Example: 6

command_line_args <- commandArgs(trailingOnly = TRUE)

courses_alias <- list(
  "geom" = "geometry-topics",
  "func" = "functional-analysis",
  "prob" = "probability-1"
)
coursePath <- courses_alias[command_line_args[1]]

apuntes_folder <- "D:/PUCP-Files/apuntes-cursos-2023-1/_apuntes/"

# Example:
# Rscript D:\tldraw-to-reveal.js\01-summary.R "D:/PUCP-Files/apuntes-cursos-2023-1/_apuntes/probability-1/02/" 5
system(
  paste0(
    "Rscript D:/tldraw-to-reveal.js/01-summary.R ",
    apuntes_folder, coursePath, 
    "/", command_line_args[2], "/ ",
    command_line_args[3]
  )
)
