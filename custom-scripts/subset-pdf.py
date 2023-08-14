'''
---- INTENDED USE ----
  Write in the file "blank-notes copy.pdf"
  and then run this code, from the project's path,
  in order to subset such pdf.

  After the subset process, remove the
  "blank-notes copy.pdf" file and copy
  the template file "blank-notes.pdf".
'''

# Example code to execute the npm command
# for this file from the terminal:
# npm run pdfSubset -- func tarea-2 4

import sys
from PyPDF2 import PdfReader, PdfWriter

courses = {
  "func": "functional-analysis",
  "geom": "geometry-topics",
  "prob": "probability-1"
}

course = "./" + courses[sys.argv[1]] + "/"
file_name = sys.argv[2] + ".pdf"

reader = PdfReader("./blank-notes copy.pdf")
pages = reader.pages

# Subset from pages 1 to end (inclusive)
start = 1 
end = int(sys.argv[3] )
pdf_writer = PdfWriter()
while start <= end:
  pdf_writer.add_page(pages[start - 1])
  start += 1

# Save file in specified path
with open(course + file_name,'wb') as out:
  pdf_writer.write(out)
