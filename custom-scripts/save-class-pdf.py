'''
Remove all but the first n pages of
the copied pdf from "blank-notes.pdf",
and copy it into specified path via pdf name.
'''

from re import search
from os import listdir

list_files = listdir()
pdf_files = list(filter(
  lambda file_name: search(".pdf$", file_name) != None, 
  list_files
))

# Let's assume there are only two pdf files in path,
# "blank-notes.pdf" and the one containing actual class notes.
course_class_pdf = list(filter(
  lambda file_name: file_name != "blank-notes.pdf", 
  pdf_files
))[0]

# Retrieve course path and class number
course_class, course_path = (
  course_class_pdf
    # Ignore pdf extension
    .split(".pdf")[0]
    # Split via separator character from create-course-class.R
    .split("#")
)

# Update pdf file name
pdf_path = f"./courses/{course_path}/apuntes/clase-{course_class}.pdf"


# Extract first n pages from course class pdf
import sys
from PyPDF2 import PdfReader, PdfWriter

reader = PdfReader(f"./{course_class_pdf}")
pages = reader.pages

# Subset from pages 1 to end (inclusive)
start = 1 
end = int(sys.argv[1])
pdf_writer = PdfWriter()
while start <= end:
  pdf_writer.add_page(pages[start - 1])
  start += 1

# Save file in specified path
with open(pdf_path,'wb') as out:
  pdf_writer.write(out)
