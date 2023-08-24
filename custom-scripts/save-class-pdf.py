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

# Deal with class number smaller than 10
if int(course_class) < 10:
  course_class = "0" + course_class

# Update pdf file name
import sys

folder = "ejercicios" if len(sys.argv) > 2 else "apuntes"
pdf_path = f"./courses/{course_path}/{folder}/clase-{course_class}.pdf"

# Extract first n pages from course class pdf
from PyPDF2 import PdfReader, PdfWriter

reader = PdfReader(f"./{course_class_pdf}")
pages = reader.pages

# Get page's width and height
a, b = reader.pages[0].mediabox.width.as_integer_ratio()
width = a/b

a, b = reader.pages[0].mediabox.height.as_integer_ratio()
height = a/b

# Add blank page indicating class number
from io import BytesIO
from reportlab.pdfgen import canvas

packet = BytesIO()
can = canvas.Canvas(packet, pagesize = (width, height))
can.setFont("Helvetica", 100)
can.drawString(width/8, height/2, f"CLASE {course_class}")
can.save()

# Move to the beginning of the StringIO buffer
packet.seek(0)

# Add page with class number
pdf_writer = PdfWriter()
pdf_writer.add_page(PdfReader(packet).pages[0])

# Subset from pages 1 to end (inclusive)
start = 1
end = int(sys.argv[1])

while start <= end:
  pdf_writer.add_page(pages[start - 1])
  start += 1

# Save file in specified path
with open(pdf_path,'wb') as out:
  pdf_writer.write(out)
