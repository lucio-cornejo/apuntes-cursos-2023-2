from json import load

# Load courses' paths
courses = open("./courses.json")
courses = load(courses).keys()

# Retrieve courses' notes' path
courses_notes_path = ["./courses/" + x + "/apuntes/" for x in courses]

from os import listdir
from PyPDF2 import PdfMerger

def merge_course_pdfs(course_notes_path):
  # Retrieve course notes
  notes = [path for path in listdir(course_notes_path) if path.endswith(".pdf")]
  
  merger = PdfMerger()
  [merger.append(course_notes_path + pdf) for pdf in notes]

  # Save in apuntes folder
  merged_pdf_path = course_notes_path.replace("apuntes/", "merged.pdf")
  with open(merged_pdf_path, "wb") as new_file:
    merger.write(new_file)


for course_notes_path in courses_notes_path:
  merge_course_pdfs(course_notes_path)
  print(course_notes_path)