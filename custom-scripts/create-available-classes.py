from os import listdir

# Retrieve classes notes
coursesPath = "./courses/"
coursesPaths = listdir(coursesPath)
coursesPaths

# Create JSON file for non empty classes
final_json = {}
for course in coursesPaths:
  # Assume the /apuntes/ folder only 
  # contains pdfs, and those are of the 
  # form "clase-number.pdf" .
  final_json[course] = listdir(coursesPath + course + "/apuntes")

  if final_json[course]:
    # Retrieve classes' numbers
    temp = final_json[course]
    final_json[course] = [pdfName[6:8] for pdfName in temp]

# Save dictionary as JSON
import json
with open('available-classes.json', 'w') as fp:
  json.dump(final_json, fp)
