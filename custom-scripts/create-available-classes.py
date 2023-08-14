import os

# Retrieve classes notes
apuntesPath = "./_apuntes/"
courses = os.listdir(apuntesPath)

# Create JSON file for non empty classes
final_json = {}
for course in courses:
  final_json[course] = []

  classes = os.listdir(apuntesPath + course)
  for classText in classes:
    # Retrieve non empty course class 
    course_class = apuntesPath + course + "/" + classText
    if os.listdir(course_class):
      final_json[course].append(classText)

# Save dictionary as JSON
import json
with open('available-classes.json', 'w') as fp:
  json.dump(final_json, fp)
