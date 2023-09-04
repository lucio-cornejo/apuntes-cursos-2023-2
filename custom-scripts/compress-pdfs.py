from os import walk, system, remove, rename
from os.path import getsize

final_json = {}
main_pdfs = ("clase-", "merged")

final_json.update(
  {
    subdir: list(filter(
      lambda file_name: file_name.startswith(main_pdfs) and file_name.endswith(".pdf"), files
      )) 
    for subdir, dirs, files in walk("courses")
  }
)

# Keep count of pdf file sizes
def b_to_mb(size):
  return round(size * (10**-6), 2)

old_total_file_sizes = 0
compressed_total_file_sizes = 0

# Remove entries where no main pdfs where found
sub_folders = list(final_json.keys())

for sub_folder in sub_folders:
  if len(final_json[sub_folder]) == 0:
    del final_json[sub_folder]

# Compress pdfs
folders = tuple(final_json.keys())
for paths in folders:
  for pdf_name in final_json[paths]:
    pdf_path = paths + '\\' + pdf_name
    new_pdf_path = pdf_path.replace("pdf", "compressed.pdf")

    # Update old pdf file sizes
    old_total_file_sizes += getsize(pdf_path)

    # Compress pdf using Ghostcript
    system(
      'gswin64c -sDEVICE=pdfwrite -dCompatibilityLevel="1.0" ' +
      '-dPDFSETTINGS=/default -r200 -dPrinted=false -dNOPAUSE -dQUIET -dBATCH ' +
      f'-sOutputFile="{new_pdf_path}" "{pdf_path}"'
    )
    # Replace old pdf with compressed one
    print(
      "Old: ", b_to_mb(getsize(pdf_path)), "MB ",
      "Compressed: ", b_to_mb(getsize(new_pdf_path)) , "MB")
    remove(pdf_path)
    rename(new_pdf_path, pdf_path)

    # Update compressed pdf file sizes
    compressed_total_file_sizes += getsize(pdf_path)

print(b_to_mb(old_total_file_sizes), "MB")
print(b_to_mb(compressed_total_file_sizes), "MB")
