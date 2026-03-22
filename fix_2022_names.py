import re

with open("code/1_1d_applicant_clean.R", "r") as f:
    content = f.read()

# Fix 2022: switch applicant_first/last_name to personal_information_first/last_name
# in the select() block
content = content.replace(
    "applicant_first_name,\n    applicant_last_name,",
    "personal_information_first_name,\n    personal_information_last_name,"
)

# Fix 2022 rename block
content = content.replace(
    "first_name = applicant_first_name,\n    last_name  = applicant_last_name,",
    "first_name = personal_information_first_name,\n    last_name  = personal_information_last_name,"
)

with open("code/1_1d_applicant_clean.R", "w") as f:
    f.write(content)

# Verify all name references
lines = content.split('\n')
for i, line in enumerate(lines):
    if any(x in line for x in [
        'personal_information_first', 'personal_information_last',
        'applicant_first_name', 'applicant_last_name'
    ]):
        print(f"Line {i+1}: {line.strip()}")
