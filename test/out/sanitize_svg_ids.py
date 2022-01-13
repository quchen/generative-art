#!/usr/bin/env python3

# Running Haskell’s Cairo library in parallel (as the testsuite does) leads to
# random numbers appearing in the identifiers of the elements. This script
# collects all of the nondeterministic entries, then sorts and numbers them,
# before writing it back. I hope this fixes the nondeterminism of the SVG file
# generation.

import glob
import re

str_old = str
str_new = str

def standardize_svg(filename: str):
    with open(filename, "r") as file:
        contents = file.read()
        new_contents = standardize_string(contents)
    with open(filename, "w") as file:
        file.write(new_contents)

def standardize_string(contents: str) -> str:
    strings_in_sections = all_nondeterministic_strings(contents)
    translation_table = create_translation_table(strings_in_sections)
    return apply_translation_table(translation_table, contents)

def apply_translation_table(translation_table: dict[str_old, str_new], contents: str) -> str:
    for before, after in translation_table.items():
        contents = contents.replace(before, after)
    return contents

def create_translation_table(strings_in_sections: dict[str, set[str_old]]) -> dict[str_old, str_new]:
    result = {}
    for section, strings in strings_in_sections.items():
        for i, old_str in enumerate(strings):
            result[old_str] = f'{section}-{i}'
    return result

def all_nondeterministic_strings(input: str) -> dict[str, set[str_old]]:
    return {
        "surface": set(re.findall(r'surface\d+', input)),
        "mask": set(re.findall(r'mask\d+', input))
    }

def main():
    for svg in glob.glob("docs/**/*.svg", recursive=True):
        standardize_svg(svg)
    for svg in glob.glob("out/**/*.svg", recursive=True):
        standardize_svg(svg)

if __name__ == '__main__':
    print("Sanitizing SVG output to make builds reproducible")
    main()
