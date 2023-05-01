#!/usr/bin/env python3

import csv
import argparse

IDKEY = "'ID"

parser = argparse.ArgumentParser()
parser.add_argument("filename", help="CAPEC list in CSV form")
args = parser.parse_args()

capecs = 0

with open('CAPEC.csv', 'w') as listfile:
    with open('CAPEC_CWE.csv', 'w') as relfile:

        list_csv = csv.writer(listfile)
        list_csv.writerow(['identifier', 'description'])

        rel_csv = csv.writer(relfile)
        rel_csv.writerow(['identifier', 'wasImpactedBy_identifier'])

        with open(args.filename) as f:
            capec_reader = csv.DictReader(f)

            for row in capec_reader:
                capec_id = f'CAPEC-{row[IDKEY]}'
                capecs += 1

                list_csv.writerow([
                    capec_id,
                    row['Description'],
                    ])

                for cwe in row['Related Weaknesses'].split("::")[1:-1]:
                    cwe_id = f"CWE-{cwe}"
                    rel_csv.writerow([capec_id, cwe_id])
            
print(f"Imported {capecs} CAPECs")
