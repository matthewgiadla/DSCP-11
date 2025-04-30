#!/bin/bash

INPUT_CSV=$1
OUTPUT_CSV=$2

Rscript job.R "$INPUT_CSV" "$OUTPUT_CSV"
