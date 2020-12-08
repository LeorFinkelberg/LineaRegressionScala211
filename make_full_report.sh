#!/bin/bash

INPUT="reader_wo_dupl.csv"
OUTPUT="Facebook_Comments_Line_Regression_wo_dupl.html"

pandas_profiling $INPUT $OUTPUT --pool_size 2
