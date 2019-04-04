"""
	This script extracts the tokens from OCR output, and creates a ground truth label
	for each token.
"""

import os
import pandas as pd

tesseract_files = os.listdir('../data/tesseract')
gtruth_files = os.listdir('../data/ground_truth')


# truth_dic is a dictionary that stores all tokens appeared in the ground truth files
truth_dic = {}
# read in all the ground truth files into a dictionary
for file_name in gtruth_files:
    path = '../data/ground_truth/'+file_name
    with open(path) as ground_file:
        ground_raw = ground_file.read()  # a string
        tokens = ground_raw.split()
        for tok in tokens:
            if tok in truth_dic:
                truth_dic.update({tok: truth_dic[tok]+1})
            else:
                truth_dic.update({tok: 1})


# For each token in the OCR output, label it as 1 if it's in the truth_dic, otherwise, label it as 0
labels = []
ocr_output = []
for file_name in tesseract_files:
    path = '../data/tesseract/'+file_name
    with open(path) as ocr_file:
        ocr_raw = ocr_file.read()  # a string
        tokens = ocr_raw.split()
        for tok in tokens:
            ocr_output.append(tok)
            if tok in truth_dic:
                labels.append(1)
            else:
                labels.append(0)               


df = pd.DataFrame(data={"tokens": ocr_output, "labels": labels})
df.to_csv("./ocr_output.csv", sep=',',index=False)







                