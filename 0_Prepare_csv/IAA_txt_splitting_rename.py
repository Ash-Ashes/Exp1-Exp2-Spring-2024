#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 3 16:46:34 2024

@author: aislingsampson

Takes text files from Presentation, renames, creates csvs and moves the csvs to participant folder 
You will need to copy the raw file to P_work in ExpX_raw
This script will process and files and place completed in P_complete 
Collect those files under new folder named with participant number + _csv and move to Exp3_csv folder

NB Due to the number of .txt and .log files, process as individual participants (i.e. don't batch these)
"""

import os
import pandas as pd 
import shutil
import fnmatch

print(os.getcwd())

os.chdir("/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_raw/P_work")

cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#Move log files to assembly folder

##Move to final folder
    
include = "Exp3_Time"    
src_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_raw/P_work"
dst_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_txt"
for root, dirs, files in os.walk(src_dir):
    for f in files:
        if fnmatch.fnmatch(f,'*_Exp3_Time_*.txt'):
            shutil.copy(os.path.join(root,f), dst_dir)
            
#Switch to WD and Process log files

os.chdir("/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_txt")

cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#this currently works for the text files
# for files ending in .txt
for file in os.listdir():
    if file == '.DS_Store':
    # split into base name and extension
        continue
    parts = file.split('_')
    new_name = '{}'.format(parts[6]) #
    os.rename(file, new_name)

#Now rename according to R compilation file requirements

# for files ending in .txt

#asses files
cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#print(f"Before Renaming : {os.listdir(path)}")   
#text files end in txt
os.renames('Block1.txt', 'Txt_B1.txt')
os.renames('Block2.txt', 'Txt_B2.txt')
os.renames('Block3.txt', 'Txt_B3.txt')
os.renames('Block4.txt', 'Txt_B4.txt')
os.renames('Block5.txt', 'Txt_B5.txt')
os.renames('Block6.txt', 'Txt_B6.txt')
os.renames('Test.txt', 'Txt_BT.txt')
        
##open in excel and save as csv


cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#B1
df = pd.read_csv('Txt_B1.txt', sep='\t', header=None)
df.to_csv('Txt_B1.csv', header =None)

df = pd.read_csv('Txt_B1.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_B1.csv', index=False)

#B2
df = pd.read_csv('Txt_B2.txt', sep='\t', header=None)
df.to_csv('Txt_B2.csv', header =None)

df = pd.read_csv('Txt_B2.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_B2.csv', index=False)

#B3
df = pd.read_csv('Txt_B3.txt', sep='\t', header=None)
df.to_csv('Txt_B3.csv', header =None)

df = pd.read_csv('Txt_B3.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_B3.csv', index=False)

#B4
df = pd.read_csv('Txt_B4.txt', sep='\t', header=None)
df.to_csv('Txt_B4.csv', header =None)

df = pd.read_csv('Txt_B4.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_B4.csv', index=False)

#B5
df = pd.read_csv('Txt_B5.txt', sep='\t', header=None)
df.to_csv('Txt_B5.csv', header =None)

df = pd.read_csv('Txt_B5.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_B5.csv', index=False)

#B6
df = pd.read_csv('Txt_B6.txt', sep='\t', header=None)
df.to_csv('Txt_B6.csv', header =None)

df = pd.read_csv('Txt_B6.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_B6.csv', index=False)

#BT
df = pd.read_csv('Txt_BT.txt', sep='\t', header=None)
df.to_csv('Txt_BT.csv', header =None)

df = pd.read_csv('Txt_BT.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Txt_BT.csv', index=False)
## ditch the empty column - then move ot new folder 


##Move to final folder
import shutil
src_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_txt"
dst_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_complete"
for root, dirs, files in os.walk(src_dir):
    for f in files:
        if f.endswith('.csv'):
            shutil.copy(os.path.join(root,f), dst_dir)


## FIN ##
    
    