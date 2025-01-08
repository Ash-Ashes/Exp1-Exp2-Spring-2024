#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 15:00:18 2024

@author: aislingsampson
"""

import os
import pandas as pd 
import shutil

print(os.getcwd())

os.chdir("/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_raw/P_work")

cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#Move log files to assembly folder

##Move to final folder

src_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_raw/P_work"
dst_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_log"
for root, dirs, files in os.walk(src_dir):
    for f in files:
        if f.endswith('.log'):
            shutil.copy(os.path.join(root,f), dst_dir)
            
#Switch to WD and Process log files

os.chdir("/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_log")

cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#for files ending in .log
for file in os.listdir():
    if file == '.DS_Store':
    # split into base name and extension - keep the Block only - pos 2
        continue
    parts = file.split('_')
    new_name = '{}'.format(parts[2]) #
    os.rename(file, new_name)


##log file now get at Block name    
for file in os.listdir():
    if file == '.DS_Store':
    # split into base name and extension - keeping only teh block identifier and the extension
        continue
    parts = file.split('-')
    new_name = '{}'.format(parts[0]) #
    os.rename(file, new_name)  

##log file add extension back
for file in os.listdir():
    if file == '.DS_Store':
    # split into base name and extension
        continue
    # rename is the file getting renamed, pre is the part of file name before extension and ext is current extension
    new_extension = '.log'
    pre, ext = os.path.splitext(file)
    os.rename(file, pre + new_extension)

##Now ready to tanslate to excel 
cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))

#print(f"Before Renaming : {os.listdir(path)}")   
#text files end in txt
os.renames('Block1.log', 'Pz_B1.log')
os.renames('Block2.log', 'Pz_B2.log')
os.renames('Block3.log', 'Pz_B3.log')
os.renames('Block4.log', 'Pz_B4.log')
os.renames('Block5.log', 'Pz_B5.log')
os.renames('Block6.log', 'Pz_B6.log')
os.renames('Test.log', 'PZ_BT.log')
        
##open in excel and save as csv


cwd = os.getcwd()
files =os.listdir(cwd)
print("Files in %r: %s" % (cwd,files))
##df = pd.read_csv('test_fwf.log', skiprows=7, sep='(?:\]\s+\[)', engine = 'python', names=['timestamp', 'code', 'message'])
#B1
df = pd.read_csv('Pz_B1.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_B1.csv', header=None)

df = pd.read_csv('Pz_B1.csv')
#delete first colum 
df.drop(columns=df.columns[0], axis=1, inplace=True)
df.to_csv('Pz_B1.csv', index=False)

#B2
df = pd.read_csv('Pz_B2.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_B2.csv', header =None)

df = pd.read_csv('Pz_B2.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Pz_B2.csv', index=False)

#B3
df = pd.read_csv('Pz_B3.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_B3.csv', header =None)

df = pd.read_csv('Pz_B3.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Pz_B3.csv', index=False)

#B4
df = pd.read_csv('Pz_B4.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_B4.csv', header =None)

df = pd.read_csv('Pz_B4.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Pz_B4.csv', index=False)

#B5
df = pd.read_csv('Pz_B5.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_B5.csv', header =None)

df = pd.read_csv('Pz_B5.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Pz_B5.csv', index=False)

#B6
df = pd.read_csv('Pz_B6.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_B6.csv', header =None)

df = pd.read_csv('Pz_B6.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Pz_B6.csv', index=False)

#BT
df = pd.read_csv('Pz_BT.log', skiprows=3, sep='\t', engine = 'python', names = ['Trial','Event', 'Type', 'Code', 'Time', 'TTime', 'Uncertainty', 'Duration','z', 'ReqTime', 'ReqDur', 'Stim Type', 'Pair Index'])
df.to_csv('Pz_BT.csv', header =None)

df = pd.read_csv('Pz_BT.csv')
df.drop(columns=df.columns[0], axis=1, inplace=True)
# Delete first
df.to_csv('Pz_BT.csv', index=False)
## ditch the empty column - then move ot new folder 


##Move to final folder
import shutil
src_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_log"
dst_dir = "/Users/aislingsampson/Documents/Documents/01 - UofT/Experiment Data/Exp3_Raw_to_excel/Exp3_csv/P_complete"
for root, dirs, files in os.walk(src_dir):
    for f in files:
        if f.endswith('.csv'):
            shutil.copy(os.path.join(root,f), dst_dir)



