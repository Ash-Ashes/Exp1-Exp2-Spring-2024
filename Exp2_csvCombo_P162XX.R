#------------------#
  #title: "16x16_CSV_Cleaner_Script_Exp2_Dual"
  #author: "A Sampson"
  #date: "`Script as of 04292024"
  #note: Cleaning for Presentation created log files (txt formats) for the 20 run 16 x 6 stimuli prepared in July and Collected in August.
  #version information: There are 4 versions - the only difference in the four versions is which stimuli are assigned to which condition.
  
#---------------------#
  #-----  Stimuli - Irrespective of Version -----

#`1` = "TRAIN",
#`2` = "B2_WHISPERINGVOICES",
#`3` = "PIGS",
#`4` = "RIVER",
#`5` = "SUMMERNIGHT",
#`6` = "B2_SANDPAPER",
#`7` = "B2_FLIPPINGBOOKPAGES",
#`8` =	"ELECTRICITY",
#`9` = "FROGPOND",
#`10`= "HAIRDRYING",
#`11`= "TRIBALCHANT",
#`12`= "HIGHWAY",
#`13`= 'THROWINGGLASS',
#`14`= 'BUBBLING',
#`15`= 'ORCHESTRATUNING',
#`16`= "GEESE",
#test stimuli
#`17`= "CLATTEROFHOOVES",
#`18`= "B2_CAROUSEL",
#`19`= "APPLAUSE",
#`20`= "LIGHTNING",
#`21`= "BASKETBALL",
#`22`= "ARCADES",
#`23`= "EERIEFOREST",
#`24`= "BALLDRUMMING"

#Practice
#1 = Wolves
#2 = Cats
#3 = Cowsmooing
#4 = Leaves
#5 = MEDIEVALBATTLE.wav
#6 = Christianprayer
#7 = snakes
#8 = glassbreaking
#9 = fireburning
#10 = Planeoverhead
#11 = WALKINGINWOODS
#12 = Chimes


##------ Load Libraries -----

library(tidyverse)
library(skimr)
library(ggplot2)
library(gt)
library(gtsummary)
library(apaTables)
library(plotrix)

#Read in the csvs - for this pilot incarnation there are 2 csv taken from the 14 output files from Presentation - the csv designated "16B" contains the exposure blocks data, the csv designated "16T" contains the test block data

##----- Set participant and Version -----
Subject = "P16208_052824"
Version = "Four"

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datB1 <-read_csv('Pz_B1.csv', skip = 3)
as.numeric(datB1$Trial)
#remove garbage rows
#dat_B1 <- datB1 [-c(326:nrow(datB1)), ]
dat_B1 <- datB1  %>% 
  filter(Trial %in% (1:130) ) #Ok so this is replacing the concat up to row blah version
#The above reduces failure point -> all blocks should be consistent in number of trials
         
#  rownames_to_column() - not here - in the next script for sure though
dat_B1_1 <-read_csv('Txt_B1.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_B1 <- dat_B1 %>% 
  rename("Event_Type" = "Event Type")
#no idea why this one is different - but hoo boy cannot get the classes to become harmonized
#print(sapply(transform(my_dataframe,column1 = as.numeric(column1)), class)) 
sapply(transform(dat_B1,Time = as.integer(Time)), class) 
dat_B1 <- dat_B1%>% mutate(Time = as.numeric(Time))
dat_B1 <- dat_B1%>% mutate(TTime = as.numeric(TTime))

#dumpcolumns we don't need 

#different number of decimals - fix this in the presentation programming at some point
dat_B1_1 <- dat_B1_1 %>% 
  mutate( Time = Time*10)

dat_B1 <- dat_B1 %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_B1)

#bind rows by time

detect1 <- bind_rows(dat_B1, dat_B1_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detect1 <- arrange(detect1, Time)
print(detect1, n=20)

detect1 <- detect1 %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

detect1<-detect1 %>% 
  fill(Trial, .direction = 'down') #Holy moly - that worked ;)

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first

detect1 <- detect1 %>% 
  group_by(Trial) %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown') %>% 
  ungroup()

#Now fill information from TrialNum FileName  Cond Location Onset - both up and down

detect1 <-detect1 %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'down')
class(detect1)

#All good to here - now the block, the repeats and the separation?
detect1 <- detect1 %>% 
#  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_1', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detect1 <- arrange(detect1,TrialNum)
print(detect1)

#do this later detect1 <- detect1 %>% 
#  mutate(Rep = case_when(
#    TrialNum <= 16 ~ 'Rep.1',
#    TrialNum >= 17 &  TrialNum<= 32 ~ 'Rep.2',
#    TrialNum >= 33 &  TrialNum<= 48 ~ 'Rep.3',
#    TrialNum >= 49 &  TrialNum<= 64 ~ 'Rep.4'
#  ))

#now fill the event type and code 
detect1 <- detect1 %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 

  
print(detect1, n=97)
##all good to here - pivot wide is the last indy step

#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detect1 <- detect1 %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% ungroup()

#remove lines without information we require (non-response screens)
detect_1 <- detect1 %>% 
  filter(Code != "ITI",
         Code != "Instructions",
         Code != "Welcome")
#now remove lines with 5 or 6 response as these are also not needed
detect_1 <-detect_1 %>% 
  filter(Code != 5,
         Code != 6)

unique(detect_1$Event_Type) 
unique(detect_1$Code)

ungroup(detect_1)

#the below section is needed to pivot wide properly
#create empty column (I didn't think this was necessary)
detect_1 <- detect_1 %>% 
  add_column(Trial_Part = "part")

#rename Event Type variables to individual unique 
detect_1 <- detect_1 %>% 
  mutate(Trial_Part = case_when(
    Code == "Audio_Clip" ~ "Trial_Begin",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    Event_Type == "Response" ~ "Response",
    TRUE ~ Trial_Part))  

#of course the one name you wanted to change did not - use base r to change the last bits named "part" to Trial_Scene
detect_1$Trial_Part [ detect_1$Trial_Part == "part" ] <- "Trial_Scene" 
#remove any Time NA for pivoting
detect_1 <- detect_1 %>% 
  filter(Time != "NA")

detect_1 <- detect_1 %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_1[,3:4])
dupes1_are <- detect_1[ind,]

#re-order columns for pivot/combine after dealing with duplicates 
detect_1 <- detect_1 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

###----- Block One prepared for pivot ------ repeat with other blocks -----


##OK - can repeat for the rest now -> some duplication of columns you might want to clean up-> check descritpives codes for column names
##----- >>>>>>>>>>>>>Detect_2<<<<<<<<<<<<<<<<< ------

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datB2 <-read_csv('Pz_B2.csv', skip = 3)
#remove garbage rows
dat_B2 <- datB2  %>% 
  filter(Trial %in% (1:130) )
#  rownames_to_column() - not here - in the next script for sure though
dat_B2_1 <-read_csv('Txt_B2.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_B2 <- dat_B2 %>% 
  rename("Event_Type" = "Event Type")

#dumpcolumns we don't need 

#different number of decimals - fix this in the presentation programming at some point
dat_B2_1 <- dat_B2_1 %>% 
  mutate( Time = Time*10)

dat_B2 <- dat_B2 %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_B2)

data.frame(sapply(dat_B2,class))
data.frame(sapply(dat_B2_1,class))

dat_B2 <- dat_B2%>% mutate(Time = as.numeric(Time))
dat_B2 <- dat_B2%>% mutate(TTime = as.numeric(TTime))
#bind rows by time

detect2 <- bind_rows(dat_B2, dat_B2_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detect2 <- arrange(detect2, Time)
print(detect2, n=20)

detect2 <- detect2 %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

detect2<-detect2 %>% 
  fill(Trial, .direction = 'down') #Holy moly - that worked ;)

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first

detect2 <- detect2 %>% 
  group_by(Trial) %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown') %>% 
  ungroup()

#Now fill information from TrialNum FileName  Cond Location Onset - both up and down

detect2 <-detect2 %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'down')
class(detect2)

#All good to here - now the block, the repeats and the separation?
detect2 <- detect2 %>% 
  #  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_2', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detect2 <- arrange(detect2,TrialNum)
print(detect2)

#now fill the event type and code 
detect2 <- detect2 %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 

print(detect2, n=97)
##all good to here - pivot wide is the last indy step (might need to handle duplicates here)
#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detect2 <- detect2 %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% ungroup()

#remove lines without information we require (non-response screens)
detect_2 <- detect2 %>% 
  filter(Code != "ITI",
         Code != "Instructions",
         Code != "Welcome")
#now remove lines with 5 or 6 response as these are also not needed
detect_2 <-detect_2 %>% 
  filter(Code != 5,
         Code != 6)

unique(detect_2$Event_Type) 
unique(detect_2$Code)

ungroup(detect_2)

#the below section is needed to pivot wide properly
#create empty column (I didn't think this was necessary)
detect_2 <- detect_2 %>% 
  add_column(Trial_Part = "part")

#rename Event Type variables to individual unique 
detect_2 <- detect_2 %>% 
  mutate(Trial_Part = case_when(
    Code == "Audio_Clip" ~ "Trial_Begin",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    Event_Type == "Response" ~ "Response",
    TRUE ~ Trial_Part))  

#of course the one name you wanted to change did not - use base r to change the last bits named "part" to Trial_Scene
detect_2$Trial_Part [ detect_2$Trial_Part == "part" ] <- "Trial_Scene" 
detect_2 <- detect_2 %>% 
  filter(Time != "NA")

detect_2 <- detect_2 %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_2[,3:4])
dupes2_are <- detect_2[ind,]

#re-order columns for pivot/combine after dealing with duplicates
detect_2 <- detect_2 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

##----- >>>>>>>>>>>>> Detect_3 <<<<<<<<<<<<<<<<< ------

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datB3 <-read_csv('Pz_B3.csv', skip = 3)
#remove garbage rows
dat_B3 <- datB3  %>% 
  filter(Trial %in% (1:130) )
#  rownames_to_column() - not here - in the next script for sure though
dat_B3_1 <-read_csv('Txt_B3.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_B3 <- dat_B3 %>% 
  rename("Event_Type" = "Event Type")

sapply(transform(dat_B3,Time = as.integer(Time)), class) 
dat_B3 <- dat_B3%>% mutate(Time = as.numeric(Time))
dat_B3 <- dat_B3%>% mutate(TTime = as.numeric(TTime))

#dumpcolumns we don't need 

#different number of decimals - fix this in the presentation programming at some point
dat_B3_1 <- dat_B3_1 %>% 
  mutate( Time = Time*10)

dat_B3 <- dat_B3 %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_B3)

#bind rows by time

detect3 <- bind_rows(dat_B3, dat_B3_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detect3 <- arrange(detect3, Time)
print(detect3, n=20)

#using select to order the columns
detect3 <- detect3 %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

###>>>>>----- Fill SECTION for Wav Info <<<<<<-----
#Fill Presentation trial number down into the txt file NAs to then fill the rest
detect3<-detect3 %>% 
  fill(Trial, .direction = 'down') 

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first -> you have tried a few versions of these. So far this is the one that give the best alignment BUT still have the into the other rows issue -> the direction needs to be down
#group first
detect3 <- detect3 %>% 
  group_by(Trial) %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown') %>% 
  ungroup()

#Now fill information from TrialNum FileName  Cond Location Onset - both up and down

detect3 <-detect3 %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'down')
class(detect3)

#All good to here - now the block, the repeats and the separation?
detect3 <- detect3 %>% 
  #  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_3', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detect3 <- arrange(detect3,TrialNum)
print(detect3)

#do this later detect3 <- detect3 %>% 
#  mutate(Rep = case_when(
#    TrialNum <= 16 ~ 'Rep.1',
#    TrialNum >= 17 &  TrialNum<= 32 ~ 'Rep.2',
#    TrialNum >= 33 &  TrialNum<= 48 ~ 'Rep.3',
#    TrialNum >= 49 &  TrialNum<= 64 ~ 'Rep.4'
#  ))

#now fill the event type and code 
detect3 <- detect3 %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 


print(detect3, n=97)
##all good to here - pivot wide is the last indy step
#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detect3 <- detect3 %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% ungroup()

#remove lines without information we require (non-response screens)
detect_3 <- detect3 %>% 
  filter(Code != "ITI",
         Code != "Instructions",
         Code != "Welcome")
#now remove lines with 5 or 6 response as these are also not needed
detect_3 <-detect_3 %>% 
  filter(Code != 5,
         Code != 6)

unique(detect_3$Event_Type) 
unique(detect_3$Code)

ungroup(detect_3)

#the below section is needed to pivot wide properly
#create empty column (I didn't think this was necessary)
detect_3 <- detect_3 %>% 
  add_column(Trial_Part = "part")

#rename Event Type variables to individual unique 
detect_3 <- detect_3 %>% 
  mutate(Trial_Part = case_when(
    Code == "Audio_Clip" ~ "Trial_Begin",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    Event_Type == "Response" ~ "Response",
    TRUE ~ Trial_Part))  

#of course the one name you wanted to change did not - use base r to change the last bits named "part" to Trial_Scene
detect_3$Trial_Part [ detect_3$Trial_Part == "part" ] <- "Trial_Scene" 
#remove any Time NA for pivoting
detect_3 <- detect_3 %>% 
  filter(Time != "NA")

detect_3 <- detect_3 %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_3[,3:4])
dupes3_are <- detect_3[ind,]

#re-order columns for pivot/combine after dealing with duplicates
detect_3 <- detect_3 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

##----- >>>>>>>>>>>>> Detect_4 <<<<<<<<<<<<<<<<< ------

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datB4 <-read_csv('Pz_B4.csv', skip = 3)
#remove garbage rows
dat_B4 <- datB4  %>% 
  filter(Trial %in% (1:130) )
#  rownames_to_column() - not here - in the next script for sure though
dat_B4_1 <-read_csv('Txt_B4.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_B4 <- dat_B4 %>% 
  rename("Event_Type" = "Event Type")

#print(sapply(transform(my_dataframe,column1 = as.numeric(column1)), class)) 
sapply(transform(dat_B4,Time = as.integer(Time)), class) 
dat_B4 <- dat_B4%>% mutate(Time = as.numeric(Time))
dat_B4 <- dat_B4%>% mutate(TTime = as.numeric(TTime))

#dumpcolumns we don't need 

#different number of decimals - fix this in the presentation programming at some point
dat_B4_1 <- dat_B4_1 %>% 
  mutate( Time = Time*10)

dat_B4 <- dat_B4 %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_B4)

#bind rows by time

detect4 <- bind_rows(dat_B4, dat_B4_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detect4 <- arrange(detect4, Time)
print(detect4, n=20)

detect4 <- detect4 %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

detect4<-detect4 %>% 
  fill(Trial, .direction = 'down') #Holy moly - that worked ;)

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first

detect4 <- detect4 %>% 
  group_by(Trial) %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown') %>% 
  ungroup()

#Now fill information from TrialNum FileName  Cond Location Onset - both up and down

detect4 <-detect4 %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'down')
class(detect4)

#All good to here - now the block, the repeats and the separation?
detect4 <- detect4 %>% 
  #  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_4', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detect4 <- arrange(detect4,TrialNum)
print(detect4)


#now fill the event type and code 
detect4 <- detect4 %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 


print(detect4, n=97)

##all good to here - pivot wide is the last indy step
#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detect4 <- detect4 %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% ungroup()

#remove lines without information we require (non-response screens)
detect_4 <- detect4 %>% 
  filter(Code != "ITI",
         Code != "Instructions",
         Code != "Welcome")
#now remove lines with 5 or 6 response as these are also not needed
detect_4 <-detect_4 %>% 
  filter(Code != 5,
         Code != 6)

unique(detect_4$Event_Type) 
unique(detect_4$Code)

ungroup(detect_4)

#the below section is needed to pivot wide properly
#create empty column (I didn't think this was necessary)
detect_4 <- detect_4 %>% 
  add_column(Trial_Part = "part")

#rename Event Type variables to individual unique 
detect_4 <- detect_4 %>% 
  mutate(Trial_Part = case_when(
    Code == "Audio_Clip" ~ "Trial_Begin",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    Event_Type == "Response" ~ "Response",
    TRUE ~ Trial_Part))  

#of course the one name you wanted to change did not - use base r to change the last bits named "part" to Trial_Scene
detect_4$Trial_Part [ detect_4$Trial_Part == "part" ] <- "Trial_Scene" 

#remove any Time NA for pivoting
detect_4 <- detect_4 %>% 
  filter(Time != "NA")
#order columns for duplication check
detect_4 <- detect_4 %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_4[,3:4])
dupes4_are <- detect_4[ind,]

#re-order columns for pivot/combine after dealing with duplicates
detect_4 <- detect_4 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

##----- >>>>>>>>>>>>> Detect_5 <<<<<<<<<<<<<<<<< ------

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datB5 <-read_csv('Pz_B5.csv', skip = 3)
#remove garbage rows
dat_B5 <- datB5  %>% 
  filter(Trial %in% (1:130) )
#  rownames_to_column() - not here - in the next script for sure though
dat_B5_1 <-read_csv('Txt_B5.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_B5 <- dat_B5 %>% 
  rename("Event_Type" = "Event Type")

#print(sapply(transform(my_dataframe,column1 = as.numeric(column1)), class)) 
sapply(transform(dat_B5,Time = as.integer(Time)), class) 
dat_B5 <- dat_B5%>% mutate(Time = as.numeric(Time))
dat_B5 <- dat_B5%>% mutate(TTime = as.numeric(TTime))

#dumpcolumns we don't need 

#different number of decimals - fix this in the presentation programming at some point
dat_B5_1 <- dat_B5_1 %>% 
  mutate( Time = Time*10)

dat_B5 <- dat_B5 %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_B5)

#bind rows by time

detect5 <- bind_rows(dat_B5, dat_B5_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detect5 <- arrange(detect5, Time)
print(detect5, n=20)

detect5 <- detect5 %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

detect5<-detect5 %>% 
  fill(Trial, .direction = 'down') #Holy moly - that worked ;)

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first

detect5 <- detect5 %>% 
  group_by(Trial) %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown') %>% 
  ungroup()

#Now fill information from TrialNum FileName  Cond Location Onset - both up and down

detect5 <-detect5 %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'down')
class(detect2)

#All good to here - now the block, the repeats and the separation?
detect5 <- detect5 %>% 
  #  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_5', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detect5 <- arrange(detect5,TrialNum)
print(detect5)

#now fill the event type and code 
detect5 <- detect5 %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 

print(detect5, n=97)
##all good to here - pivot wide is the last indy step
#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detect5 <- detect5 %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% 
  ungroup()

#remove lines without information we require (non-response screens)
detect_5 <- detect5 %>% 
  filter(Code != "ITI",
         Code != "Instructions",
         Code != "Welcome")
#now remove lines with 5 or 6 response as these are also not needed
detect_5 <-detect_5 %>% 
  filter(Code != 5,
         Code != 6)

unique(detect_5$Event_Type) 
unique(detect_5$Code)

ungroup(detect_5)

#the below section is needed to pivot wide properly
#create empty column (I didn't think this was necessary)
detect_5 <- detect_5 %>% 
  add_column(Trial_Part = "part")

#rename Event Type variables to individual unique 
detect_5 <- detect_5 %>% 
  mutate(Trial_Part = case_when(
    Code == "Audio_Clip" ~ "Trial_Begin",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    Event_Type == "Response" ~ "Response",
    TRUE ~ Trial_Part))  

#of course the one name you wanted to change did not - use base r to change the last bits named "part" to Trial_Scene
detect_5$Trial_Part [ detect_5$Trial_Part == "part" ] <- "Trial_Scene" 
#remove any Time NA for pivoting
detect_5 <- detect_5 %>% 
  filter(Time != "NA")

#order columns for duplication check
detect_5 <- detect_5 %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_5[,3:4])
dupes5_are <- detect_5[ind,]

#re-order columns for pivot/combine after dealing with duplicates
detect_5 <- detect_5 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

##----- >>>>>>>>>>>>> Detect_6 <<<<<<<<<<<<<<<<< ------

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datB6 <-read_csv('Pz_B6.csv', skip = 3)
#remove garbage rows
dat_B6 <- datB6  %>% 
  filter(Trial %in% (1:130) )
#  rownames_to_column() - not here - in the next script for sure though
dat_B6_1 <-read_csv('Txt_B6.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_B6 <- dat_B6 %>% 
  rename("Event_Type" = "Event Type")

#print(sapply(transform(my_dataframe,column1 = as.numeric(column1)), class)) 
sapply(transform(dat_B6,Time = as.integer(Time)), class) 
dat_B6 <- dat_B6%>% mutate(Time = as.numeric(Time))
dat_B6 <- dat_B6%>% mutate(TTime = as.numeric(TTime))

#dumpcolumns we don't need 

#different number of decimals - fix this in the presentation programming at some point
dat_B6_1 <- dat_B6_1 %>% 
  mutate( Time = Time*10)

dat_B6 <- dat_B6 %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_B6)

#bind rows by time

detect6 <- bind_rows(dat_B6, dat_B6_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detect6 <- arrange(detect6, Time)
print(detect6, n=20)

detect6 <- detect6 %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

detect6<-detect6 %>% 
  fill(Trial, .direction = 'down') #Holy moly - that worked ;)

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first

detect6 <- detect6 %>% 
  group_by(Trial) %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown') %>% 
  ungroup()

#Now fill information from TrialNum FileName  Cond Location Onset - both up and down

detect6 <-detect6 %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'down')
class(detect6)

#All good to here - now the block, the repeats and the separation?
detect6 <- detect6 %>% 
  #  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_6', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detect6 <- arrange(detect6,TrialNum)
print(detect6)

#do this later detect6 <- detect6 %>% 
#  mutate(Rep = case_when(
#    TrialNum <= 16 ~ 'Rep.1',
#    TrialNum >= 17 &  TrialNum<= 32 ~ 'Rep.2',
#    TrialNum >= 33 &  TrialNum<= 48 ~ 'Rep.3',
#    TrialNum >= 49 &  TrialNum<= 64 ~ 'Rep.4'
#  ))

#now fill the event type and code 
detect6 <- detect6 %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 

print(detect6, n=97)
##all good to here - pivot wide is the last indy step
#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detect6 <- detect6 %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% ungroup()

#remove lines without information we require (non-response screens)
detect_6 <- detect6 %>% 
  filter(Code != "ITI",
         Code != "Instructions",
         Code != "Welcome")
#now remove lines with 5 or 6 response as these are also not needed
detect_6 <-detect_6 %>% 
  filter(Code != 5,
         Code != 6)

unique(detect_6$Event_Type) 
unique(detect_6$Code)

ungroup(detect_6)

#the below section is needed to pivot wide properly
#create empty column (I didn't think this was necessary)
detect_6 <- detect_6 %>% 
  add_column(Trial_Part = "part")

#rename Event Type variables to individual unique 
detect_6 <- detect_6 %>% 
  mutate(Trial_Part = case_when(
    Code == "Audio_Clip" ~ "Trial_Begin",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    Event_Type == "Response" ~ "Response",
    TRUE ~ Trial_Part))  

#of course the one name you wanted to change did not - use base r to change the last bits named "part" to Trial_Scene
detect_6$Trial_Part [ detect_6$Trial_Part == "part" ] <- "Trial_Scene" 
#remove any Time NA for pivoting
detect_6 <- detect_6 %>% 
  filter(Time != "NA")

#order columns for duplication check

detect_6 <- detect_6 %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_6[,3:4])
dupes6_are <- detect_6[ind,]

#re-order columns 
detect_6 <- detect_6 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)


##----- >>>>>>>>>>>>> Detect_T <<<<<<<<<<<<<<<<< ------

#Note - ignore any NA by coercion warnings here - and other warnings -  
### Read in the two files for that block - the Pz_Bx and the Txt_Bx
datBT <-read_csv('Pz_BT.csv', skip = 3)
#remove garbage rows
#dat_BT <- datBT [-c(299:nrow(datBT)), ]
dat_BT <- datBT  %>% 
  filter(Trial %in% (1:172) ) #more reliable method - reduces failure point
#  rownames_to_column() - not here - in the next script for sure though
dat_BT_1 <-read_csv('Txt_BT.csv') 
#  rownames_to_column()

#fix the event type oops again
dat_BT <- dat_BT %>% 
  rename("Event_Type" = "Event Type")

sapply(transform(dat_BT,Time = as.integer(Time)), class) 
dat_BT <- dat_BT%>% mutate(Time = as.numeric(Time))
dat_BT <- dat_BT%>% mutate(TTime = as.numeric(TTime))
dat_BT_1 <-read_csv('Txt_BT.csv') 

#dumpcolumns we don't need - this is a bit farther down now

#different number of decimals - fix this in the presentation programming at some point
dat_BT_1 <- dat_BT_1 %>% 
  mutate( Time = Time*10)

dat_BT <- dat_BT %>% 
  select(Trial, Event_Type, Code, Time, TTime)
colnames(dat_BT)

#bind rows by time

detectT <- bind_rows(dat_BT, dat_BT_1) #because you need to create a new df to remove the rows,start without the underscore then use the underscore for the block name to harmonize with the existing 

detectT <- arrange(detectT, Time)
print(detectT, n=20)
#recall the before the pivot Test has teh same number of columns as blocks - this is actually what you want
detectT <- detectT %>% 
  select(Trial, TrialNum, FileName, Cond, Location, Onset,  Event_Type, Code, Time, TTime)

detectT<-detectT %>% 
  fill(Trial, .direction = 'down') #Holy moly - that worked ;)

#now need to fill down the other variables - TrialNum, through Onset -> arranged by Trial and TrailNum first

#detectT <- arrange(detectT, Trial, TrialNum)
#print(detectT, n=120)

#Now fill information from TrialNum FileName  Cond Location Onset -> for the test do not rearrange before the fill or things get messed up

detectT <-detectT %>% 
  fill(TrialNum, FileName,  Cond, Location, Onset, .direction = 'updown')

#All good to here - now the block, the repeats and the separation?
detectT <- detectT %>% 
  #  add_column(Rep = 0, .before = 'Trial') %>% ADD this after 
  add_column(Block = 'detect_T', .before = 'Trial') %>% 
  add_column(Sbj = ('P16208_052824'), .before = 'Block')

##arrange by TrialNum to insert rep number
detectT <- arrange(detectT,TrialNum)
print(detectT)

#now fill the event type and code 
detectT <- detectT %>% replace_na(list(Event_Type = "Sound")) %>% 
  replace_na(list(Code = "Audio_Clip")) %>% 
  replace_na(list(TTime = 0)) 

print(detectT, n=97)
##all good to here - pivot wide is the last indy step
ungroup(detectT)
#Remove Trial column for pivot wide - does not add any information here and the wider call goes awry otherwise
detectT <- detectT %>% 
  group_by(TrialNum) %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Event_Type, Code, Time, TTime) %>% ungroup()

#remove lines without information we require (non-response screens)
detect_T <- detectT %>% 
  filter(Code != "ITI",
         Code != "TestInstructions",
         Code != "TestWelcome")

#remove "enter" to advance to next screen responses
detect_T <-detect_T %>% 
  filter(Code != 5)
print(detect_T, n=50)

unique(detect_T$Event_Type) 
unique(detect_T$Code)

ungroup(detect_T)

#the below section is needed to pivot wide properly
#create empty column for identifying the trial parts(I didn't think the empty was was necessary??)
detect_T <- detect_T %>% 
  add_column(Trial_Part = "part")

#create lagged column for naming the multiple response codes in Trial_Part
detect_T <- detect_T %>% 
  mutate('resp' = lag(Code))

unique(detect_T$resp)
#rename Event Type variables to individual unique 
detect_T <- detect_T %>% 
  mutate(Trial_Part = case_when(
    Code == "Cue" ~ "Trial_Cue",
    Code == "Audio_Clip" ~ "Trial_End",
    Code == "old_new_response" ~ "Trial_Q_ON",
    Code == "LRsideMatch" ~ "Trial_Q_LR",
    Event_Type == "Picture" ~ "Trial_Type",
    Code == "Target Onset" ~ "Trial_Target",
    resp == "Target Onset" ~ "Response",
    resp == "old_new_response" ~ "old_new_response",
    resp == "LRsideMatch" ~ "location_response",
    TRUE ~ Trial_Part))  

#filter rows -> if trial part is still part then we do not need that row any longer
detect_T <- detect_T %>% 
  filter(Trial_Part != 'part')

#list duplicates for Vetting
detect_T <- detect_T %>% 
  select(Sbj,Block, TrialNum, Trial_Part, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

#find duplicated responses, assign to new DF and move on
ind <- duplicated(detect_T[,3:4])
dupesT_are <- detect_T[ind,]

#re-order columns for pivot/combine after dealing with duplicates

#order columns for pivoting
detect_T <- detect_T %>% 
  select(Sbj, Block, TrialNum, FileName, Cond, Location, Onset, Trial_Part, Code, Time, TTime)

### ----- BEFORE PIVOTING -----
#assess duplicates and decided which to remove using the following code (remove hash tag and run) - the following line is intended to stop the run

#halting row '964'
break()
#export csv with duplicates

dupes_all <- bind_rows(
   dupes1_are, 
   dupes2_are,
   dupes3_are,
#   dupes4_are,
   dupes5_are,
   dupes6_are,
#  dupesT_are
)

write_csv(dupes_all,'P16208_4_05292024_all_dupes.csv') 

#remove rows once decided which based on duplicates - remove hash tag, fill in row numbers and continue

detect_1 <- detect_1[-c(46,47, 138),] #these are the row numbers - good
detect_2 <- detect_2[-c(8),] #these are the row numbers
detect_3 <- detect_3[-c(261),] #these are the row numbers
#detect_4 <- detect_4[-c(251,311),] #these are the row numbers
detect_5 <- detect_5[-c(136, 262, 318),] #these are the row numbers
detect_6 <- detect_6[-c(86, 117),] #these are the row numbers
#detect_T <- detect_T[-c(4,5,1),] #these are the row numbers


#16208 has dupes in all except d_4 & d_T

#Now run the rest

#ready to pivot wide ----- PIVOT TEST----
detect_T <- detect_T%>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_T)

#update responses
detect_T$Code_Response [ detect_T$Code_Response == "3" ] <- "left" 
detect_T$Code_Response [ detect_T$Code_Response == "4"] <- "right"

##---split the condition column ---

detect_T <- detect_T %>%
  separate(Code_Trial_Type, c("Condition", "Onset_T", "Side"), "_")

##correct the condition using the numerics

detect_T <- detect_T %>% 
  mutate(Condition = case_when(
    Cond == "1" ~ "fixed",
    Cond == "2" ~ "fixed",
    Cond == "3" ~ "random",
    Cond == "4" ~ "random",
    Cond == "5" ~ "test",
    Cond == "6" ~ "test"
))  

#now clean up the one oooops for time/side swap (also find this and fix it in Presentation for next time)

detect_T$Onset_T [ detect_T$Onset_T == "right" ] <- "1600" 
detect_T$Side [ detect_T$Side == "1600"] <- "right"

colnames(detect_T)
##all good so far -> now we have many unneeded columns to ditch
detect_T <- detect_T %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Condition, Onset_T, Side, Code_Response, Code_old_new_response, Code_location_response, Time_Trial_Cue, Time_Trial_Target, Time_Response, Time_Trial_Q_ON,Time_old_new_response, Time_Trial_Q_LR, Time_location_response, Time_Trial_End, TTime_Trial_Cue, TTime_Trial_Target, TTime_Response, TTime_old_new_response, TTime_location_response)

colnames(detect_T)
ungroup(detect_T)

#calculate the duplicates for test 
dupesT_are <- subset(detect_T,duplicated(TrialNum))

###---- >>>>>>>>>>>>>> Pivot Section for Wide data <<<<<<<<<<<<<< ---------

###>>>Block 1<<<###
#ready to pivot wide
detect_1 <- detect_1%>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_1)

#update responses
detect_1$Code_Response [ detect_1$Code_Response == "3" ] <- "left" 
detect_1$Code_Response [ detect_1$Code_Response == "4"] <- "right"

##all good so far -> now we have many unneeded columns to ditch

detect_1 <- detect_1 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Code_Trial_Type, Code_Response, Time_Trial_Scene, Time_Trial_Target, Time_Response, TTime_Trial_Scene, TTime_Trial_Target, TTime_Response)

colnames(detect_1)
ungroup(detect_1)

##---split the condition column ---

detect_1 <- detect_1 %>%
  separate(Code_Trial_Type, c("Condition", "Side", "Onset_T"), "_")

#add the exposure number

#ready for accuracy checks


###>>>Block 2<<<###
#ready to pivot wide --- Block 2 -------------------#
detect_2 <- detect_2 %>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_2)

#the above is getting close - try pivot wider again with the code_sound column omitted
detect_2$Code_Response [ detect_2$Code_Response == "3" ] <- "left" 
detect_2$Code_Response [ detect_2$Code_Response == "4"] <- "right"

##all good so far -> now we have many unneeded columns to ditch

detect_2 <- detect_2 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Code_Trial_Type, Code_Trial_Target, Code_Response, Time_Trial_Scene, Time_Trial_Target, Time_Response, TTime_Trial_Scene, TTime_Trial_Target, TTime_Response)

colnames(detect_2)

detect_2 <- detect_2 %>%
  separate(Code_Trial_Type, c("Condition", "Side", "Onset_T"), "_")

#ready for accuracy checks

###>>>Block 3<<<###
#ready to pivot wide - block 3 ------
detect_3 <- detect_3%>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_3)

#update responses
detect_3$Code_Response [ detect_3$Code_Response == "3" ] <- "left" 
detect_3$Code_Response [ detect_3$Code_Response == "4"] <- "right"

##all good so far -> now we have many unneeded columns to ditch

detect_3 <- detect_3 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Code_Trial_Type, Code_Response, Time_Trial_Scene, Time_Trial_Target, Time_Response, TTime_Trial_Scene, TTime_Trial_Target, TTime_Response)

colnames(detect_3)
ungroup(detect_3)

##---split the condition column ---

detect_3 <- detect_3 %>%
  separate(Code_Trial_Type, c("Condition", "Side", "Onset_T"), "_")

#add the exposure number

##ready for accuracy checks

###>>>Block 4<<<###
#ready to pivot wide - block 4------
detect_4 <- detect_4%>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_4)

#update responses
detect_4$Code_Response [ detect_4$Code_Response == "3" ] <- "left" 
detect_4$Code_Response [ detect_4$Code_Response == "4"] <- "right"

##all good so far -> now we have many unneeded columns to ditch

detect_4 <- detect_4 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Code_Trial_Type, Code_Response, Time_Trial_Scene, Time_Trial_Target, Time_Response, TTime_Trial_Scene, TTime_Trial_Target, TTime_Response)

colnames(detect_4)
ungroup(detect_4)

##---split the condition column ---

detect_4 <- detect_4 %>%
  separate(Code_Trial_Type, c("Condition", "Side", "Onset_T"), "_")


##----- After Pivot BUT before binding blocks check for duplicate responses -----
#check for duplicates by block? Must be a way try group by but might then need tidy soloution for piping and ungroupoing - in the meantime can produce df of trials with more than one response and so chose which to keep based on your rules -> 
#>>>>>>>>>>>>>>>This happens earlier in the code

###>>>Block 5<<<###
#ready to pivot wide - block 4------
detect_5 <- detect_5%>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_5)

#update responses
detect_5$Code_Response [ detect_5$Code_Response == "3" ] <- "left" 
detect_5$Code_Response [ detect_5$Code_Response == "4"] <- "right"

##all good so far -> now we have many unneeded columns to ditch

detect_5 <- detect_5 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Code_Trial_Type, Code_Response, Time_Trial_Scene, Time_Trial_Target, Time_Response, TTime_Trial_Scene, TTime_Trial_Target, TTime_Response)

colnames(detect_5)
ungroup(detect_5)

##---split the condition column ---

detect_5 <- detect_5 %>%
  separate(Code_Trial_Type, c("Condition", "Side", "Onset_T"), "_")

#add the exposure number
#ready for accuracy checks

###>>>Block 6<<<###
#ready to pivot wide - block 4------
detect_6 <- detect_6%>%
  pivot_wider(names_from = Trial_Part, values_from = c(Code, Time, TTime))
print(detect_6)

#update responses
detect_6$Code_Response [ detect_6$Code_Response == "3" ] <- "left" 
detect_6$Code_Response [ detect_6$Code_Response == "4"] <- "right"

##all good so far -> now we have many unneeded columns to ditch

detect_6 <- detect_6 %>% 
  select(Sbj,Block, TrialNum, FileName, Cond, Location, Onset, Code_Trial_Type, Code_Response, Time_Trial_Scene, Time_Trial_Target, Time_Response, TTime_Trial_Scene, TTime_Trial_Target, TTime_Response)

colnames(detect_6)
ungroup(detect_6)

##---split the condition column ---

detect_6 <- detect_6 %>%
  separate(Code_Trial_Type, c("Condition", "Side", "Onset_T"), "_")

#add exposure number - trialNum is numeric

detect_1 <- detect_1 %>% 
  mutate(Rep = case_when(
    TrialNum <= 16  ~ 1,
    TrialNum >= 17 & TrialNum <= 32 ~ 2,
    TrialNum >= 33 & TrialNum <= 48 ~ 3,
    TrialNum >= 49 & TrialNum <= 64 ~ 4,
  ))  

detect_2 <- detect_2 %>% 
  mutate(Rep = case_when(
    TrialNum <= 16  ~ 5,
    TrialNum >= 17 & TrialNum <= 32 ~ 6,
    TrialNum >= 33 & TrialNum <= 48 ~ 7,
    TrialNum >= 49 & TrialNum <= 64 ~ 8,
  )) 

detect_3 <- detect_3 %>% 
  mutate(Rep = case_when(
    TrialNum <= 16  ~ 9,
    TrialNum >= 17 & TrialNum <= 32 ~ 10,
    TrialNum >= 33 & TrialNum <= 48 ~ 11,
    TrialNum >= 49 & TrialNum <= 64 ~ 12,
  )) 

detect_4 <- detect_4 %>% 
  mutate(Rep = case_when(
    TrialNum <= 16  ~ 13,
    TrialNum >= 17 & TrialNum <= 32 ~ 14,
    TrialNum >= 33 & TrialNum <= 48 ~ 15,
    TrialNum >= 49 & TrialNum <= 64 ~ 16,
  )) 

detect_5 <- detect_5 %>% 
  mutate(Rep = case_when(
    TrialNum <= 16  ~ 17,
    TrialNum >= 17 & TrialNum <= 32 ~ 18,
    TrialNum >= 33 & TrialNum <= 48 ~ 19,
    TrialNum >= 49 & TrialNum <= 64 ~ 20,
  ))

detect_6 <- detect_6 %>% 
  mutate(Rep = case_when(
    TrialNum <= 16  ~ 21,
    TrialNum >= 17 & TrialNum <= 32 ~ 22,
    TrialNum >= 33 & TrialNum <= 48 ~ 23,
    TrialNum >= 49 & TrialNum <= 64 ~ 24,
  ))

#ready for accuracy checks
data.frame(sapply(detect_1,class))
#Bind Rows
detect_exp <- bind_rows(detect_T,detect_1, detect_2, detect_3, detect_4, detect_5, detect_6)
detect_exp <- detect_exp %>% 
  mutate(Wav = FileName,
  )


detect_exp$Wav <- recode_factor(
  detect_exp$Wav,
`1` = "TRAIN",
`2` = "B2_WHISPERINGVOICES",
`3` = "PIGS",
`4` = "RIVER",
`5` = "SUMMERNIGHT",
`6` = "B2_SANDPAPER",
`7` = "B2_FLIPPINGBOOKPAGES",
`8` =	"ELECTRICITY",
`9` = "FROGPOND",
`10`= "HAIRDRYING",
`11`= "TRIBALCHANT",
`12`= "HIGHWAY",
`13`= 'THROWINGGLASS',
`14`= 'BUBBLING',
`15`= 'ORCHESTRATUNING',
`16`= "GEESE",
`17`= "CLATTEROFHOOVES",
`18`= "B2_CAROUSEL",
`19`= "APPLAUSE",
`20`= "LIGHTNING",
`21`= "BASKETBALL",
`22`= "ARCADES",
`23`= "EERIEFOREST",
`24`= "BALLDRUMMING") #seems to have worked

#add version number

detect_exp <- detect_exp %>% 
  mutate(Ver = "Four")

#add RMsq

detect_exp <-detect_exp %>% 
  mutate(RmSq = case_when(
    Wav == "TRAIN" ~ 0.071109,
    Wav == "B2_WHISPERINGVOICES" ~ 0.043599481,
    Wav == "PIGS" ~ 0.077371,
    Wav == "RIVER" ~ 0.056176,
    Wav == "SUMMERNIGHT" ~ 0.066183,
    Wav == "B2_SANDPAPER" ~ 0.048593,
    Wav == "B2_FLIPPINGBOOKPAGES" ~ 0.023434,
    Wav ==	"ELECTRICITY" ~ 0.068457,
    Wav == "FROGPOND" ~ 0.059547,
    Wav == "HAIRDRYING" ~ 0.069044,
    Wav == "TRIBALCHANT" ~ 0.069044,
    Wav == "HIGHWAY" ~ 0.086179,
    Wav == 'THROWINGGLASS' ~ 0.056993,
    Wav == 'BUBBLING' ~ 0.095958,
    Wav == 'ORCHESTRATUNING' ~ 0.0607422,
    Wav == "GEESE" ~ 0.099313,
    Wav == "CLATTEROFHOOVES" ~ 0.013302,
    Wav == "B2_CAROUSEL" ~ 0.040665,
    Wav == "APPLAUSE" ~ 0.070322702,
    Wav == "LIGHTNING" ~ 0.147110,
    Wav == "BASKETBALL" ~ 0.083089 ,
    Wav == "ARCADES" ~ 0.089529,
    Wav == "EERIEFOREST" ~ 0.082144,
    Wav == "BALLDRUMMING" ~ 0.1434
  ))


#recode columns to match analysis scripts
#Fix duplicate names first

detect_exp <- detect_exp %>% 
  rename(
    StimCon = Cond,
    rmv1 = Onset)
         
#note the old "Time" is now a number fo columns and will re-emerge when pivot long - keep these and leave them as is
detect_exp <- detect_exp %>% 
  rename(
         Cond = Condition,
         Onset = Onset_T,
         Resp = Code_Response,
         Resp_Old = Code_old_new_response,
         Resp_Loc = Code_location_response,
         KTime = TTime_Response,
         TTime = TTime_Trial_Target,
         rt_Old = TTime_old_new_response,
         rt_Loc = Time_location_response
         ) 


detect_exp$Side [ detect_exp$Side == "Left" ] <- "left"  
detect_exp$Side [ detect_exp$Side == "Right" ] <- "right"  

##----- ACC TIME -----

# determine rt by subtracting the response time from total trial time then divide to get time into typical format
detect_exp <- detect_exp %>% 
  mutate(base_rt = ((KTime - TTime) / 10))

detect_exp <- detect_exp %>% 
  mutate(round(base_rt, digits = 0)) #rounds to three integers with no decimals

detect_exp <- detect_exp %>% 
   rename_at('round(base_rt, digits = 0)', ~'rt') #rename rt column


#cross check column
detect_exp <- detect_exp %>% 
  mutate(RT_Chk = (Time_Response - Time_Trial_Target)/10)

## Accuracy 

detect_exp <- detect_exp %>% 
  mutate(ACC = if_else( 
    (Resp) == (Side), "1", "0"))

detect_exp$ACC [ is.na(detect_exp$ACC) ] = 0 

#Old/New accuracy
detect_exp <- detect_exp %>% 
  mutate(ACC_ON = case_when(
    Resp_Old == 3 & StimCon <= 4 ~ 1,
    Resp_Old == 3 & StimCon >= 5 ~ 0,
    Resp_Old == 4 & StimCon >= 5 ~ 1,
    Resp_Old == 4 & StimCon <= 4 ~ 0,
  ))


#Location accuracy
detect_exp <- detect_exp %>% 
  mutate(ACC_Loc = case_when(
    Resp_Loc == 3 & StimCon == 1 ~ 1,
    Resp_Loc == 3 & StimCon != 1 ~ 0,
    Resp_Loc == 4 & StimCon == 2 ~ 1,
    Resp_Loc == 4 & StimCon != 2 ~ 0,
    Resp_Loc == 6 & StimCon >= 5 ~ 1,
    Resp_Loc == 6 & StimCon <= 4 ~ 0,
  ))

##recode test extras

detect_exp$Resp_Old <- recode_factor(
  detect_exp$Resp_Old,
  `3` = "old",
  `4` = "new",
  .default = "NA"
 # .missing = "M"
)

detect_exp$Resp_Loc <- recode_factor(
  detect_exp$Resp_Loc,
  `3` = "left",
  `4` = "right",
  `6` = "new",
  .default = "NA"
#  .missing = "M"
)
colnames(detect_exp)
#arrange columns
detect_exp <- detect_exp %>% 
  select(Sbj, Block, TrialNum, Wav, Cond, Side,Location, Onset, Resp, rt, ACC, Resp_Old, rt_Old, ACC_ON, Resp_Loc, rt_Loc, ACC_Loc, TTime, KTime, Time_Trial_Target, Time_Response, StimCon, FileName,  RT_Chk, Time_Trial_Cue, Time_Trial_Target, Time_Response,  Time_Trial_Q_ON, TTime_Trial_Cue, TTime_location_response, Time_Trial_Scene, TTime_Trial_Scene, Ver, Rep, RmSq)  

arrange(detect_exp, TrialNum)

#Outliers at the level ofthe individual

detect_exp_out <-detect_exp %>% 
  filter(Block != 'detect_T')

detect_exp_test <-detect_exp %>% 
  filter(Block == 'detect_T')

##calculate all exposure phase only outliers using detect_exp_out
## Recall that the outliers are actually "1" and you keep the "NOT an Outlier 0"
detect_exp_out <- mutate(detect_exp_out , 
                         ex_rt_median = median(rt, na.rm=T), 
                         ex_rt_stdev = sd(rt, na.rm=T), 
                         ex_rt_Outlier = 1*(abs(rt - ex_rt_median)>2*ex_rt_stdev))
hist(filter(detect_exp_out,ex_rt_Outlier==0)$rt, na.rm=T) #in case you want to see the outcome data shape for exclusions

#and for test - make a little df then change all vales for ex_rt_Outlier columns to 0 before binding
detect_exp_test <- mutate(detect_exp_test , 
                          ex_rt_median = median(rt, na.rm=T), 
                          ex_rt_stdev = sd(rt, na.rm=T), 
                          ex_rt_Outlier = 1*(abs(rt - ex_rt_median)>2*ex_rt_stdev))

#little data frame
test_rt_outliers <- detect_exp_test

# change to 0s to prevent filtering out by accident as an outlier 
detect_exp_test <- mutate(detect_exp_test, ex_rt_Outlier = 0)

#reassemble data frame

detect_exp <- bind_rows(detect_exp_out,detect_exp_test)

#extra column arrange to alloe manual add of this one csv rather than running the whole combining one again 

detect_exp <- detect_exp %>% 
  select(Sbj, Block, TrialNum, Wav, Cond, Side,Location, Onset, Resp, rt, ACC, Resp_Old, rt_Old, ACC_ON, Resp_Loc, rt_Loc, ACC_Loc, TTime, KTime, ex_rt_median, ex_rt_stdev, ex_rt_Outlier, Time_Trial_Target, Time_Response, StimCon, FileName,  RT_Chk, Time_Trial_Cue, Time_Trial_Q_ON, TTime_Trial_Cue, TTime_location_response, Ver, Rep, RmSq)  

arrange(detect_exp, TrialNum)

#export Comb csv for visualizing and analysis
write_csv(detect_exp,'P16208_4_COMBO_052924_Cleaned.csv') ## ALL good
save(detect_exp, file='P16208_4.RData')

#remove error
detect_exp_plt <- detect_exp %>%
  filter (ACC==1)

#remove too fast
detect_exp_plt <- detect_exp %>% 
  filter(rt > 200)  

detect_exp_plt <- detect_exp_plt %>% 
  filter(ex_rt_Outlier == 0)#recall 1 denotes outliers

#plots for peeking
datp_Block_scat <- ggplot(detect_exp, aes(x = Block, y = rt, fill = Cond)) 
datp_Block_scat + geom_point() + theme_minimal() + geom_jitter()

datp_Block_box <- ggplot(detect_exp, aes(x = Block, y = rt, fill = Cond)) 
datp_Block_box + geom_boxplot() + theme_minimal()

# ----- Plots for Saving

FLT_datp_Block_scat <- ggplot(detect_exp, aes(x = Block, y = rt, fill = Cond, color=Cond)) + geom_point() + labs(title = "Clean rt P16208_4", x = "Block", y = "rt in milliseconds") + theme_grey() + geom_jitter() + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_datp_Block_scat

ggsave('All_cond_block_Scat.png', units = 'in', width = 5, height = 5)

FLT_cond_block <- ggplot(detect_exp_plt, aes(x = Block, y = rt, fill = Cond)) +  geom_boxplot()  + labs(title = "Condition by Block P16208_4", x = "Block", y = "rt in milliseconds")  + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_cond_block

# ----- Save Plot Without Outliers

ggsave('All_cond_block_Box.png', units = 'in', width = 5, height = 5)

rt_summary <-detect_exp_plt %>%
  group_by(Block, Cond ) %>%
  summarize(mean=mean(rt,na.rm=TRUE),
            median=median(rt,na.rm=TRUE),
            SErt = std.error(rt,na.rm = TRUE),
            std_dev=sd(rt,na.rm=TRUE))
rt_summary

rt_summary <- as.data.frame(rt_summary)
rt_summary
rt_summary <-gt(rt_summary)
rt_summary
rt_summary <- 
  rt_summary |>
  tab_header(
    title = md("**P16208_4 Mean, Median & SD AVG rt by Condition**"),
    subtitle =md("All Correct Exposure Trials")
  )
rt_summary |> gtsave('ALL_AVG_rt_Cond_Means.png', expand = 10)

### ----- Without Test---

detect_exp_plt <- detect_exp_plt %>% 
  filter(Block != "detect_T")

detect_exp_con <- detect_exp_plt %>% 
  group_by(Cond)

detect_exp_plt <- detect_exp_plt %>% 
  group_by(Block, Cond)

#plots for peeking
datp_Block_scat <- ggplot(detect_exp_plt, aes(x = Block, y = rt, fill = Cond)) 
datp_Block_scat + geom_point() + theme_minimal() + geom_jitter()

datp_Block_box <- ggplot(detect_exp_plt, aes(x = Block, y = rt, fill = Cond)) 
datp_Block_box + geom_boxplot() + theme_minimal()

#plots for peeking
datp_Block_scat <- ggplot(detect_exp_con, aes(x = Cond, y = rt, fill = Cond)) 
datp_Block_scat + geom_point() + theme_minimal() + geom_jitter()

datp_Block_box <- ggplot(detect_exp_con, aes(x = Cond, y = rt, fill = Cond)) 
datp_Block_box + geom_boxplot() + theme_minimal()

# ----- Plots for Saving

FLT_datp_Block_scat <- ggplot(detect_exp_plt, aes(x = Block, y = rt, fill = Cond, color=Cond)) + geom_point() + labs(title = "Clean rt P16208_4", x = "Block", y = "rt in milliseconds") + theme_grey() + geom_jitter() + theme(
 axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_datp_Block_scat

ggsave('cond_block_Scat.png', units = 'in', width = 5, height = 5)

FLT_cond_block <- ggplot(detect_exp_plt, aes(x = Block, y = rt, fill = Cond)) +  geom_boxplot()  + labs(title = "Condition by Block P16208_4", x = "Block", y = "rt in milliseconds")  + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_cond_block

# ----- Save Plot Without Outliers

ggsave('cond_block_Box.png', units = 'in', width = 5, height = 5)

# plot by condition only
FLT_cond <- ggplot(detect_exp_con, aes(x = Cond, y = rt, fill = Cond)) +  geom_boxplot()  + labs(title = "Condition Only by Block P16208_4", x = "Block", y = "rt in milliseconds")  + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_cond

# ----- Save Plot Without Outliers

ggsave('cond_Box.png', units = 'in', width = 5, height = 5)


#make 3 digits a thing
options(digits = 3) 

#Summary Table for above plots

detect_exp_plt_sum <- detect_exp_plt  %>% 
  group_by(Block, Cond) %>%
  summarize(mean=mean(rt,na.rm=TRUE),
            median=median(rt,na.rm=TRUE),
            SErt = std.error(rt,na.rm = TRUE),
            std_dev=sd(rt,na.rm=TRUE))
print(detect_exp_plt_sum)

detect_exp_plt_sum <- as.data.frame(detect_exp_plt_sum)
detect_exp_plt_sum
detect_exp_plt_sum <-gt(detect_exp_plt_sum)
detect_exp_plt_sum
detect_exp_plt_sum <- 
  detect_exp_plt_sum |>
  tab_header(
    title = md("**P16208_4 Mean, Median & SD AVG rt by Block & Condition**"),
    subtitle =md("All Correct Exposure Trials")
  )
detect_exp_plt_sum |> gtsave('AVG_rt_Cond_Block_Means.png', expand = 10)

print(detect_exp_plt_sum)

rt_summary <-detect_exp_plt %>%
  group_by(Cond) %>%
  summarize(mean=mean(rt,na.rm=TRUE),
            median=median(rt,na.rm=TRUE),
            SErt = std.error(rt,na.rm = TRUE),
            std_dev=sd(rt,na.rm=TRUE))
rt_summary

rt_summary <- as.data.frame(rt_summary)
rt_summary
rt_summary <-gt(rt_summary)
rt_summary
rt_summary <- 
  rt_summary |>
  tab_header(
    title = md("**P16208_4 Mean, Median & SD AVG rt by Condition**"),
    subtitle =md("All Correct Exposure Trials")
  )
rt_summary |> gtsave('AVG_rt_Cond_Means.png', expand = 10)



## TEST Accuracy
Test_rt_ACC <-detect_exp%>% 
  filter(ACC == 1)

Test_rt_ACC <-Test_rt_ACC %>%
  group_by(Cond) %>%
  summarize(mean=mean(rt,na.rm=TRUE),
            median=median(rt,na.rm=TRUE),
            SErt = std.error(rt,na.rm = TRUE),
            std_dev=sd(rt,na.rm=TRUE))
Test_rt_ACC

Test_rt_ACC<- as.data.frame(Test_rt_ACC)
Test_rt_ACC <- gt(Test_rt_ACC)
Test_rt_ACC

Test_rt_ACC <- 
  Test_rt_ACC |>
  tab_header(
    title = md("**Tone Detection @ Test Accuracy Proportions**"),
    subtitle =md("Participant P16208_4")
  )
Test_rt_ACC 
Test_rt_ACC  |> gtsave('Detect_Test_Prop.png')

Test_ON_ACC <-detect_exp

Test_Loc_ACC <-detect_exp

Old_tbl <- proportions(table(detect_exp$ACC_ON))
Old_tbl<- as.data.frame(prop.table(Old_tbl))
Old_tbl <- gt(Old_tbl )
Old_tbl
Old_tbl <- 
  Old_tbl |>
  tab_header(
    title = md("**ALL OLD NEW Test Accuracy Proportions**"),
    subtitle =md("Participant P16208_4")
  )
Old_tbl
Old_tbl |> gtsave('OLDNEW_Prop.png')


Old_Rand_Table <- detect_exp %>% 
  filter(Cond == "random")
Old_Rand_Table <- Old_Rand_Table %>% 
  filter(Block == 'detect_T')

Old_Rand_Table <- proportions(table(Old_Rand_Table$ACC_ON))
Old_Rand_Table
Old_Rand_Table<- as.data.frame(prop.table(Old_Rand_Table))
Old_Rand_Table <- gt(Old_Rand_Table)
Old_Rand_Table
Old_Rand_Table <- 
  Old_Rand_Table |>
  tab_header(
    title = md("**OLD NEW Test Accuracy Proportions Random Condiiton**"),
    subtitle =md("Participant P16208_4")
  )
Old_Rand_Table
Old_Rand_Table |> gtsave('Rand_OLDNEW_Prop.png')

Old_Fix_Table <- detect_exp %>% 
  filter(Cond == "fixed")
Old_Fix_Table
Old_Fix_Table <- Old_Fix_Table %>% 
  filter(Block == 'detect_T')

Old_Fix_Table <- proportions(table(Old_Fix_Table$ACC_ON))
Old_Fix_Table
Old_Fix_Table<- as.data.frame(prop.table(Old_Fix_Table))
Old_Fix_Table <- gt(Old_Fix_Table)
Old_Fix_Table
Old_Fix_Table <- 
  Old_Fix_Table |>
  tab_header(
    title = md("**OLD NEW Test Accuracy Proportions Fixed Condiiton**"),
    subtitle =md("Participant P16208_4")
  )
Old_Fix_Table
Old_Fix_Table |> gtsave('Fixed_OLDNEW_Prop.png')

Old_Test_Table <- detect_exp %>% 
  filter(Cond == "test")
Old_Test_Table

Old_Test_Table <- proportions(table(Old_Test_Table$ACC_ON))
Old_Test_Table
Old_Test_Table<- as.data.frame(prop.table(Old_Test_Table))
Old_Test_Table <- gt(Old_Test_Table)
Old_Test_Table
Old_Test_Table <- 
  Old_Test_Table |>
  tab_header(
    title = md("**OLD NEW Test Accuracy Proportions Test Condiiton**"),
    subtitle =md("Participant P16208_4")
  )
Old_Test_Table
Old_Test_Table |> gtsave('Test_OLDNEW_Prop.png')

Loc_tbl <- proportions(table(detect_exp$ACC_Loc))
Loc_tbl<- as.data.frame(prop.table(Loc_tbl))
Loc_tbl <- gt(Loc_tbl )
Loc_tbl

Loc_tbl <- 
  Loc_tbl |>
  tab_header(
    title = md("**Location Test Accuracy Proportions ALL Condiiton**"),
    subtitle =md("Participant P16208_4")
  )
Loc_tbl
Loc_tbl |> gtsave('ALL_Loc_Prop.png')

test_tbl <-proportions(table(detect_exp$ACC, detect_exp$ACC_ON, detect_exp$ACC_Loc))
test_tbl
test_tbl<- as.data.frame(prop.table(test_tbl))
test_tbl <- gt(test_tbl )
test_tbl

test_tbl <- 
  test_tbl |>
  tab_header(
    title = md("**Test Accuracy Proportions**"),
    subtitle =md("Participant P16208_4")
  )

#test_tbl <- test_tbll[order('Var1')]
test_tbl <- 
  test_tbl |>
  cols_label(
    Var1 = html("Tone detect"),
    Var2 = html("Old / New"),
    Var3 = html("Location"),
    Freq = html("Proportion")
  )

test_tbl

test_tbl |> gtsave('Test_ACCs_Props.png')

detect_exp_plt <- detect_exp_plt %>% 
  filter(Block == 'detect_1')

(plot_3 <- ggplot(data = detect_exp_plt, aes(x = Rep, y = rt, fill = Cond)) +
    geom_violin() +
    stat_summary(fun = mean, geom = "point", position = position_dodge(width = .9))  +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .15, position = position_dodge(width = .9))
)
ggsave('P31_Violin_Exp_Block_rt.jpeg', units = 'in', width = 5.5, height = 6)

#summary(plot_3)

freq_tbl <- detect_exp |>
  dplyr::group_by(Cond, Onset, Block, Side) |> 
  dplyr::summarise(
    count <- (frequency = n()))

arrange(freq_tbl, desc(count <- (frequency = n()))) %>% 
  print(n=100)
freq_tbl %>% 
  print(n=100)

grp_frq_dist <- detect_exp |> 
  dplyr::mutate(
    time_group = ggplot2::cut_width(rt, 100)
  ) |> 
  dplyr::group_by(time_group) |> 
  dplyr::summarise(
    frequency = n()
  ) |> 
  dplyr::mutate(
    relative_freq = frequency/sum(frequency),
    percent = relative_freq*100
  )
grp_frq_dist

block_dist <- detect_exp %>% 
  dplyr::group_by(Wav, Block, Cond, Side) |>
  dplyr::summarize(
    count <- (frequency = n())
  )

print(block_dist, n=180)
  
  freq_tbl <- detect_exp |>
  dplyr::group_by(Cond, Onset, Block, Side) |> 
  dplyr::summarise(
    count <- (frequency = n()))
  
#FIN
#
#
#
