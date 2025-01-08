#------------------#
#title: "Ex3-Combo"
#author: "A Sampson"
#date: "`Script as of 091124" DDMMYY
#note: Takes "combo csv" files produced by Cleaning Script as Pz_BX and Txt_BX and similar and produces combined data frame 
#version information: There are 4 versions - the only difference in the four versions is which stimuli are assigned to which condition. Simply to ensure all stimuli appear the same number of times across conditions 

#Columns for rep number (AKA block), Version number, rmSq are already in
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

#---- Load Libraries ---

library(tidyverse)
library(skimr)
library(ggplot2)
library(gt)
library(gtsummary)
library(apaTables)
library(plotrix)
library(ez)
library(Hmisc)
#Read in the csvs - for this pilot incarnation there are 2 csv taken from the 14 output files from Presentation - the csv designated "16B" contains the exposure blocks data, the csv designated "16T" contains the test block data

##----- Set participant and Version -----
## input subject range, version, and number of subjects 
## these will populate file names and graphic titles
Subject <- "Ps 1 - 12"
Version <- "One thru Four"
SbjNum <- "12"

###---- read files ----

files <- getwd()
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

###THIS WILL PRODUCE AN ERROR _ HIT THE NEXT LINE

#This here is the activation for the bind above
tbl = lapply(files, read_csv) %>% bind_rows()

dat_all <- tbl
unique(dat_all$Sbj)

colnames(dat_all)
sapply(dat_all, class)
##Harmonize colnames
#dat_all <- read_csv("18_Subjects_Exp2.csv") %>% 
#  rownames_to_column()

#clean out unneeded columns
dat_all <- dat_all %>% 
  select(Sbj, Block, TrialNum, Wav, Cond, Side,Location, Onset, Resp, rt, ACC, Resp_Old, rt_Old, ACC_ON, Resp_Time, rt_Time, ACC_Time, TTime, KTime, ex_rt_median, ex_rt_stdev, ex_rt_Outlier, Time_Trial_Target, Time_Response, StimCon, FileName,  RT_Chk, Time_Trial_Cue, Time_Trial_Target, Time_Response, Time_Trial_Q_ON, TTime_Trial_Cue, TTime_time_response, Ver, Rep, RmSq) 

#save data frame and compiles csv
save(dat_all, file=(paste(SbjNum,'subjects_Exp3.Rda', sep = '_'))) #data frame
write_csv(dat_all, (paste(SbjNum, 'Subjects_Exp3.csv', sep = '_'))) #csv
#you will want to do this again after you have calculated the group outliers and rebound

unique(dat_all$Sbj)

#----Split and Describe ---- RAW 
#grouped by participant and condition without test

missing.values <- dat_all %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))


dat_exp <- dat_all %>% 
  filter(Block != 'detect_T')

dat_test <- dat_all %>% 
  filter(Block == 'detect_T')

#Learning accuracy proportions by participant

#Pivoting to assign outliers - select relevant columns - remove NA rt values - then z scores
dat_smol <- dat_exp %>% 
  select(Sbj, Block, Rep, TrialNum, Wav, Cond, rt, ACC, ex_rt_Outlier)

dat_smol <- dat_smol %>% 
  arrange(Block) %>% 
  group_by(Sbj) %>% 
ungroup()

dat_wide <- dat_smol %>% 
  pivot_wider(
    id_cols = c(Sbj),
    names_from = c(Block, Rep, TrialNum  ),
    names_sep = ".",
    values_from = c( rt, ACC, Cond,ex_rt_Outlier, Wav)
  )

dat_smol <- dat_smol %>% 
  mutate(rt_log = log(rt))

psych::pairs.panels(dat_smol)

plot_res <-ggplot(dat_smol, aes(Block, rt_log, colour=Cond)) + geom_point() +
  facet_grid(.~Sbj) +
  geom_smooth(method = "lm") + 
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Subject Raw by Block")
plot_res
ggsave(paste(SbjNum, "p_Subject_x_Block_Exp3.png", sep =''))

dat_sbj <- dat_smol %>% 
  group_by(Sbj)
fit_sbj <- lm(rt_log ~ Cond, data = dat_sbj)
plot(fit_sbj, which = 3)

plot(fit_sbj, which = 2)

shapiro.test(resid(fit_sbj)) #- we know not normal

summary(fit_sbj)

dat_sbj$rt_log_c <- scale(dat_sbj$rt_log, scale = F)
fit_Rep_c <- lm(rt_log_c ~ Rep, data = dat_sbj)
summary(fit_Rep_c)

sqrt(summary(fit_Rep_c)$r.squared)
summary(lm.beta::lm.beta(fit_Rep_c))

car::Anova(fit_Rep_c$lm, type = "III", 
           contrasts = list(Rep = contr.sum, rt = contr.sum),
           white.adjust = TRUE)

ggplot(dat_sbj, aes(Block, rt_log, colour =Cond)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggthemes::theme_few()

fit_rt <- lm(rt ~ Block, data = dat_smol)
summary(fit_rt)

psych::outlier(dat_smol[, c("rt", "Rep")])


dat_smol_means <- dat_smol %>% 
  group_by(Block, Cond) %>% 
  summarise(mean_rt = mean(rt, na.rm = TRUE),
            med_rt = median(rt, na.rm = TRUE),
            SE_rt = std.error(rt,na.rm = TRUE),
            count =n(),
            SD_rt = sd(rt, na.rm = TRUE)) %>% 
  ungroup()


dat_smol_Qd <- dat_smol[-c(696, 881, 1099, 1375, 1440),]
psych::outlier(dat_smol_Qd[, c("rt", "Rep")])
dat_smol_Qd <- dat_smol[-c(1185, 1204, 1433, 1465),] #did not remover 197


ggplot(dat_sbj, aes(Block, rt_log_c, color = Cond)) + geom_point() + facet_grid(.~Sbj)+
  geom_smooth(method = "lm") + ggthemes::theme_few() + theme(axis.text.x = element_text(angle=90)) + labs(title = "Centred RT Raw")
ggsave(paste(SbjNum, "p_Subject_Cntrd_x_Block_Exp3.png", sep = ''))

#----- Repeating with cleaned -----

dat_smol <- dat_smol %>% 
  mutate(rt_log = log(rt))

dat_smol <- dat_smol %>% 
  na.omit(rt) %>% 
  filter(ACC == 1,
         rt > 200,
         ex_rt_Outlier == 0)


dat_smol <- dat_smol %>% 
  filter(rt > 200) %>% 
  group_by(Cond, Block) %>% 
ungroup()

dat_smol <- as_tibble(dat_smol)
dat_smol

psych::describe(dat_smol) 
dat_sbj$rt_log_c <- scale(dat_sbj$rt_log, scale = F)

dat_test <- dat_test %>% 
  mutate(rt_log = log(rt))

dat_T <- dat_test %>% 
  filter(rt > 200)


psych::describe(dat_T)


###----TEST----- TEST-----TEST-----

dat_test_plt <- ggplot(dat_test, aes(x = Cond, y = rt_log, fill = Cond, color = Cond)) 
dat_test_plt + geom_boxplot(width = 1, alpha= .7, notch=FALSE) +  guides(x = guide_axis(angle = 90)) + labs(title = "Exp 3 Test 12P Responses", x = "Condition", y = "Average rt") + theme_minimal()
#Save ALL rts ALL Cond plot
ggsave(paste(SbjNum, '_Subjects_Exp3_ALL_Test_rt.jpeg', sep = ''), units = 'in', width = 5.5, height = 6)

dat_test_correct <- dat_test %>% 
  filter(ACC == 1)
dat_test_correct <- dat_test_correct %>% 
  filter( rt > 200)

write_csv(dat_test_correct, (paste(SbjNum, '_Subjects_Exp3_Correct_Test_rt.csv', sep = '')))

dat_test_plt <- ggplot(dat_test_correct, aes(x = Cond, y = rt_log, fill = Cond, color = Cond)) 
dat_test_plt + geom_boxplot(width = 1, notch=FALSE, alpha = .7) +  guides(x = guide_axis(angle = 90)) + labs(title = "Exp 3 Test Correct rt Responses 9P", x = "Condition", y = "Average rt") + theme_minimal()
ggsave(paste(SbjNum, '_Subjects_Exp3_Correct_Test_rt.jpeg', sep = ''),  units = 'in', width = 5.5, height = 6)

dat_test_summary <- dat_test_correct %>% 
  group_by(Block, Cond )


dat_summary_test <- dat_test_summary %>%
  summarise(
    TC_mean=mean(rt, na.rm = TRUE),
    TC_med=median(rt, na.rm = TRUE),
    TC_SErt = std.error(rt,na.rm = TRUE),
    TC_std_dev=sd(rt,na.rm=TRUE),
                 n = n()) %>% 
ungroup()
dat_summary_test

dat_summary_test <- as.data.frame((dat_summary_test))

save(dat_summary_test, file=(paste(SbjNum, '_subjects_Exp3_SumTest.Rda', sep = ''))) #data frame
write_csv(dat_summary_test, (paste(SbjNum, '_Subjects_Exp3_SumTest.csv', sep = '')))

dat_test_means <- read_csv('13_Subjects_Exp3_SumTest.csv')

dat_test <- dat_test %>% 
  group_by(Sbj, Cond) %>% 
  summarise(mean = mean(ACC, na.rm = TRUE),
            std.error = std.error(ACC,na.rm = TRUE),
            sd = sd(ACC, na.rm = TRUE),
            count =n()
  ) %>% 
ungroup()
print(dat_test, n=80)

dat_sum_sum <- dat_test_correct %>% 
  group_by(Sbj, Cond) %>% 
      summarise(
         mean = mean(rt, na.rm = TRUE),
         median = median(rt, na.rm =TRUE),
         std.error = std.error(rt,na.rm = TRUE),
         sd = sd(rt, na.rm = TRUE),
         count =n()
 )

as.data.frame(dat_sum_sum)
#### Test correct - not t-testable - comment out 

dat_sum <- dat_sum_sum %>% 
  filter(Cond != "test")

t.test(mean ~ Cond, data = dat_sum, paired = TRUE, alternative = "two.sided")

Test_lm <- lm(formula = rt ~ Cond, data = dat_test_correct)
Test_lm %>% 
  summary

AllCond_lm <- lm(formula = rt_log ~ Cond , data = dat_test_correct)
AllCond_lm %>% 
  summary

library(lsr)

cohensD(mean ~ Cond, 
        data = dat_sum)

#-----EXP-----EXP-----EXP-----
#now clean out errors, <.200 and outliers
dat_exp <-dat_exp %>% 
  filter(ACC == 1)
dat_exp <- dat_exp %>% 
  filter( rt > 200)
dat_exp <- dat_exp %>% 
  filter(ex_rt_Outlier==0) #filtering outliers at the level of individual within the group
hist(dat_exp$rt) 


cohensD(mean_rt ~ Cond, 
        data = dat_smol_means)

results <- lm(formula = rt ~ Cond * Side, data = dat_exp)
results %>% 
  summary

dat_paired <- dat_exp 

dat_paired <- dat_paired %>% 
  group_by (Sbj, Cond) %>% 
  summarise(mean_rt = mean(rt, na.rm = TRUE),
            med_rt = median(rt, na.rm = TRUE),
            SE_rt = std.error(rt,na.rm = TRUE),
            count =n(),
            SD_rt = sd(rt, na.rm = TRUE),
            Part = "Exp")


t.test(mean_rt ~ Cond, data = dat_paired, paired = TRUE, alternative = "two.sided")

print(dat_paired, n= 60)
unique(dat_paired$Sbj)
write_csv(dat_paired, (paste(SbjNum,'_by_paired_Exp3.csv', sep =''))) #csv
colnames (dat_paired)

###----- Test Continues ??? why here? - Ah create csvs for JASP

#recall the test values are raw
##>>>>>>>Location Subset 
dat_test_Loc <- dat_T %>% 
  filter(ACC_Time == 1) %>% 
  group_by(Sbj, Cond) %>% 
  summarise(mean_rt_on = mean(rt_Time, na.rm = TRUE),
            med_rt = median(rt_Time, na.rm = TRUE),
            SE_rt = std.error(rt_Time,na.rm = TRUE),
            count =n(),
            SD_rt = sd(rt_Time, na.rm = TRUE),
            Part = 'Test')
print(dat_test_Loc, n= 50)
write_csv(dat_test_Loc, (paste(SbjNum,'_correct_Loc_by_paired_Exp3.csv', sep = ''))) #csv 


##>>>>>>>>Old New subset
dat_test_ON <- dat_T %>% 
  filter(ACC_ON == 1) %>% 
  group_by(Sbj, Cond) %>% 
  summarise(mean_rt_on = mean(rt_Old, na.rm = TRUE),
            med_rt = median(rt_Old, na.rm = TRUE),
            SE_rt = std.error(rt_Old,na.rm = TRUE),
            count =n(),
            SD_rt = sd(rt_Old, na.rm = TRUE),
            Part = 'Test')
print(dat_test_ON, n= 36)
write_csv(dat_test_ON, (paste(SbjNum,'_correct_ON_by_paired.csv', sep = ''))) #csv 


#grouped_paired_data <- rbind(dat_paired, dat_test_group)
#write_csv(grouped_paired_data, '23_exp_correct_test_by_paired.csv') #csv 


dataset <- read_csv('12_by_paired_Exp3.csv')

dataset = read_csv("12_by_paired_exp_prepped.csv")
colnames(dataset)
dataset  <- dataset %>% 
  select(Sbj,Cond, mean_rt)

  
dataset <- group_by(Sbj, Cond)

dataset %>% 
  t.test(fixed_rt, random_rt, data =dataset, paired =TRUE, var.equal = TRUE ) 

library(lsr)

cohensD(mean_rt ~ Cond, 
        data = dataset)

 
 t.test(rt ~ Cond, data = test, paired = FALSE, alternative = "two.sided") 


 
dat_exp <- mutate(dat_exp, 
                  grp_rt_median = median(rt, na.rm=T), 
                  grp_rt_stdev = sd(rt, na.rm=T), 
                  grp_rt_Outlier = 1*(abs(rt - grp_rt_median)>2*grp_rt_stdev))
hist(filter(dat_exp, grp_rt_Outlier==0)$rt)
#save exposure only df and csv
save(dat_exp, file=(paste(SbjNum,'_Sbj_grpout_Ex3.Rda', sep = ''))) #data frame
write_csv(dat_all, (paste(SbjNum,'_Sbj_grpout_Ex3.csv', sep =''))) #csv

#now clean out errors, <.200 and outliers
dat_exp <-dat_exp %>% 
  filter(ACC == 1)
dat_exp <- dat_exp %>% 
  filter( rt > 200)
dat_exp <- dat_exp %>% 
  filter(ex_rt_Outlier==0) #filtering outliers at the level of individual within the group
hist(dat_exp$rt)

#descriptives

dat_exp_cond <-dat_exp %>% 
  group_by(Cond)

 
dat_exp_cond %>% 
  summarise(mean_rt = mean(rt, na.rm = TRUE),
            med_rt = median(rt, na.rm = TRUE),
            SE_rt = std.error(rt,na.rm = TRUE),
            count =n(),
            SD_rt = sd(rt, na.rm = TRUE))

Cond_lm <- lm(formula = rt ~ Cond, data = dat_exp_cond)
Cond_lm %>% 
  summary
AllCond_lm <- lm(formula = rt ~ Cond + Side + Onset, data = dat_exp_cond)
AllCond_lm %>% 
  summary

t.test(rt ~ Cond, data = dat_exp_cond, paired = TRUE, alternative = "two.sided") 


boxplot(rt~Cond,
        data=dat_exp_cond,
        main="rt by Cond",
        xlab="Cond",
        ylab="rt",
        col="steelblue",
        border="black"
)

dat_exp_sbj <- dat_exp %>% 
  group_by(Sbj)

dat_exp_sbj%>% 
  t.test(rt~Cond, data = ., var.equal = FALSE) 

dat_exp %>% 
  t.test(rt~Cond, data =., paired = TRUE, var.equal = FALSE ) 

results <- t.test(rt ~ Cond, 
                  data = dat_exp, 
                  paired = TRUE, 
                  alternative = "two.sided") 
results %>% 
  summary

unique(dat_exp$Sbj)
#now figure out the revised condition rts with plots and table - scatter by participant
geom_jitter <- position_jitter(width = 0.1)
datp_Block_scat <- ggplot(dat_exp, aes(x = Cond, y = rt , fill = Cond, color = Cond)) 
datp_Block_scat + geom_point() + theme_grey() + geom_jitter()  +
  facet_wrap(vars(Sbj)) +
  labs(title="Participant Average rt - Learning")
ggsave('Cond_AvgAvg.png', units = 'in', width = 6, height = 6)

#Cond by all
FLT_cond_block <- ggplot(dat_exp, aes(x = Cond, y = rt, fill = Cond)) +  
  geom_boxplot()  + 
  labs(title = "Condition All Subjects All Learning", x = "Condition", y = "rt in milliseconds") + 
  facet_wrap(vars(Sbj))+
  theme(axis.text.x = element_text( color ="black", size = 9, angle= 90),
        axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_cond_block

# ----- Save Plot Without Outliers

ggsave('Allcond_means_Box_2_Ex3.png', units = 'in', width = 6, height = 6)

#print related table
rt_summary <-dat_test_correct %>%
  group_by(Block, Cond) %>%
  summarise(mean=mean(rt,na.rm=TRUE),
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
    title = md(paste(SbjNum, "**Sbj Correct rt Averages Test Exp3**", sep = ' '))
  )
rt_summary |> gtsave(paste(SbjNum,'Sbj_AVG_Correct_rt_Test_Exp3.png'), expand = 10)

#FIN
halt

#---- indy participants ---
unique(dat_all$Sbj)

dat_all <- dat_all %>% 
  group_by(Block, Cond)

dat_all <- dat_all %>% 
  filter(ACC == 1) %>% 
  filter( rt >= 200) %>% 
  filter(ex_rt_Outlier == 0)

dat_exp_only <- dat_all %>% 
  filter(Block != "detect_T")

FLT_datp_Block_scat <- ggplot(dat_all, aes(x = Block, y = rt, fill = Cond, color=Cond)) + geom_point() + labs(title = (paste("Clean rt exp3", SbjNum,"P", sep = '')), x = "Block", y = "rt in milliseconds") + theme_grey() + geom_jitter() + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_datp_Block_scat

ggsave('All_cond_block_Scat.png', units = 'in', width = 5, height = 5)

FLT_cond_block <- ggplot(dat_all, aes(x = Block, y = rt, fill = Cond)) +  geom_boxplot()  + labs(title = (paste("Condition by Block rt exp3", SbjNum,"P", sep = '')), x = "Block", y = "rt in milliseconds")  + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_cond_block

# plot by condition only
FLT_cond <- ggplot(dat_all, aes(x = Cond, y = rt, fill = Cond)) +  geom_boxplot()  + labs(title = "Condition Only by Block", x = "Block", y = "rt in milliseconds")  + theme(
  axis.text.x = element_text( color ="black", size = 9, angle= 90),
  axis.text.y = element_text( color="black", size=8, angle = 0))
FLT_cond
ggsave('All_Cond.png', units = 'in', width = 5, height = 5)

rt_summary <-dat_exp %>%
  group_by(Cond) %>%
  summarise(mean=mean(rt,na.rm=TRUE),
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
    title = md(paste("**Mean, Median & SD AVG rt by Condition**", SbjNum, "P**", sep = '')),
    subtitle =md(" All Correct Exposure Trials Exp3")
  )
rt_summary |> gtsave('AVG_rt_Cond_Means.png', expand = 10)

rt_summary <-dat_all %>%
  group_by(Block, Cond ) %>%
  summarise(mean=mean(rt,na.rm=TRUE),
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
    title = md(paste("** Mean, Median & SD AVG rt by Condition**", SbjNum, "P**", sep = '')),
    subtitle =md("All Correct Exposure Trials Exp3")
  )
rt_summary |> gtsave('ALL_AVG_rt_Cond_Means.png', expand = 10)



ggplot(dat_all, aes(x=Block, y = ACC, fill=Cond)) +
  geom_bar(stat="identity", position = position_dodge()) + labs(title = 'Accuracy by Block', x = 'Block', y = 'Proportion')
ggsave('ALL-ACCxBlock_exp3.png')
class(dat_exp$ACC)
dat_ACC <- dat_all %>% 
  filter(Block!="detect_T") %>% 
  group_by(ACC)
dat_ACC$ACC <- as.factor(dat_ACC$ACC)
ggplot(dat_ACC, aes(x=ACC, fill=Cond)) +
  geom_bar(position = 'fill') + labs(title = 'Accuracy by Condition', x = 'ACC', y = 'Cond')
ggsave('ALL_ACC_COND.png')

###-------Filter out individual pariticipants because why not?
dat1 <- dat_all %>% 
  filter(Sbj == 'P16101_030823')

dat2 <- dat_all %>% 
  filter(Sbj == 'P16102_040823')

dat3 <- dat_all %>% 
  filter(Sbj == 'P16103_040823')

dat4 <- dat_all %>% 
  filter(Sbj == 'P16104_040823')

dat5 <- dat_all %>% 
  filter(Sbj == 'P16105_090823')

dat6 <- dat_all %>% 
  filter (Sbj == 'P16106_100823')

dat7 <- dat_alll %>% 
  filter(Sbj == 'P16107_100823')

dat8 <- dat_alll %>% 
  filter(Sbj == 'P16108_110823')

dat9 <- dat_all %>% 
  filter(Sbj == 'P16109_110823')

dat10 <- dat_all %>% 
  filter(Sbj == 'P16110_110823')

dat11 <- dat_all %>% 
  filter(Sbj == 'P16110_150823')

dat12 <- dat_all %>% 
  filter (Sbj == 'P16112_150823')

dat13 <- dat_all %>% 
  filter(Sbj == 'P16113_180823')

dat14 <-dat_all %>% 
  filter(Sbj == 'P16114_180823')

dat15 <- dat_all%>% 
  filter (Sbj == 'P16115_180823')

dat16 <- dat_all%>% 
  filter (Sbj == 'P16116_230823')

dat17 <- dat_all%>% 
  filter (Sbj == 'P16117_290823')

dat18 <- dat_all%>% 
  filter (Sbj == 'P16118_290823')

dat19 <- dat_all %>% 
  filter(Sbj == 'P16119_231123')

dat20 <- dat_all %>% 
  filter(Sbj == 'P16120_201123')

dat21 <- dat_all %>% 
  filter(Sbj == 'P16121_111223')

dat22 <- dat_all %>% 
  filter (Sbj == 'P16122_012524')

dat23 <- dat_all %>% 
  filter(Sbj == 'P16123_013124')

dat24 <-dat_all %>% 
  filter(Sbj == 'P16124_020824')

dat25 <- dat_all%>% 
  filter (Sbj == 'P16125_120224')

dat26 <- dat_all%>% 
  filter (Sbj == 'P16126_130224')

dat27 <- dat_all%>% 
  filter (Sbj == 'P16127_021524')

dat28 <- dat_all%>% 
  filter (Sbj == 'P16128_021524')

dat29 <- dat_all%>% 
  filter (Sbj == 'P16129_022224')

dat30 <- dat_all%>% 
  filter (Sbj == 'P16130_022224')

###------ANOVA Prep -----

dat_all <- dat_all %>% 
  filter(ex_rt_Outlier == 0)

dat_anv1 <- dat_all %>% 
  select(Sbj, Block, TrialNum,  Cond, rt,  Ver, Rep) 



dat_anv1$Pos <- paste(dat_anv1$Block, dat_anv1$Rep, dat_anv1$TrialNum)

####---from sheffield)

dat_anv2 <- dat_anv1 

dat_anv2$Cond <-factor(dat_anv2$Cond)

anv1 <- ezANOVA(data = dat_anv2, dv =. (rt), wid=.(Sbj), within=.(Cond))
  
dat_all %>% count(Onset)

dat_rnd <- dat_all %>% 
  filter(Cond=="random")

dat_fx <- dat_all %>% 
  filter(Cond=="fixed")

dat_rnd %>% count(Onset)
dat_fx %>%  count (Onset)

dat_all %>% count(Onset)

dat_anv4 <- dat_anv1 %>%
  pivot_wider(
    names_from = c(Block, Rep, TrialNum  ),
    names_sep = ".",
    values_from = c(Cond, rt)
  )

dat_anv5 <- dat_anv1 %>%
  pivot_wider(
    names_from = c(Block, Rep, TrialNum, ),
    names_sep = ".",
    values_from = c( rt)
  )

write_csv(dat_anv3, "rt_anova_30P.csv")


dat_anv1 %>%
  pivot_wider(
    names_from = TrialNum,
    names_sep = ".",
    values_from = c( Cond, Side, Onset, Resp, rt, ACC, Resp_Old, rt_Old, ACC_ON, Resp_Loc, rt_Loc, ACC_Loc, Ver)
  )

dat_anv1 <- dat_anv1 %>%
  pivot_wider(names_from = Sbj, Block, Rep, values_from = c(TrialNum, Wav, Cond, Side, Onset, Resp, rt, ACC, Resp_Old, rt_Old, ACC_ON, Resp_Loc, rt_Loc, ACC_Loc, Ver ))

class(dat_anv1$TrialNum)   

print(detect_T)

##FIN

