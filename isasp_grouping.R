### packages ###
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)

#### read in data ####


## Contains the overal Subject for each subtest
## as well subtest abbreviation and title

testNames <- read_excel("ISASP test names.xlsx") 

## This data frame is from the ISASP extract reordred into a tidy format for easier processing

tidy_ISASP <- read_csv("C:/Users/delabruerejosh/Downloads/Tidy ISASP data - Sheet1 (2).csv", 
                       col_types = cols(Asian = col_logical(), 
                                        ELL = col_logical(), 
                                        Homeless = col_logical(), 
                                        T1L = col_logical(), 
                                        T1M = col_logical(), 
                                        africanAmerican = col_logical(), 
                                        americanIndianorAlaskan = col_logical(), 
                                        freeAndReducedLunch = col_logical(), 
                                        gifted = col_logical(), 
                                        hawaiianPacificIslander = col_logical(), 
                                        hispanicLatino = col_logical(), 
                                        militaryConnected = col_logical(), 
                                        plan504 = col_logical(), specialEd = col_logical(), 
                                        testLabel = col_character(), white = col_logical()
                                        )
                       )



#### Data Preperations ####

## Get the averages by school by grade for each subtest
tidy_ISASP %>% 
  group_by(testLabel, grade, school)%>%
  summarize(mean_score=mean(testPct), median_score=median(testPct)) %>%
  left_join(testNames) -> subtestAverages

## Group by school and grade for future manipulation
tidy_ISASP %>%
  group_by(school, grade) -> groupedISASP

#### Making Plots ####
## Setting School, Grade, and Subject
## gradeVal is the grade
## schoolVal name of the school. 
## Valid Values: "BRIDGEVIEW ELEMENTARY SCHOOL","CODY ELEMENTARY SCHOOL", "HOPEWELL ELEMENTARY","PLEASANT VIEW ELEMENTARY SCHOOL", 
##"RIVERDALE HEIGHTS ELEM SCHOOL", "PLEASANT VALLEY  JUNIOR  HIGH SCHOOL", "PLEASANT VALLEY HIGH SCHOOL"
## subjectVal is one of 4 values: "Reading", "Language/Writing", "Math", and "Science"
## Science is only for grades 5,8,and 10

gradeVal  <-  7
schoolVal <- "PLEASANT VALLEY  JUNIOR  HIGH SCHOOL"
subjectVal <- "Reading"


##  Plot median scores for each subtest by selected grade, and school. 
subtestAverages %>% 
  filter(school==schoolVal) %>% ##filter to the desired school
  filter(grade==gradeVal) %>% ##filter the desired grade
  filter(!is.na(Subject)) %>%  ##remove NA's from subject
  ggplot(aes(y=median_score, x=testTitle))+  
  geom_bar(stat="identity", fill="#003569")+
  geom_text(aes(label=paste0(median_score,"%")), hjust=1.25, color="#D6D6D6", size=3.5)+
  ylim(0, 100)+   # set the scale
  labs(x="", y="")+ 
  coord_flip()+
  theme_PleasVal()+
  theme(aspect.ratio = .25, # set the aspect ratio h:w 1:4
        axis.text.y = element_text(hjust=0))+  #aligns the test names to the left. 
  facet_wrap(vars(Subject), scales="free", ncol=1) ## this pulls out each subject in 1 column
## save the output as pdf in portrate mode, open in illustrator and align the text. 
##Then copy indivudal plots into the document


## Violin plots of math scores by schools grades <6

groupedISASP %>% 
  filter(grade<=6) %>%
  ggplot(aes(x=school, y=scaleMath))+
  geom_violin(aes(fill=school))+
  geom_boxplot(width=0.1)+
  facet_wrap(vars(grade))+
  theme_PleasVal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
        )+
  theme(aspect.ratio = .9,legend.position="bottom")+
  guides(fill=guide_legend(ncol=2,bycol=TRUE, title= "School"))+
  labs(y="Scale Score Math", title="Elementary ISASP Math Scale Score Distribution")


## violin plots of ELA scores by schools grades <6
groupedISASP %>% 
  filter(grade<=6)%>%
  ggplot(aes(x=school, y=scaleELA))+
  geom_violin(aes(fill=school))+
  geom_boxplot(width=0.1)+
  facet_wrap(vars(grade))+
  theme_PleasVal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(aspect.ratio = .9,legend.position="bottom")+
  guides(fill=guide_legend(ncol=2,bycol=TRUE, title= "School"))+
  labs(y="Scale Score ELA", title="Elementary ISASP ELA Scale Score Distribution")



# groupedISASP %>% 
#   filter(grade<=6)%>%
#   ggplot(aes(x=school, y=scaleELA))+
#   geom_violin(aes(fill=school))+
#   geom_violin(aes(x=school, y= scaleELA[filter(grade<=6)]))
#   geom_boxplot(width=0.1)+
#   theme_PleasVal()+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())+
#   theme(aspect.ratio = .9,legend.position="bottom")+
#   guides(fill=guide_legend(ncol=2,bycol=TRUE, title= "School"))+
#   labs(y="Scale Score Math", title="Elementary ISASP Math Scale Score Distribution")



## this is an attempt at stacking a schools number of students in each category. It is not done. 
groupedISASP %>%
  left_join(testNames)%>%
  filter(school==schoolVal) %>%
  filter(grade==gradeVal)%>%
  filter(!is.na(levelELA)) %>%
  mutate(levelELA=as.factor(levelELA))%>%
  ggplot(aes(x=levelELA))+
  geom_bar(aes(fill=levelELA), position = position_stack())

## density plot and histogram for a specified grade.  
## this is more of a sketch
groupedISASP %>%
  filter(school==schoolVal) %>%
  filter(grade==gradeVal)%>%
  ggplot()+
    geom_histogram(aes(x=scaleReading, y= ..density..), col="#011638", fill="#D6D6D6")+
    geom_density(aes(x=scaleReading), alpha=.2, col="#011638", fill="#011638", size=1)+
  # geom_vline(xintercept=453, size=1)+
    theme_minimal()+
    labs(title = paste0("Density plot of PVCSD " ,gradeVal, " Reading"))+
    ylim(0,0.03)

## density plots colored by the range, currently you need to manually update the ranges for proficency
groupedISASP %>%
  filter(school==schoolVal) %>%
  filter(grade==gradeVal)%>%
    ggplot()+
  geom_density(aes(x=scaleELA), alpha=.1, size=3)->p

d <- ggplot_build(p)$data[[1]]
p + geom_area(data=subset(d, x<=455), aes(x=x, y=y), fill="Red", alpha=.5)+
  geom_area(data=subset(d, x<541&x>455), aes(x=x, y=y), fill="gold", alpha=.5)+
  geom_area(data=subset(d, x>=541), aes(x=x, y=y), fill="forest green", alpha=.5)

