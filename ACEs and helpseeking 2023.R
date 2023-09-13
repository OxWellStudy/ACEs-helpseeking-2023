## This analysis uses OxWell data (oxwell.org; osf.io/sekhr) to explore the
##    mental health helpseeking patterns of young people aged 16-18 years by
##      self-reported experiences of adversity. 


library(readxl)
library(writexl)
library(tidyverse)
library(expss)
library(lme4)
library(DT)
library(standardize)
library(lmerTest)
library(ggplot2)
library(merTools)
library(compareGroups)
library(createTable)
library(merTools)
library(glmmLasso)
library(reshape2)
library(cyphr)
library(ggpubr)

#Load in data (cyphr) 
if ( 0 ) {
  OXWELL.DATA.DIR <- "" #  path to OxWell data
  PERSONAL.SSH.DIR <- "" # path to personal ssh key (for decrypting OxWell data)
  PERSONAL.DIR <- "" # path to personal wd 
  ADMIN.DFE <- "" # path to DFE data (publicly available, in GitHub files)
  ADMIN.ONS <- "" # path to ONS data (publicly available, in GitHub files)
} else {
  source("local.R")
}

## The following line decrypts the data-key
## NOTE: for additional security, cyphr stores a session-specific decrypted version of the key
##       you will need to decrypt the data-key for each session
##
## The following will ask you for your ssh-rsa keyphrase

KEY <- cyphr::data_key(path_data=OXWELL.DATA.DIR,path_user=PERSONAL.SSH.DIR)

if( 0 ) {
  ## Test that encryption/decryption is working
  cyphr::decrypt_string(data=file.path(OXWELL.DATA.DIR,"test.string-cyphr"), key=KEY )
}

DATA <- cyphr::decrypt_object(key=KEY, data=file.path(OXWELL.DATA.DIR, "DATA.r01.rds-cyphr") ) #<- merged 2019-2021 data 

setwd(PERSONAL.DIR)

data<-DATA
data<-subset(data, SURVEY==2020)
data$yeargroup <- data$YEARGROUP
data<-subset(data, yeargroup=="12" | yeargroup=="13") #only years 12/13 are asked about ACEs --> N = 2360

#subset data to include only flgclean=Y --> this is how data is marked as usable by survey admin
table(droplevels(data$SCHOOL.ID.ORIGINAL)) #65 schools 
data<-subset(data,FLAG=="Y")
table(droplevels(data$SCHOOL.ID.ORIGINAL)) #64 schools (i.e. one school had no valid data)

############# SCHOOL LEVEL CHARACTERISTICS  ###########
# Link in DfE & ONS publicly available data (available as repository files)
DfE <-readRDS(ADMIN.DFE)
DfE <- subset(DfE, SURVEY==2020)
ONS<-readRDS(ADMIN.ONS)
data2<-merge(data,DfE,by="SCHOOL.LINK") # (decreases b/c some of the schools are N)
data2<-merge(data2,ONS,by="LSOA.LINK")

# Prepare school-level variables from admin data 
data2$imd <- data2$"Income Decile (where 1 is most deprived 10% of LSOAs)"  #1 = most deprived 
data2$imd_quint<- ifelse(data2$imd==1, "1",
                         ifelse(data2$imd==2, "1",
                                ifelse(data2$imd==3, "2",
                                       ifelse(data2$imd==4, "2",
                                              ifelse(data2$imd==5, "3",
                                                     ifelse(data2$imd==6, "3",
                                                            ifelse(data2$imd==7, "4",
                                                                   ifelse(data2$imd==8, "4", "5"))))))))

data2$schoolsex <- data2$"sex_of_school_description"
data2$schoolsex <- as.factor(data2$schoolsex)
data2$schoolsex <- relevel(data2$schoolsex, ref="Mixed")

data2$headcount <- data2$"headcount of pupils"

data2$type <- data2$"phase-type_grouping"

data2$urbanicity <- data2$"urban_rural" 
data2$urbanicity <- as.factor(data2$urbanicity)

data2$fsm <- data2$"% of pupils known to be eligible for free school meals (Performance Tables)"

data2$whitebritish<- data2$"number of pupils classified as white British ethnic origin"/data2$headcount*100 

# Descriptives (school level)
schooltable <- data2 %>% group_by(SCHOOL.ID.ORIGINAL) %>% filter (! duplicated(SCHOOL.ID.ORIGINAL))
table(schooltable$schoolsex)
table(schooltable$type)
table(schooltable$urbanicity)
median(schooltable$imd)
IQR(schooltable$imd)
mean(schooltable$headcount)
sd(schooltable$headcount)

# Exclude independent schools, which don't report the below information (recorded as 0% in data) 
mean(schooltable$fsm[schooltable$type!="Independent school"])
sd(schooltable$fsm[schooltable$type!="Independent school"])
mean(schooltable$whitebritish[schooltable$type!="Independent school"])
sd(schooltable$whitebritish[schooltable$type!="Independent school"])

# Comparison with all schools in England 
DfE_sec <- DfE[DfE$`phase-type_grouping`== "State-funded secondary"]
mean(DfE_sec$`% of pupils known to be eligible for free school meals (Performance Tables)`)
sd(DfE_sec$`% of pupils known to be eligible for free school meals (Performance Tables)`)

DfE_sec$headcount <- DfE_sec$"headcount of pupils"
mean(DfE_sec$headcount)
sd(DfE_sec$headcount)

DfE_sec$whitebritish <- DfE_sec$"number of pupils classified as white British ethnic origin"/DfE_sec$headcount*100 
mean(DfE_sec$whitebritish, na.rm=TRUE)
sd(DfE_sec$whitebritish, na.rm = TRUE)

############# MAIN ANALYSIS ############# 
##### VARIABLE PREPARATION #####
# MAIN INDEPENDENT: ACEs (adverse childhood experiences, recorded as sum score 0-10)
data$aces <- as.numeric(data$Q12_01) #number of ACEs
data$acesany <- ifelse(data$aces<1, 0, 1) #ACEs as a binary of none vs. any

# RCADS (pre-processed into a total score using scoring guidance)
data$rcadst <- data$RCADS.Total.tscore
data$rcadstbinary <- ifelse(data$rcadst<70,0,1) # from guidance, t-scores >= 70 probable disorder

# Create scaled RCADS var -- this is for numerical stability in the models as did not converge 
data$rcads_scaled <- scale(data$rcadst)

# OUTCOMES: access (rcvhelp) & perceived unmet need (benhelp)
data$rcvhelp <- ifelse(data$Q9_29=="N", 0, 
                       ifelse(data$Q9_29=="Y",1,NA)) #received help y/n
data$benhelp <- ifelse(data$Q9_32=="N",0, 
                       ifelse(data$Q9_32=="Y",1,NA)) #could have benefitted from help y/n

# COVARIATES
data$gender <- ifelse(data$D1_01=="Boy", 0, 
                      ifelse(data$D1_01=="Girl",1,NA))
data$gender <- as.factor(data$gender)

# Prepare free school meals data
data$fsm1 <- ifelse(data$Q1_13=="N", 0, 
                    ifelse(data$Q1_13=="Y",1,
                           ifelse(data$Q1_13=="D",2,NA))) #for use in descriptives table
data$fsm1 <- as.factor(data$fsm1)

# Prepare free school meals data where 'Don't know' = No
data$fsm <- ifelse(data$Q1_13=="N", 0, 
                   ifelse(data$Q1_13=="D",0, #for use in analyses
                          ifelse(data$Q1_13=="Y",1,NA))) #NULL->NA
data$fsm <- as.factor(data$fsm)

#As above, for where born
data$bornuk1 <- ifelse(data$D1_11=="Yes", 0, 
                       ifelse(data$D1_11=="No",1,
                              ifelse(data$D1_11=="Would rather not say",2,NA))) #for use in descriptives table
data$bornuk1 <- as.factor(data$bornuk1)

data$bornuk <-ifelse(data$D1_11=="Yes", 0, 
                     ifelse(data$D1_11=="No",1,NA))
data$bornuk<-as.factor(data$bornuk) # 'prefer not to say' counted as NA; for use in analyses

##### DESCRIPTIVES (individual participants) #####
# Make ACEs for summary table where you have 0,1,2,3,4+ as options
data$acescat <-data$aces
data$acescat[data$acescat>=4] <- "4+"

### Full sample table, N = 2018 ###
tableaces <- compareGroups(~ gender + 
                             yeargroup +
                             fsm1 +
                             bornuk1 +
                             aces +
                             acescat +
                             rcadst + rcads_scaled,
                           data = data, #all data
                           byrow = TRUE, include.miss = TRUE)
createTable(tableaces,show.ratio = TRUE)
summary(data$aces) #median

### Receive help outcome tables ###
# All with outome data on prior access, N = 2002
data_rcvhelp<-subset(data,(!is.na(rcvhelp)))
summary(data_rcvhelp$aces)
tableacesfull <- compareGroups(~ gender + 
                                 yeargroup +
                                 fsm1 +
                                 bornuk1 +
                                 aces +
                                 acescat +
                                 rcadst,
                               data = data_rcvhelp, #just those with data on outcome
                               byrow = TRUE, include.miss = TRUE)
createTable(tableacesfull)
summary(data_rcvhelp$aces)

# By prior acces Y/N, N = 2002
tableaces <- compareGroups(rcvhelp ~ gender + 
                             yeargroup +
                             fsm1 +
                             bornuk1 +
                             aces +
                             acescat +
                             rcadst + rcads_scaled,
                           data = data_rcvhelp, #just those with data on outcome
                           byrow = TRUE, include.miss = TRUE)
createTable(tableaces,show.ratio = TRUE)
aggregate(aces ~ rcvhelp, data = data_rcvhelp, summary) #ACEs medians

# Compare complete cases v. those w/ missing data
data_rcvhelp$complete <- ifelse(!is.na(data_rcvhelp$gender) & !is.na(data_rcvhelp$fsm) & !is.na(data_rcvhelp$yeargroup) &
                                  !is.na(data_rcvhelp$bornuk) & !is.na(data_rcvhelp$aces) & !is.na(data_rcvhelp$rcads_scaled),
                                "Complete", "Not complete")

tableaces <- compareGroups(complete ~ gender + 
                             yeargroup +
                             fsm1 +
                             bornuk1 +
                             aces +
                             acescat +
                             rcadst + rcads_scaled,
                           data = data_rcvhelp, #just those with data on outcome
                           byrow = FALSE, include.miss = TRUE)
createTable(tableaces,show.ratio = TRUE)
aggregate(aces ~ complete, data = data_rcvhelp, summary) #ACEs medians

### Benefit from help outcome tables ###
# All with outome data on unmet need, N = 1377
data_benhelp<-subset(data,(!is.na(benhelp)))
summary(data_benhelp$aces)
tableaces <- compareGroups(~gender + 
                             yeargroup +
                             fsm1 +
                             bornuk1 +
                             aces +
                             acescat +
                             rcadst,
                           data = data_benhelp,
                           byrow = TRUE, include.miss = TRUE)
createTable(tableaces,show.ratio = TRUE)

# By perceived unmet need Y/N, N = 1377
tableaces <- compareGroups(benhelp ~ gender + 
                             yeargroup +
                             fsm1 +
                             bornuk1 +
                             bornuk + #added b/c the prefer not to say option made OR impossible
                             aces +
                             acescat +
                             rcadst +rcads_scaled,
                           data = data_benhelp,
                           byrow = TRUE, include.miss = TRUE)

createTable(tableaces,show.ratio = TRUE)
aggregate(aces ~ benhelp, data = data_benhelp, summary) #ACEs median

# Comparing complete cases v. missing data
data_benhelp$complete <- ifelse(!is.na(data_benhelp$gender) & !is.na(data_benhelp$fsm) & !is.na(data_benhelp$yeargroup) &
                                  !is.na(data_benhelp$bornuk) & !is.na(data_benhelp$aces) & !is.na(data_benhelp$rcads_scaled),
                                "Complete", "Not complete")

tableaces <- compareGroups(complete ~ gender + 
                             yeargroup +
                             fsm1 +
                             bornuk1 +
                             aces +
                             acescat +
                             rcadst + rcads_scaled,
                           data = data_benhelp, #just those with data on outcome
                           byrow = FALSE, include.miss = TRUE)
createTable(tableaces,show.ratio = TRUE)
aggregate(aces ~ complete, data = data_benhelp, summary) #ACEs medians


##### DATA VISUALISATION AND EXPLORATION #####
cro(data$rcadstbinary,data$rcvhelp)
cro(data$rcadstbinary,data$benhelp)
cro_rpct(data$rcadstbinary,data$rcvhelp)
cro_rpct(data$rcadstbinary,data$benhelp)

# Histograms of ACEs and RCADS scores
counts <- table(data$aces)
barplot(counts, main="Number of ACEs")
hist(data$rcadst, main="RCADS Total Score")

# Table of RCADS score vs. receive help (y/n) for any vs. no ACEs
cro(data$rcadstbinary,data$rcvhelp,data$acesany)
cro_rpct(data$rcadstbinary,data$rcvhelp,data$acesany)

# Table of RCADS score vs. whether could have benefitted (y/n) for any vs. no ACEs
cro(data$rcadstbinary,data$benhelp,data$acesany)
cro_rpct(data$rcadstbinary,data$benhelp,data$acesany)

# Table of any vs. no ACEs vs. whether could have benefitted (y/n)
cro(data$acesany,data$benhelp)
cro_rpct(data$acesany,data$benhelp)

# Prop. receiving help by ACE score
table(data$aces, data$rcvhelp)
datapoormh <- subset(data, rcadst>=70)

# Graph - receive help total sample
data$acesgraph <- ifelse(data$aces==0, "0", 
                             ifelse(data$aces==1,"1",
                                    ifelse(data$aces==2, "2", 
                                           ifelse(data$aces==3, "3", "4+")))) 

table<-table(data$rcvhelp, data$acesgraph)
table2<-prop.table(table,2)

table2data<-as.data.frame.matrix(table2)
table2data$newvar<-rownames(table2data)
table2data$newvar[table2data$newvar==0]<-"No"
table2data$newvar[table2data$newvar==1]<-"Yes"

table2datalong<-melt(table2data, id.vars=c("newvar"),value.name="proportion")
names(table2datalong)[2]<-paste("ACEs")

p1 <- ggplot() + geom_bar(aes(y=proportion,
                        x=ACEs,
                        fill=factor(newvar, levels=c("No", "Yes"))),
                    data=table2datalong,
                    stat="identity") + 
  ggtitle("Prior access to mental health support \nby cumulative ACE score") +
  ylab("Proportion")+
  scale_fill_discrete(name = "Prior access \nto support")
#ggsave("Prior access total sample (publication).jpg", width = 5, height = 4)

# Graph - receive help RCADS >=70
datapoormh$acesgraph <- ifelse(datapoormh$aces==0, "0", 
                               ifelse(datapoormh$aces==1,"1",
                                      ifelse(datapoormh$aces==2, "2", 
                                             ifelse(datapoormh$aces==3, "3", "4+")))) 

table<-table(datapoormh$rcvhelp,datapoormh$acesgraph)
table3<-prop.table(table,2)

table3data<-as.data.frame.matrix(table3)
table3data$newvar<-rownames(table3data)
table3data$newvar[table3data$newvar==0]<-"No"
table3data$newvar[table3data$newvar==1]<-"Yes"

table3datalong<-melt(table3data, id.vars=c("newvar"),value.name="proportion")
names(table3datalong)[2]<-paste("ACEs")

p2 <- ggplot() + geom_bar(aes(y=proportion,
                        x=ACEs,
                        fill=factor(newvar, levels=c("No", "Yes"))),
                    data=table3datalong,
                    stat="identity") + 
  ggtitle("Prior access to mental health support \nby cumulative ACE score (RCADS≥70)") +
  ylab("Proportion")+
  scale_fill_discrete(name = "Prior access \nto support")
#ggsave("Prior access high RCADS (publication).jpg", width = 5, height = 4)

# Graph - benefit help total sample
data$acesgraph <- ifelse(data$aces==0, "0", 
                             ifelse(data$aces==1,"1",
                                    ifelse(data$aces==2, "2", 
                                           ifelse(data$aces==3, "3", "4+")))) 

table<-table(data$benhelp, data$acesgraph)
table2<-prop.table(table,2)

table2data<-as.data.frame.matrix(table2)
table2data$newvar<-rownames(table2data)
table2data$newvar[table2data$newvar==0]<-"No"
table2data$newvar[table2data$newvar==1]<-"Yes"

table2datalong<-melt(table2data, id.vars=c("newvar"),value.name="proportion")
names(table2datalong)[2]<-paste("ACEs")

p3 <- ggplot() + geom_bar(aes(y=proportion,
                        x=ACEs,
                        fill=factor(newvar, levels=c("No", "Yes"))),
                    data=table2datalong,
                    stat="identity") + 
  ggtitle("Perceived unmet need for mental health \nservices by cumulative ACE score") +
  ylab("Proportion")+
  scale_fill_discrete(name = "Perceived \nunmet need")
#ggsave("Percevied need total sample (publication).jpg", width = 5, height = 4)

# Graph - benefit help RCADS >=70
datapoormh$acesgraph <- ifelse(datapoormh$aces==0, "0", 
                               ifelse(datapoormh$aces==1,"1",
                                      ifelse(datapoormh$aces==2, "2", 
                                             ifelse(datapoormh$aces==3, "3", "4+")))) 

table<-table(datapoormh$benhelp,datapoormh$acesgraph)
table3<-prop.table(table,2)

table3data<-as.data.frame.matrix(table3)
table3data$newvar<-rownames(table3data)
table3data$newvar[table3data$newvar==0]<-"No"
table3data$newvar[table3data$newvar==1]<-"Yes"

table3datalong<-melt(table3data, id.vars=c("newvar"),value.name="proportion")
names(table3datalong)[2]<-paste("ACEs")

p4 <-ggplot() + geom_bar(aes(y=proportion,
                        x=ACEs,
                        fill=factor(newvar, levels=c("No", "Yes"))),
                    data=table3datalong,
                    stat="identity") + 
  ggtitle("Perceived unmet need for mental health \nservices by cumulative ACE score (RCADS≥70)") +
  ylab("Proportion")+
  scale_fill_discrete(name = "Perceived \nunmet need")
#ggsave("Percevied need high RCADS (publication).jpg", width = 5, height = 4)

figure <- ggarrange(p1, p2, p3, p4,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
figure
#ggsave("All plots (publication).jpg", width = 10, height = 8)

##### RECEIVE HELP OUTCOME #####
# Multilevel logistic regression with receiving help outcome
table(data$rcvhelp, useNA="always")
data$school<- as.factor(data$SCHOOL.ID.ORIGINAL)
data$school<- droplevels(data$school)

model_rcvhelp <- glmer(rcvhelp ~ (1|school) +
                         gender + fsm + yeargroup + bornuk + aces*rcads_scaled, 
                       data=data,
                       family=binomial(link="logit")
)

summary(model_rcvhelp)
exp(fixef(model_rcvhelp))

# Calculate CIs
vcov(model_rcvhelp)
se <- sqrt(diag(vcov(model_rcvhelp))) #standard errors
CI_estimates <- cbind(Est = fixef(model_rcvhelp), lwr = 
                        fixef(model_rcvhelp) - 1.96 * se, upr = fixef(model_rcvhelp) + 1.96 * se)
est<-exp(CI_estimates)
est<-as.data.frame(est)
est<-cbind(" "=rownames(est), est)
#write_xlsx(est, "Access to Services Regression.xlsx")


# Plot of ACES*RCADS interaction term
newdata_rcvhelp<-expand.grid(school=levels(data$school)[1],
                             gender=factor(0), 
                             yeargroup=factor(12),
                             fsm=factor(0), 
                             bornuk=factor(0),
                             aces=c(0:10),
                             rcads_scaled=c(-1,0,1))
newdata_rcvhelp$predict <-predict(model_rcvhelp,newdata_rcvhelp,re.form=NA,type = c("response")) #re.form tells predict to ignore all random effects
newdata_rcvhelp$rcads_scaled_factor<- as.factor(newdata_rcvhelp$rcads_scaled)

Pfixed <- predictInterval(model_rcvhelp,newdata_rcvhelp,which="fixed",level=0.95,n.sims=50000, type="probability")

ints<-cbind(newdata_rcvhelp,Pfixed)

interaction_rcvhelp<-ggplot(data=newdata_rcvhelp, aes(x=aces, y=predict, color=rcads_scaled_factor)) + geom_smooth() + 
  geom_ribbon(aes(ymin=ints$lwr, ymax=ints$upr), linetype=2, alpha=0.1) + 
  ggtitle("ACEs*RCADS interaction (prior access)") +
  ylab("Predicted proportion with prior access") + xlab("ACEs") + 
  scale_color_discrete(name="RCADS\ntotal score\n(scaled)")
interaction_rcvhelp
#ggsave("RCVHELP*RCADS Interaction (publication).png", interaction_rcvhelp)

# Generate residual plots to check model fit
plot(residuals(model_rcvhelp, type="pearson"))
plot(y=residuals(model_rcvhelp, type="pearson"), x=model_rcvhelp@frame$aces)

##### COULD HAVE BENEFITTED OUTCOME ######

table(data$benhelp,useNA="always")

# Multilevel logistic regression with could have benefitted outcome
model_benhelp <- glmer(benhelp ~ (1|school) +
                         gender + fsm + yeargroup + bornuk + aces*rcads_scaled, 
                       data=data,
                       family=binomial(link="logit")
)

summary(model_benhelp)
exp(fixef(model_benhelp))

# Calculate CIs
vcov(model_benhelp)
se <- sqrt(diag(vcov(model_benhelp))) #standard errors
CI_estimates <- cbind(Est = fixef(model_benhelp), lwr = 
                        fixef(model_benhelp) - 1.96 * se, upr = fixef(model_benhelp) + 1.96 * se)
est<-exp(CI_estimates)
est<-as.data.frame(est)
est<-cbind(" "=rownames(est), est)
#write_xlsx(est, "Perceived Unmet Need Regression.xlsx")

# Plot of ACES*RCADS interaction term
newdata_benhelp<-expand.grid(school=levels( data$school)[1],
                             gender=factor(0), 
                             yeargroup=factor(12),
                             fsm=factor(0), 
                             bornuk=factor(0),
                             aces=c(0:10),
                             rcads_scaled=c(-1,0,1))
newdata_benhelp$predict <-predict(model_benhelp,newdata_benhelp,re.form=NA,type = c("response")) #re.form tells predict to ignore all random effects
newdata_benhelp$rcads_scaled_factor<- as.factor(newdata_benhelp$rcads_scaled)

Pfixed <- predictInterval(model_benhelp,newdata_benhelp,which="fixed",level=0.95,n.sims=50000, type="probability")
ints<-cbind(newdata_benhelp,Pfixed)

interaction_benhelp<-ggplot(data=newdata_benhelp, aes(x=aces, y=predict, color=rcads_scaled_factor)) + geom_smooth() + 
  geom_ribbon(aes(ymin=ints$lwr, ymax=ints$upr), linetype=2, alpha=0.1) +
  ggtitle("ACEs*RCADS interaction (perceived unmet need)") +
  ylab("Predicted proportion with unmet perceived need") + xlab("ACEs") + 
  scale_color_discrete(name="RCADS\ntotal score\n(scaled)")
interaction_benhelp
#ggsave("BENHELP*RCADS Interaction (publication).png", interaction_benhelp)

# Generate residual plots to check model fit
plot(residuals(model_benhelp, type="pearson"))
plot(y=residuals(model_benhelp, type="pearson"), x=model_benhelp@frame$aces)
