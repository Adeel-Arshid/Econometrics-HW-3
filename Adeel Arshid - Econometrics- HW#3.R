#Adeel Arshid, Mario Wisa, Minghao Deng
Homework#3
Econometrics 
load("/Users/adeelarshid/Desktop/Household_Pulse_data.RData")





#I want to see if there's a trend associated with higher education vs vaccination status

all_doses <- data.frame(matrix(ncol=1+length(summary(EEDUC)),nrow=0))
colnames(all_doses) <- levels(unique(EEDUC))
for (i in 1:length(summary(EEDUC))){
  all_doses[1,i] <- summary(EEDUC[DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'])[i] / summary(EEDUC)[i]
}
all_doses



#To reflect on the belief, I also tested people who work remotely might be less likely to get vaccinated vs people who work on-site.

onsite_only <- subset(Household_Pulse_data, Works_onsite == 'worked onsite' & works_remote != 'worked remotely')
remote_only <- subset(Household_Pulse_data, works_remote == 'worked remotely' & Works_onsite != 'worked onsite')
hybrid <- subset(Household_Pulse_data, works_remote == 'worked remotely' & Works_onsite == 'worked onsite')
vaccinated1 <- nrow(subset(onsite_only,DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'))/nrow(onsite_only)
vaccinated2 <- nrow(subset(remote_only,DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'))/nrow(remote_only)
vaccinated3 <- nrow(subset(hybrid,DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'))/nrow(hybrid)
vaccination_rate <- c(vaccinated1,vaccinated3,vaccinated2)
work_related <- data.frame(vaccination_rate,index = c('onsite','hybrid','remote'))
work_related





# vaccination status and covid status of workers working onsite and remotely.
restrict1 <- (Household_Pulse_data$Works_onsite == "worked onsite") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
restrict2 <- (Household_Pulse_data$Works_remote == "worked remotely") & (Household_Pulse_data$RECVDVACC == "yes got vaxx") | (Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$HADCOVID == "yes doctor told had covid") | (Household_Pulse_data$HADCOVID == "no did not")
data_Works_onsite <- subset(Household_Pulse_data,restrict2)
data_Works_remote <- subset(Household_Pulse_data, restrict1)

data

# Summary vaccination status and covid status of workers working onsite and remotely. 
summary(data_Works_onsite$RECVDVACC)
summary(data_Works_remote$RECVDVACC)
summary(data_Works_onsite$HADCOVID)
summary(data_Works_remote$HADCOVID)








# All of these four sections of code below are used to determine sd(standard deviation) and t-test for the standard error formula.
#Standard error formula is the sample standard deviation/ number of samples(n=68962)
sd(summary(data_Works_onsite$RECVDVACC))
t.test(summary(data_Works_onsite$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$RECVDVACC))
t.test(summary(data_newf$RECVDVACC),var.equal = TRUE)

sd(summary(data_newf$HADCOVID))
t.test(summary(data_newf$HADCOVID),var.equal = TRUE)

sd(summary(data_newm$HADCOVID))
t.test(summary(data_newm$HADCOVID),var.equal = TRUE)
#The prop table is used to determine the marginal probabilities in term of data_new for females and males
prop.table(summary(data_newf$RECVDVACC), margin = NULL)
prop.table(summary(data_newm$RECVDVACC), margin = NULL)
prop.table(summary(data_newf$HADCOVID), margin = NULL)
prop.table(summary(data_newm$HADCOVID), margin = NULL)











