Estimation of Rhat for WB data
#Using read_excel() to import the excel file
library(readxl)
west_bengal<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Block-7 - Level-06 Particulars of persons currently not attending any educational institute (Tanisha) (1).xlsx",sheet="West Bengal")
#Creating the indicator variables x and y
#x_i being the indicator of ever enrolled but not attending the institution currently
#y_i being the indicator of ever enrolled but not attending the institution currently and discontinued with reason not interested in education
x_i<-ifelse(west_bengal$enrolled=="yes",1,0)
print(x_i)
y_i<-ifelse(west_bengal$enrolled=="yes" & west_bengal$never_enrol_reason=="not interested in education",1,0)
print(y_i)
#Creating multiplier array from the file
mult_i<-as.array(west_bengal$wgt_combined)
print(mult_i)
#Calculating the value of X_hat, Y_hat and R_hat
X_hat<-sum(x_i*mult_i)
print(X_hat)
Y_hat<-sum(y_i*mult_i)
print(Y_hat)
R_hat<-Y_hat/X_hat
print(R_hat)
##Merging of Block 4 and Block 7
install.packages("openxlsx")
install.packages("dplyr") 
library(readxl)
#importing two excel files of block 4 and block 7 for All India
block_4<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Block 4 and 7 new.xlsx",sheet="Block 4")
block_7<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Block 4 and 7 new.xlsx",sheet="Block 7")
#merging the details of two excel files
library(dplyr)
merged<-inner_join(block_4,block_7,by=c("HH_ID","psrl_no"))
print(merged)
summary(merged)
#exporting the merged data in an excel file
library(openxlsx)
write.xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx")
##estimation of R_hat for all the enrollment data
#importing the Enrollment codes for enrollment details
library(readxl)
enrol_data<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx",sheet="Enrollment code")
enrol_cd<-enroll_data$enroll_cd
print(enrol_cd)
never_enrol_reason<-enrol_data$Reason
print(never_enrol_reason)
#creating a vector that denotes the never enrollment reason by their code
enrol_map<-setNames(never_enrol_reason,enrol_cd)
print(enrol_map)
#let i be the language spoken at home and let j be the medium of instruction
#x_i be the indicator of kth person ever enrolled or not
#x_j be the indicator of kth person unenrollment reason
j<-enrol_cd
print(j)
m<-length(j)
print(m)

##another for wb not used
library(readxl)
enrol_data<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Codes.xlsx",sheet="Enrollment_merged")
enrol_cd<-enrol_data$Enrol_cd
print(enrol_cd)
never_enrol_reason<-enrol_data$never_enrol_reason
print(never_enrol_reason)
#creating a vector that denotes the never enrollment reason by their code
enrol_map<-setNames(never_enrol_reason,enrol_cd)
print(enrol_map)
#let i be whether enrolled or not and let j be unenrollment reason
#x_i be the indicator of kth person ever enrolled or not
#x_j be the indicator of kth person unenrollment reason
j<-enrol_cd
print(j)
m<-length(j)
print(m)
##R_hat_ij for reason 1 for not attending institution currently for rural and male
#importing the excel file with the details of enrollment and not attending reason 
library(readxl)
survey_data<-read_excel("C:/Users/TANISHA/Downloads/Sample_Survey.xlsx",sheet="India")
sector<-ifelse(survey_data$sector=="Rural",1,0)
print(sector)
gender<-ifelse(survey_data$gender=="Male",1,0)
print(gender)
x_i<-ifelse(survey_data$sector=="Rural" & survey_data$gender=="Male" &survey_data$enrolled=="yes",1,0)
print(x_i)
y_i<-ifelse(survey_data$sector=="Rural" & survey_data$gender=="Male" &survey_data$enrolled=="yes" & survey_data$never_enrol_reason==never_enrol_reason[1],1,0)
print(y_i)
#Creating multiplier array from the file
mult_i<-as.array(survey_data$wgt_combined)
print(mult_i)
#Calculating the value of X_hat, Y_hat and R_hat
X_hat<-sum(x_i*mult_i)
print(X_hat)
Y_hat<-sum(y_i*mult_i)
print(Y_hat)
R_hat<-Y_hat/X_hat
print(R_hat)

##estimation of R_hat for all the enrollment data
#importing the Enrollment codes for enrollment details
library(readxl)
enrol_data<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx",sheet="Enrollment code")
enrol_cd<-enrol_data$enroll_cd
print(enrol_cd)
never_enrol_reason<-enrol_data$Reason
print(never_enrol_reason)
enrollment<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx", sheet="Enrollment")
enrol_id<-enrollment$enroll
print(enrol_id)
enrolled<-enrollment$enrollment
print(enrolled)
#creating a vector that denotes the never enrollment reason by their code
enrol_map<-setNames(never_enrol_reason,enrol_cd)
print(enrol_map)
enrolled_map<-setNames(enrolled,enrol_id)
print(enrolled_map)
#let i be whether enrolled or not and let j be unenrollment reason
#x_i be the indicator of kth person ever enrolled or not
#x_j be the indicator of kth person unenrollment reason
j<-enrol_cd
print(j)
m<-length(j)
n<-length(enrol_id)
print(m)
print(n)
MSE_R_hat<-matrix(nrow=m, ncol=n)
RSE_R_hat<-matrix(nrow=m, ncol=n)
library(openxlsx)
Wb<-createWorkbook(title="R_hat_ij")
Wb2<-createWorkbook(title="MSE_R_hat")
Wb3<-createWorkbook(title="RSE_R_hat")

#importing the excel file with the details of enrollment and not attending reason
library(readxl)
survey_data_t<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx",sheet="India")
R_hat_ij<-matrix(nrow = m,ncol = n)
#Creating multiplier array from the file
mult_i<-as.array(survey_data_t$wgt_combined)
print(mult_i)


##1) R_hat_ij of not attending institution currently for (Urban, Male)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban, Male")
writeData(Wb,"Urban, Male", R_hat_ij)

##2) R_hat_ij of not attending institution currently for (Urban, Female)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" &survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" &survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}
#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban,Female")
writeData(Wb,"Urban,Female", R_hat_ij)

##3) R_hat_ij of not attending institution currently for (Urban, Male+Female)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban, Male+Female")
writeData(Wb,"Urban, Male+Female", R_hat_ij)

##4) R_hat_ij of not attending institution currently for (Rural, Male)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" &survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" &survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Rural, Male")
writeData(Wb,"Rural, Male", R_hat_ij)

##5) R_hat_ij of not attending institution currently for (Rural, Female)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" &survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" &survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Rural,Female")
writeData(Wb,"Rural,Female", R_hat_ij)

##6) R_hat_ij of not attending institution currently for (Rural, Male+Female)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Rural, Male+Female")
writeData(Wb,"Rural, Male+Female", R_hat_ij)

##7) R_hat_ij of not attending institution currently for (Urban+Rural, Male)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Male" &survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Male" &survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
   c<-c+1
  }
  z<z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban+Rural, Male")
writeData(Wb,"Urban+Rural, Male", R_hat_ij)

##8) R_hat_ij of not attending institution currently for (Urban+Rural, Female)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Female" &survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Female" &survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
   c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban+Rural, Female")
writeData(Wb,"Urban+Rural, Female", R_hat_ij)

##9) R_hat_ij of not attending institution currently for (Urban+Rural,Male+Female)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$enrolled==enrolled_map[z],1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$enrolled==enrolled_map[z] & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  Z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban+Rural, Male+Female")
writeData(Wb,"Urban+Rural, Male+Female", R_hat_ij)

#saving the R_hat_ij for all the divisions in a single excel file
saveWorkbook(Wb,"C:/Users/TANISHA/Downloads/R_hat_ij_India.xlsx")


##1) R_hat_ij of not attending institution currently for Urban, Male ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban, Male")
writeData(Wb,"Urban, Male", R_hat_ij)

##2) R_hat_ij of not attending institution currently for Urban, Female ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban, Female")
writeData(Wb,"Urban, Female", R_hat_ij)

##3) R_hat_ij of not attending institution currently for Urban, Male+Female ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Urban" & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban, Male+Female")
writeData(Wb,"Urban, Male+Female", R_hat_ij)

##4) R_hat_ij of not attending institution currently for Rural, Male ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Rural, Male")
writeData(Wb,"Rural, Male", R_hat_ij)

##5) R_hat_ij of not attending institution currently for Rural, Female ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Rural, Female")
writeData(Wb,"Rural, Female", R_hat_ij)

##6) R_hat_ij of not attending institution currently for Rural, Male+Female ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$sector=="Rural" & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)

    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Rural, Male+Female")
writeData(Wb,"Rural, Male+Female", R_hat_ij)

##7) R_hat_ij of not attending institution currently for Urban+Rural, Male ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] & survey_data_t$gender=="Male"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban+Rural, Male")
writeData(Wb,"Urban+Rural, Male", R_hat_ij)

##8) R_hat_ij of not attending institution currently for Urban+Rural, Female ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z]  & survey_data_t$gender=="Female"  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban+Rural, Female")
writeData(Wb,"Urban+Rural, Female", R_hat_ij)

##9) R_hat_ij of not attending institution currently for Urban+Rural, Male+Female ( for West Bengal)
z<-1
for(a in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z] ,1,0)
  print(x_i)
  X_hat<-sum(x_i*mult_i)
  print(X_hat)
  c<-1
  for(b in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrolled_map[z]  & survey_data_t$never_enrol_reason==never_enrol_reason[c],1,0)
    print(y_i)
    Y_hat<-sum(y_i*mult_i)
    print(Y_hat)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    R_hat_ij[c,z]<-R_hat
    c<-c+1
  }
  z<-z+1
}
print(R_hat_ij)
a<-1
for(x in 1:n){
  sum<-0
  b<-1
  for(y in 1:m){
    sum<-sum+R_hat_ij[b,a]
    b<-b+1
  }
  print(sum)
  a<-a+1
}

#Saving the R_hat_ij data in an excel file
addWorksheet(Wb,"Urban+Rural, Male+Female")
writeData(Wb,"Urban+Rural, Male+Female", R_hat_ij)

#saving the R_hat_ij for all the divisions in a single excel file
saveWorkbook(Wb,"C:/Users/TANISHA/Downloads/Kajal Ma'am/R_hat_ij_wb.xlsx")


#10)Estimation of variance of every R_hat for (West Bengal)
z<-1
for (a in 1:n) {
  enrol1<-enrolled_map[z]
  c<-1
  for (b in 1:m) {
    unenroll1<-enrol_map[c]
    x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrol1,1,0)
    X_hat<-sum(x_i*mult_i)
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$enrolled==enrol1 & survey_data_t$never_enrol_reason==unenroll1 ,1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    data_stratum<-split(survey_data_t, survey_data_t$stratum) #stratum split
    var<-0
    for(s in 1:length(data_stratum)){
      vars<-0
      data_s<-data_stratum[[s]]
      data_s_substratum<-split(data_s,data_s$sstratum) #substratum split
      for(t in 1:length(data_s_substratum)){
        varst<-0
        data_st<-data_s_substratum[[t]]
        data_st_subsample<-split(data_st,data_st$ssample) # subsample split
        y_est1_i<-ifelse(data_st_subsample[[1]]$state_cd=="19" & data_st_subsample[[1]]$enrolled==enrol1 & data_st_subsample[[1]]$never_enrol_reason==unenroll1,1,0)
        y_est1<-sum(y_est1_i*mult_i)
        x_est1_i<-ifelse(data_st_subsample[[1]]$state_cd=="19" & data_st_subsample[[1]]$enrolled==enrol1,1,0)
        x_est1<-sum(x_est1_i*mult_i)
        if(length(data_st_subsample)<2){
          y_est2<-0
          x_est2<-0
        }
        else{
          y_est2_i<-ifelse(data_st_subsample[[2]]$state_cd=="19" & data_st_subsample[[2]]$enrolled==enrol1 & data_st_subsample[[2]]$never_enrol_reason==unenroll1,1,0)
          y_est2<-sum(y_est2_i*mult_i)
          x_est2_i<-ifelse(data_st_subsample[[2]]$state_cd=="19" & data_st_subsample[[2]]$enrolled==enrol1,1,0)
          x_est2<-sum(x_est2_i*mult_i)
        }
        y_est<-y_est1-y_est2
        x_est<-x_est1-x_est2
        varst<-(y_est*2+(R_hat2)(x_est**2)-(2*R_hat*y_est*x_est))
        vars<-vars+varst
      }
      var<-var+varst
    }
    varij<-(var/(4*(X_hat**2)))
    print(varij)
    MSE_R_hat[c,z]<-varij
    rse_ij<-(sqrt(varij)/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[c,z]<-rse_rhat
    c<-c+1
  }
  z<-z+1
}
print(MSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"West Bengal")
writeData(Wb2,"West Bengal", MSE_R_hat)
print(RSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"West Bengal")
writeData(Wb3,"West Bengal", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_Rhat.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_Rhat.xlsx")

#Estimation of Variance

##Estimation of variance of every R_hat and Relative Standard Error of every R_hat
library(readxl)
survey_data_t<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx",sheet="wb")
mult_i<-as.array(survey_data$wgt_combined)
print(mult_i)
enrol_data<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Codes.xlsx",sheet="Enrollment_merged")
enrol_cd<-enrol_data$Enrol_cd
print(enrol_cd)
never_enrol_reason<-enrol_data$never_enrol_reason
print(never_enrol_reason)
enrollment<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx",sheet="Enrollment")
enrol_id<-enrollment$enroll
print(enrol_id)
enrolled<-enrollment$enrollment
print(enrolled)
#creating a vector that denotes the never enrollment reason by their code
enrol_map<-setNames(never_enrol_reason,enrol_cd)
print(enrol_map)
enrolled_map<-setNames(enrolled,enrol_id)
print(enrolled_map)
#let i be the language spoken at home and let j be the medium of instruction
#x_i be the indicator of kth person ever enrolled or not
#x_j be the indicator of kth person unenrollment reason
j<-enrol_cd
print(j)
m<-length(j)
print(m)
n<-length(enrol_id)
print(n)
MSE_R_hat<-matrix(nrow=m, ncol=n)
RSE_R_hat<-matrix(nrow=m, ncol=n)
r_hat<-matrix(nrow=m, ncol=n)
library(openxlsx)
Wb2<-createWorkbook(title="MSE_Rhat_Tanisha")
Wb3<-createWorkbook(title="RSE_Rhat_Tanisha")
Wb4<-createWorkbook(title="Rhat_merged")
s_max<-max(survey_data$stratum)
print(s_max)
ss_max<-max(survey_data$sstratum)
print(ss_max)
subs_max<-max(survey_data$ssample)
print(subs_max)

#Estimation of Variance for India 

#1)Estimation of variance of every R_hat for (Urban, Male)
  a<-1
  for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male")
writeData(Wb2,"Urban, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male")
writeData(Wb3,"Urban, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Male")
writeData(Wb4,"Urban, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat1_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat1_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat1_merged.xlsx")


#2)Estimation of variance of every R_hat for (Urban, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Female")
writeData(Wb2,"Urban, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Female")
writeData(Wb3,"Urban, Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat2.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat2.xlsx")


#3)Estimation of variance of every R_hat for (Urban, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban"  & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Urban" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male+Female")
writeData(Wb2,"Urban, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male+Female")
writeData(Wb3,"Urban, Male+Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat3.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat3.xlsx")


#4)Estimation of variance of every R_hat for (Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male")
writeData(Wb2,"Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male")
writeData(Wb3,"Rural, Male", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat4.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat4.xlsx")


#5)Estimation of variance of every R_hat for (Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Female")
writeData(Wb2,"Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Female")
writeData(Wb3,"Rural, Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat5.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat5.xlsx")


#6)Estimation of variance of every R_hat for (Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$sector=="Rural" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male+Female")
writeData(Wb2,"Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male+Female")
writeData(Wb3,"Rural, Male+Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat6.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat7.xlsx")


#7)Estimation of variance of every R_hat for (Urban+Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Male" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male")
writeData(Wb2,"Urban+Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male")
writeData(Wb3,"Urban+Rural, Male", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat7.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat6.xlsx")


#8)Estimation of variance of every R_hat for (Urban+Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$gender=="Female" & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Female")
writeData(Wb2,"Urban+Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Female")
writeData(Wb3,"Urban+Rural, Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat8.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat8.xlsx")


#9)Estimation of variance of every R_hat for (Urban+Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male+Female")
writeData(Wb2,"Urban+Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male+Female")
writeData(Wb3,"Urban+Rural, Male+Female", RSE_R_hat)


#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat9.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat9.xlsx")

#Estimation of Variance for West Bengal

#1)Estimation of variance of every R_hat for (Urban, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male")
writeData(Wb2,"Urban, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male")
writeData(Wb3,"Urban, Male", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat1.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat1.xlsx")

#2)Estimation of variance of every R_hat for (Urban, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Female")
writeData(Wb2,"Urban, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Female")
writeData(Wb3,"Urban, Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat2new.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat2new.xlsx")

#3)Estimation of variance of every R_hat for (Urban, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male+Female")
writeData(Wb2,"Urban, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male+Female")
writeData(Wb3,"Urban, Male+Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat3.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat3.xlsx")

#4)Estimation of variance of every R_hat for (Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male")
writeData(Wb2,"Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male")
writeData(Wb3,"Rural, Male", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat4.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat4.xlsx")

#5)Estimation of variance of every R_hat for (Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Female")
writeData(Wb2,"Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Female")
writeData(Wb3,"Rural, Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat5.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat5.xlsx")

#6)Estimation of variance of every R_hat for (Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male+Female")
writeData(Wb2,"Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male+Female")
writeData(Wb3,"Rural, Male+Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat6.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat6.xlsx")

#1)Estimation of variance of every R_hat for (Urban, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male")
writeData(Wb2,"Urban, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male")
writeData(Wb3,"Urban, Male", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat1.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat1.xlsx")

#7)Estimation of variance of every R_hat for (Urban+Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male")
writeData(Wb2,"Urban+Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male")
writeData(Wb3,"Urban+Rural, Male", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat7.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat7.xlsx")

#8)Estimation of variance of every R_hat for (Urban+Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Female")
writeData(Wb2,"Urban+Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Female")
writeData(Wb3,"Urban+Rural, Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat8.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat8.xlsx")

#9)Estimation of variance of every R_hat for (Urban+Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male+Female")
writeData(Wb2,"Urban+Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male+Female")
writeData(Wb3,"Urban+Rural, Male+Female", RSE_R_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat9.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat9.xlsx")


#Merged Estimated Variance for India

#1)Estimation of variance of every R_hat for (Urban, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male")
writeData(Wb2,"Urban, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male")
writeData(Wb3,"Urban, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Male")
writeData(Wb4,"Urban, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat1_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat1_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat1_merged.xlsx")

#2)Estimation of variance of every R_hat for (Urban, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Female")
writeData(Wb2,"Urban, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Female")
writeData(Wb3,"Urban, Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Female")
writeData(Wb4,"Urban, Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat2_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat2_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat2_merged.xlsx")

#3)Estimation of variance of every R_hat for (Urban, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male+Female")
writeData(Wb2,"Urban, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male+Female")
writeData(Wb3,"Urban, Male+Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Male+Female")
writeData(Wb4,"Urban, Male+Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat3_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat3_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat3_merged.xlsx")

#4)Estimation of variance of every R_hat for (Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male")
writeData(Wb2,"Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male")
writeData(Wb3,"Rural, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Rural, Male")
writeData(Wb4,"Rural, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat4_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat4_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat4_merged.xlsx")

#5)Estimation of variance of every R_hat for (Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Female")
writeData(Wb2,"Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Female")
writeData(Wb3,"Rural, Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Rural, Female")
writeData(Wb4,"Rural, Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat5_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat5_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat5_merged.xlsx")

#6)Estimation of variance of every R_hat for (Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male+Female")
writeData(Wb2,"Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male+Female")
writeData(Wb3,"Rural, Male+Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Rural, Male+Female")
writeData(Wb4,"Rural, Male+Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat6_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat6_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat6_merged.xlsx")

#7)Estimation of variance of every R_hat for (Urban+Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male")
writeData(Wb2,"Urban+Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male")
writeData(Wb3,"Urban+Rural, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban+Rural, Male")
writeData(Wb4,"Urban+Rural, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat7_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat7_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat7_merged.xlsx")

#8)Estimation of variance of every R_hat for (Urban+Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Female")
writeData(Wb2,"Urban+Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Female")
writeData(Wb3,"Urban+Rural, Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban+Rural, Female")
writeData(Wb4,"Urban+Rural, Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat8_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat8_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat8_merged.xlsx")

#9)Estimation of variance of every R_hat for (Urban+Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male+Female")
writeData(Wb2,"Urban+Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male+Female")
writeData(Wb3,"Urban+Rural, Male+Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban+Rural, Male+Female")
writeData(Wb4,"Urban+Rural, Male+Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat9_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat9_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat9_merged.xlsx")

#Merged Estimated Variance for West Bengal

#1)Estimation of variance of every R_hat for (Urban, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$state_cd=="19" & survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male")
writeData(Wb2,"Urban, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male")
writeData(Wb3,"Urban, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Male")
writeData(Wb4,"Urban, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat1_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat1_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat1_merged.xlsx")

#2)Estimation of variance of every R_hat for (Urban, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Female")
writeData(Wb2,"Urban, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Female")
writeData(Wb3,"Urban, Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Female")
writeData(Wb4,"Urban, Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat2_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat2_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat2_merged.xlsx")

#3)Estimation of variance of every R_hat for (Urban, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Urban" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban, Male+Female")
writeData(Wb2,"Urban, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban, Male+Female")
writeData(Wb3,"Urban, Male+Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban, Male+Female")
writeData(Wb4,"Urban, Male+Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat3_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat3_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat3_merged.xlsx")

#4)Estimation of variance of every R_hat for (Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male")
writeData(Wb2,"Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male")
writeData(Wb3,"Rural, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Rural, Male")
writeData(Wb4,"Rural, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat4_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat4_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat4_merged.xlsx")

#5)Estimation of variance of every R_hat for (Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Female")
writeData(Wb2,"Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Female")
writeData(Wb3,"Rural, Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Rural, Female")
writeData(Wb4,"Rural, Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat5_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat5_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat5_merged.xlsx")


#6)Estimation of variance of every R_hat for (Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$sector=="Rural" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Rural, Male+Female")
writeData(Wb2,"Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Rural, Male+Female")
writeData(Wb3,"Rural, Male+Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Rural, Male+Female")
writeData(Wb4,"Rural, Male+Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat6_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat6_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat6_merged.xlsx")


#7)Estimation of variance of every R_hat for (Urban+Rural, Male)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Male"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Male" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male")
writeData(Wb2,"Urban+Rural, Male", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male")
writeData(Wb3,"Urban+Rural, Male", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban+Rural, Male")
writeData(Wb4,"Urban+Rural, Male",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat7_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat7_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat7_merged.xlsx")
library(dplyr)


#8)Estimation of variance of every R_hat for (Urban+Rural, Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Female"  & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$gender=="Female" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Female")
writeData(Wb2,"Urban+Rural, Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Female")
writeData(Wb3,"Urban+Rural, Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban+Rural, Female")
writeData(Wb4,"Urban+Rural, Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat8_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat8_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat8_merged.xlsx")

#9)Estimation of variance of every R_hat for (Urban+Rural, Male+Female)
a<-1
for(u in 1:n){
  x_i<-ifelse(survey_data_t$enrolled==enrolled_map[a],1,0)
  X_hat<-sum(x_i*mult_i)
  b<-1
  for (v in 1:m){
    y_i<-ifelse(survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
    Y_hat<-sum(y_i*mult_i)
    R_hat<-Y_hat/X_hat
    print(R_hat)
    r_hat[b,a]<-R_hat
    variance<-0
    c<-1
    for(s in 1:s_max){
      var_s<-0
      d<-1
      for(t in 1:ss_max){
        var_st<-0
        Y_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st1<-sum(Y_hat_st1_i*mult_i)
        X_hat_st1_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 1" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st1<-sum(X_hat_st1_i*mult_i)
        Y_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a] & survey_data_t$never_enrol_reason==enrol_map[b],1,0)
        Y_hat_st2<-sum(Y_hat_st2_i*mult_i)
        X_hat_st2_i<-ifelse(survey_data_t$stratum==c & survey_data_t$sstratum==d & survey_data_t$ssample=="Sub-Sample 2" & survey_data_t$enrolled==enrolled_map[a],1,0)
        X_hat_st2<-sum(X_hat_st2_i*mult_i)
        Y_hat_st<-Y_hat_st1-Y_hat_st2
        X_hat_st<-X_hat_st1-X_hat_st2
        var_st<-((Y_hat_st**2)+(R_hat**2)*(X_hat_st**2)-2*(R_hat*Y_hat_st*X_hat_st))
        var_s<-var_s+var_st
        d<-d+1
      }
      variance<-variance+var_s
      c<-c+1
    }
    den<-4*(X_hat**2)
    varij<-variance/den
    print(varij)
    MSE_R_hat[b,a]<-varij
    num<-sqrt(varij)
    rse_ij<-(num/R_hat)*100
    rse_rhat<-round(rse_ij,2)
    print(rse_rhat)
    RSE_R_hat[b,a]<-rse_rhat
    b<-b+1
  }
  a<-a+1
}
print(r_hat)
print(MSE_R_hat)
print(RSE_R_hat)

#Saving the MSE_R_hat data in an excel file
addWorksheet(Wb2,"Urban+Rural, Male+Female")
writeData(Wb2,"Urban+Rural, Male+Female", MSE_R_hat)

#Saving the RSE_R_hat data in an excel file
addWorksheet(Wb3,"Urban+Rural, Male+Female")
writeData(Wb3,"Urban+Rural, Male+Female", RSE_R_hat)

#Saving the r_hat data in an excel file
addWorksheet(Wb4,"Urban+Rural, Male+Female")
writeData(Wb4,"Urban+Rural, Male+Female",r_hat)

#saving the MSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb2,"C:/Users/TANISHA/Downloads/Kajal Ma'am/MSE_R_hat9_merged.xlsx")
#saving the RSE_R_hat for all the divisions in a single excel file
saveWorkbook(Wb3,"C:/Users/TANISHA/Downloads/Kajal Ma'am/RSE_R_hat9_merged.xlsx")
#saving the r_hat for all the divisions in a single excel file
saveWorkbook(Wb4,"C:/Users/TANISHA/Downloads/Kajal Ma'am/r_hat9_merged.xlsx")

#Importing block 3 data for calculation of UMPCE
library(readxl)
block_3<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Block-3-Level-02 Household Characteristics.xlsx",sheet="Block-3-Level-02 Household Char")
hh_consump<-as.array(block_3$hh_cons_exp)
print(hh_consump)
hh_size<-as.array(block_3$hhsize)
print(hh_size)
umpce<-hh_consump/hh_size
print(umpce)

install.packages("writexl")
#Merging UMPCE data of block 3 with data of block 7 according to HH_id
library(readxl)
survey_data_t<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey.xlsx",sheet="India")
block_3_data<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Block-3-Level-02 Household Characteristics.xlsx",sheet="Block-3-Level-02 Household Char")
library(writexl)
merged_data<-merge(survey_data_t,block_3_data,by="HH_ID")
print(merged_data)

#Exporting the merged data in an excel file
library(openxlsx)
write_xlsx(merged_data,"C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx")


#creating a vector calculate the quantiles for urban sector of West Bengal
Wb_urban<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx", sheet="Wb_Urban")
Wb_urban_multiplier<-as.array(Wb_urban$wgt_combined.x)
print(Wb_urban_multiplier)
Wb_urban_umpce<-as.array(Wb_urban$umpce)
quintile_urban_Wb<-quantile(Wb_urban_umpce, probs=c(0,0.2,0.4,0.6,0.8,1))
print(quintile_urban_Wb)
print(Wb_urban_umpce)
i=(length(Wb_urban_multiplier))
print(i)
unique_Wb_urban<-as.array(unique(Wb_urban_multiplier))
print(unique_Wb_urban)
print(unique_Wb_urban[1])
j<-length(unique_Wb_urban)
print(j)
frequency<-as.array(table((Wb_urban_multiplier)))
print(frequency)
library(dplyr)
final_vector_urban<-c()
vector<-c()
a<-1
for (b in 1:j){
  print(unique_Wb_urban[a])
  freq<-sum(Wb_urban$wgt_combined.x==unique_Wb_urban[a])
  print(freq)
  umpce<-Wb_urban$umpce[which(Wb_urban$wgt_combined.x==unique_Wb_urban[a])]
  print(umpce)
  vector<-rep(umpce,freq)
  print(vector)
  final_vector_urban<-append(final_vector_urban, vector)
  a<-a+1
}
print(final_vector_urban)
quintiles_urban_Wb<-quantile(final_vector_urban, probs=c(0,0.2,0.4,0.6,0.8,1))
print(quintiles_urban_Wb)


#creating a vector calculate the quantiles for rural sector of West Bengal
Wb_rural<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx", sheet="Wb_Rural")
Wb_rural_multiplier<-as.array(Wb_rural$wgt_combined.x)
print(Wb_rural_multiplier)
Wb_rural_umpce<-as.array(Wb_rural$umpce)
quintile_rural_Wb<-quantile(Wb_rural_umpce, probs=c(0,0.2,0.4,0.6,0.8,1))
print(quintile_rural_Wb)
print(Wb_rural_umpce)
i=(length(Wb_rural_multiplier))
print(i)
unique_Wb_rural<-as.array(unique(Wb_rural_multiplier))
print(unique_Wb_rural)
print(unique_Wb_rural[1])
j<-length(unique_Wb_rural)
print(j)
frequency<-as.array(table((Wb_rural_multiplier)))
print(frequency)
library(dplyr)
final_vector_rural<-c()
vector<-c()
a<-1
for (b in 1:j){
  print(unique_Wb_rural[a])
  freq<-sum(Wb_rural$wgt_combined.x==unique_Wb_rural[a])
  print(freq)
  umpce<-Wb_rural$umpce[which(Wb_rural$wgt_combined.x==unique_Wb_rural[a])]
  print(umpce)
  vector<-rep(umpce,freq)
  print(vector)
  final_vector_rural<-append(final_vector_rural, vector)
  a<-a+1
}
print(final_vector_rural)
quintiles_rural_Wb<-quantile(final_vector_rural, probs=c(0,0.2,0.4,0.6,0.8,1))
print(quintiles_rural_Wb)


#assigning the quintiles classes depending on the umpce values(Urban)
quintile_class_urban<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(Wb_urban_umpce[a] <=quintiles_urban_Wb[2], 1,
                          ifelse(Wb_urban_umpce[a] <=quintiles_urban_Wb[3], 2,
                                 ifelse(Wb_urban_umpce[a] <=quintiles_urban_Wb[4], 3,
                                        ifelse(Wb_urban_umpce[a] <=quintiles_urban_Wb[5], 4,
                                               ifelse(Wb_urban_umpce[a] <=quintiles_urban_Wb[6], 5)))))
  print(Wb_urban_umpce[a])
  print(quintile_class)
  quintile_class_urban[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_urban)
library(dplyr)
merged<-data.frame(Column1=Wb_urban_umpce,Column2=quintile_class_urban)
print(merged)
summary(merged)


#exporting the merged data in an excel file(Urban)
library(writexl)
write_xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/UMPCE_Urban.xlsx")

#assigning the quintiles classes depending on the umpce values(Rural)
quintile_class_rural<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(Wb_rural_umpce[a] <=quintiles_rural_Wb[2], 1,
                          ifelse(Wb_rural_umpce[a] <=quintiles_rural_Wb[3], 2,
                                 ifelse(Wb_rural_umpce[a] <=quintiles_rural_Wb[4], 3,
                                        ifelse(Wb_rural_umpce[a] <=quintiles_rural_Wb[5], 4,
                                               ifelse(Wb_rural_umpce[a] <=quintiles_rural_Wb[6], 5)))))
  print(Wb_rural_umpce[a])
  print(quintile_class)
  quintile_class_rural[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_rural)
library(dplyr)
merged<-data.frame(Column1=Wb_rural_umpce,Column2=quintile_class_rural)
print(merged)
summary(merged)


#exporting the merged data in an excel file(Rural)
library(writexl)
write_xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/UMPCE_Rural.xlsx")


#creating a vector calculate the quantiles for urban sector of India(Urban)
urban<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx", sheet="India_Urban")
urban_multiplier<-as.array(urban$wgt_combined.x)
print(urban_multiplier)
urban_umpce<-as.array(urban$umpce)
print(urban_umpce)
i=(length(urban_multiplier))
print(i)
quintiles_urban<-c(0,1200,1667,2250,3333)
print(quintiles_urban)


#assigning the quintiles classes depending on the umpce values(Urban)
quintile_class_u<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(urban_umpce[a] <=quintiles_urban[2], 1,
                          ifelse(urban_umpce[a] <=quintiles_urban[3], 2,
                                 ifelse(urban_umpce[a] <=quintiles_urban[4], 3,
                                        ifelse(urban_umpce[a] <=quintiles_urban[5], 4,
                                               ifelse(urban_umpce[a] >quintiles_urban[5], 5)))))
  print(urban_umpce[a])
  print(quintile_class)
  quintile_class_u[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_u)
library(dplyr)
merged<-data.frame(Column1=urban_umpce,Column2=quintile_class_u)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/UMPCE_Urban_India.xlsx")

#creating a vector calculate the quantiles for urban sector of India(Rural)
rural<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx", sheet="India_Rural")
rural_multiplier<-as.array(rural$wgt_combined.x)
print(rural_multiplier)
rural_umpce<-as.array(rural$umpce)
print(rural_umpce)
i=(length(rural_multiplier))
print(i)
quintiles_rural<-c(0,786,1000,1287,1667)
print(quintiles_rural)

#assigning the quintiles classes depending on the umpce values(Rural)
quintile_class_r<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(rural_umpce[a] <=quintiles_rural[2], 1,
                          ifelse(rural_umpce[a] <=quintiles_rural[3], 2,
                                 ifelse(rural_umpce[a] <=quintiles_rural[4], 3,
                                        ifelse(rural_umpce[a] <=quintiles_rural[5], 4,
                                               ifelse(rural_umpce[a] >quintiles_rural[5], 5)))))
  print(rural_umpce[a])
  print(quintile_class)
  quintile_class_r[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_r)
library(dplyr)
merged<-data.frame(Column1=rural_umpce,Column2=quintile_class_r)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/UMPCE_Rural_India_new.xlsx")

#creating a vector calculate the quantiles for urban sector of Wb(Urban)
urban<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx", sheet="Wb_Urban")
urban_multiplier<-as.array(urban$wgt_combined.x)
print(urban_multiplier)
urban_umpce<-as.array(urban$umpce)
print(urban_umpce)
i=(length(urban_multiplier))
print(i)
quintiles_urban<-c(0,1150,1500,2000,3142.86)
print(quintiles_urban)


#assigning the quintiles classes depending on the umpce values(Urban)
quintile_class_u<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(urban_umpce[a] <=quintiles_urban[2], 1,
                          ifelse(urban_umpce[a] <=quintiles_urban[3], 2,
                                 ifelse(urban_umpce[a] <=quintiles_urban[4], 3,
                                        ifelse(urban_umpce[a] <=quintiles_urban[5], 4,
                                               ifelse(urban_umpce[a] >quintiles_urban[5], 5)))))
  print(urban_umpce[a])
  print(quintile_class)
  quintile_class_u[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_u)
library(dplyr)
merged<-data.frame(Column1=urban_umpce,Column2=quintile_class_u)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/UMPCE_Urban_Wb.xlsx")

#creating a vector calculate the quantiles for urban sector of Wb(Rural)
rural<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx", sheet="Wb_Rural")
rural_multiplier<-as.array(rural$wgt_combined.x)
print(rural_multiplier)
rural_umpce<-as.array(rural$umpce)
print(rural_umpce)
i=(length(rural_multiplier))
print(i)
quintiles_rural<-c(0,800,1000,1214.29,1500)
print(quintiles_rural)

#assigning the quintiles classes depending on the umpce values(Rural)
quintile_class_r<-matrix(nrow=i, ncol=1)
a<-1
for (b in 1:i){
  quintile_class<- ifelse(rural_umpce[a] <=quintiles_rural[2], 1,
                          ifelse(rural_umpce[a] <=quintiles_rural[3], 2,
                                 ifelse(rural_umpce[a] <=quintiles_rural[4], 3,
                                        ifelse(rural_umpce[a] <=quintiles_rural[5], 4,
                                               ifelse(rural_umpce[a] >quintiles_rural[5], 5)))))
  print(rural_umpce[a])
  print(quintile_class)
  quintile_class_r[a,1]<-quintile_class
  a<-a+1
}
print(quintile_class_r)
library(dplyr)
merged<-data.frame(Column1=rural_umpce,Column2=quintile_class_r)
print(merged)
summary(merged)

#exporting the merged data in an excel file
library(writexl)
write_xlsx(merged,"C:/Users/TANISHA/Downloads/Kajal Ma'am/UMPCE_Rural_Wb.xlsx")


#logistic regressions for India
library(readxl)
survey_data_new<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx",sheet="India")
y_1<-ifelse(survey_data_new$enrolled=="yes",1,0)
print(y_1)
y_2<-ifelse(survey_data_new$grade_class_comp<4,1,0)
print(y_2)
y_3<-ifelse(survey_data_new$grade_class_comp>4 & survey_data_new$grade_class_comp<8,1,0)
print(y_3)
y_4<-ifelse(survey_data_new$grade_class_comp>8 & survey_data_new$grade_class_comp<10,1,0)
print(y_4)
y_5<-ifelse(survey_data_new$grade_class_comp>=10,1,0)
print(y_5)
sector<-survey_data_new$sector.x#factor1
gender<-survey_data_new$gender#factor2
Neverenrollmentreason<-survey_data_new$never_enrol_reason#factor3
Householdtype<-survey_data_new$hhtype#factor4
umpce_quintile<-survey_data_new$`Column 2`#factor5
print(umpce_quintile)
umpce_quintile_factor <- factor(umpce_quintile, levels = 1:5)
print(umpce_quintile_factor)
institution<-survey_data_new$institution_type_last#factor6
weights<-round(survey_data_new$wgt_combined.x)
print(weights)


# Fitting the logistic regression model for y_1
log_model1 <- glm(y_1~ sector+gender+Householdtype+umpce_quintile,family = binomial(link=logit), data = survey_data_new)
summary(log_model1)
# Extracting the coefficients
coefs1 <- coef(log_model1)
print(coefs1)
# Exponentiating the coefficients to get odds ratios
odds_ratios1<- exp(coefs1)
# Printing the odds ratios
print(odds_ratios1)
plot(log_model1)


# Fit the logistic regression model for y_2
log_model2 <- glm(y_2~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model2)
# Extract the coefficients
coefs2 <- coef(log_model2)
print(coefs2)
# Exponentiate the coefficients to get odds ratios
odds_ratios2<- exp(coefs2)
# Print the odds ratios
print(odds_ratios2)
plot(log_model2)

# Fit the logistic regression model for y_3
log_model3 <- glm(y_3~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit),  data = survey_data_new)
summary(log_model3)
# Extract the coefficients
coefs3 <- coef(log_model3)
print(coefs3)
# Exponentiate the coefficients to get odds ratios
odds_ratios3<- exp(coefs3)
# Print the odds ratios
print(odds_ratios3)
plot(log_model3)

# Fit the logistic regression model for y_4
log_model4 <- glm(y_4~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model4)
# Extract the coefficients
coefs4 <- coef(log_model4)
print(coefs4)
# Exponentiate the coefficients to get odds ratios
odds_ratios4<- exp(coefs4)
# Print the odds ratios
print(odds_ratios4)
plot(log_model4)

# Fit the logistic regression model for y_5
log_model5 <- glm(y_5~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model5)
# Extract the coefficients
coefs5 <- coef(log_model5)
print(coefs5)
# Exponentiate the coefficients to get odds ratios
odds_ratios5<- exp(coefs5)
# Print the odds ratios
print(odds_ratios5)
plot(log_model5)

#logistic regressions for West Bengal
library(readxl)
survey_data_new<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx",sheet="Wb")
y_1<-ifelse(survey_data_new$enrolled=="yes",1,0)
print(y_1)
y_2<-ifelse(survey_data_new$grade_class_comp<4,1,0)
print(y_2)
y_3<-ifelse(survey_data_new$grade_class_comp>4 & survey_data_new$grade_class_comp<8,1,0)
print(y_3)
y_4<-ifelse(survey_data_new$grade_class_comp>8 & survey_data_new$grade_class_comp<10,1,0)
print(y_4)
y_5<-ifelse(survey_data_new$grade_class_comp>=10,1,0)
print(y_5)
sector<-survey_data_new$sector.x#factor1
gender<-survey_data_new$gender#factor2
Neverenrollmentreason<-survey_data_new$never_enrol_reason#factor3
Householdtype<-survey_data_new$hhtype#factor4
umpce_quintile<-survey_data_new$Column2#factor5
print(umpce_quintile)
umpce_quintile_factor <- factor(umpce_quintile, levels = 1:5)
print(umpce_quintile_factor)
institution<-survey_data_new$institution_type_last#factor6
weights<-round(survey_data_new$wgt_combined.x)
print(weights)

# Fitting the logistic regression model for y_1
log_model1 <- glm(y_1~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model1)
# Extracting the coefficients
coefs1 <- coef(log_model1)
print(coefs1)
# Exponentiating the coefficients to get odds ratios
odds_ratios1<- exp(coefs1)
# Printing the odds ratios
print(odds_ratios1)
plot(log_model1)

# Fit the logistic regression model for y_2
log_model2 <- glm(y_2~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model2)
# Extract the coefficients
coefs2 <- coef(log_model2)
print(coefs2)
# Exponentiate the coefficients to get odds ratios
odds_ratios2<- exp(coefs2)
# Print the odds ratios
print(odds_ratios2)
plot(log_model2)

# Fit the logistic regression model for y_3
log_model3 <- glm(y_3~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit),  data = survey_data_new)
summary(log_model3)
# Extract the coefficients
coefs3 <- coef(log_model3)
print(coefs3)
# Exponentiate the coefficients to get odds ratios
odds_ratios3<- exp(coefs3)
# Print the odds ratios
print(odds_ratios3)
plot(log_model3)

# Fit the logistic regression model for y_4
log_model4 <- glm(y_4~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model4)
# Extract the coefficients
coefs4 <- coef(log_model4)
print(coefs4)
# Exponentiate the coefficients to get odds ratios
odds_ratios4<- exp(coefs4)
# Print the odds ratios
print(odds_ratios4)
plot(log_model4)

# Fit the logistic regression model for y_5
log_model5 <- glm(y_5~ sector+gender+Householdtype+umpce_quintile_factor, family = binomial(link=logit), data = survey_data_new)
summary(log_model5)
# Extract the coefficients
coefs5 <- coef(log_model5)
print(coefs5)
# Exponentiate the coefficients to get odds ratios
odds_ratios5<- exp(coefs5)
# Print the odds ratios
print(odds_ratios5)
plot(log_model5)
colours()

# Load the data for WB

survey_data_wb_r<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx",sheet="Wb_Rural")
y_1_r<-ifelse(survey_data_wb_r$enrolled=="yes",1,0)
print(y_1_r)
y_2_r<-ifelse(survey_data_wb_r$grade_class_comp<4,1,0)
print(y_2_r)
y_3_r<-ifelse(survey_data_wb_r$grade_class_comp>4 & survey_data_wb_r$grade_class_comp<8,1,0)
print(y_3_r)
y_4_r<-ifelse(survey_data_wb_r$grade_class_comp>8 & survey_data_wb_r$grade_class_comp<10,1,0)
print(y_4_r)
y_5_r<-ifelse(survey_data_wb_r$grade_class_comp>=10,1,0)
print(y_5_r)

#explanatory variables for rural sector of WB
sector_wb_r<-survey_data_wb_r$sector.x#factor1
gender_wb_r<-survey_data_wb_r$gender#factor2
print(gender_wb_r)
Neverenrollmentreason_wb_r<-survey_data_wb_r$never_enrol_reason#factor3
Householdtype_wb_r<-survey_data_wb_r$hhtype#factor4
umpce_quintile_wb_r<-survey_data_wb_r$`Column2`#factor5
print(umpce_quintile_wb_r)
umpce_quintile_factor_wb_r <- factor(umpce_quintile_wb_r, levels = 1:5)
print(umpce_quintile_factor_wb_r)
institution_wb_r<-survey_data_wb_r$institution_type_last#factor6
weights_wb_r<-round(survey_data_wb_r$wgt_combined.x)
print(weights_wb_r)

survey_data_wb_u<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx",sheet="Wb_Urban")
y_1_u<-ifelse(survey_data_wb_u$enrolled=="yes",1,0)
print(y_1_u)
y_2_u<-ifelse(survey_data_wb_u$grade_class_comp<4,1,0)
print(y_2_u)
y_3_u<-ifelse(survey_data_wb_u$grade_class_comp>4 & survey_data_wb_u$grade_class_comp<8,1,0)
print(y_3_u)
y_4_u<-ifelse(survey_data_wb_u$grade_class_comp>8 & survey_data_wb_u$grade_class_comp<10,1,0)
print(y_4_u)
y_5_u<-ifelse(survey_data_wb_u$grade_class_comp>=10,1,0)
print(y_5_u)

#explanatory variables for urban sector of WB
sector_wb_u<-survey_data_wb_u$sector.x#factor1
gender_wb_u<-survey_data_wb_u$gender#factor2
print(gender_wb_u)
Neverenrollmentreason_wb_u<-survey_data_wb_u$never_enrol_reason#factor3
Householdtype_wb_u<-survey_data_wb_u$hhtype#factor4
umpce_quintile_wb_u<-survey_data_wb_u$`Column2`#factor5
print(umpce_quintile_wb_u)
umpce_quintile_factor_wb_u <- factor(umpce_quintile_wb_u, levels = 1:5)
print(umpce_quintile_factor_wb_u)
institution_wb_u<-survey_data_wb_u$institution_type_last#factor6
weights_wb_u<-round(survey_data_wb_u$wgt_combined.x)
print(weights_wb_u)


1) #contingency for wb rural gender
contingency_table1<-table(y_1_r,gender_wb_r)
print(contingency_table1)
prop_gender_wb_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_gender_wb_r_y1)
barplot(prop_gender_wb_r_y1, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_1", ylim=c(0,100), beside = TRUE, col=c("orange3","olivedrab4"))
chisq.test(prop_gender_wb_r_y1)

contingency_table2<-table(y_2_r, gender_wb_r)
print(contingency_table2)
prop_gender_wb_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_gender_wb_r_y2)
barplot(prop_gender_wb_r_y2, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_gender_wb_r_y2)

contingency_table3<-table(y_3_r, gender_wb_r)
print(contingency_table3)
prop_gender_wb_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_gender_wb_r_y3)
barplot(prop_gender_wb_r_y3, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_gender_wb_r_y3)

contingency_table4<-table(y_4_r, gender_wb_r)
print(contingency_table4)
prop_gender_wb_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_gender_wb_r_y4)
barplot(prop_gender_wb_r_y4, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_gender_wb_r_y4)

contingency_table5<-table(y_5_r, gender_wb_r)
print(contingency_table5)
prop_gender_wb_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_gender_wb_r_y5)
barplot(prop_gender_wb_r_y5, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_gender_wb_r_y5)

#contingency for wb urban gender
contingency_table1<-table(y_1_u, gender_wb_u)
print(contingency_table1)
prop_gender_wb_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_gender_wb_u_y1)
barplot(prop_gender_wb_u_y1, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_gender_wb_u_y1)

contingency_table2<-table(y_2_u, gender_wb_u)
print(contingency_table2)
prop_gender_wb_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_gender_wb_u_y2)
barplot(prop_gender_wb_u_y2, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_gender_wb_u_y2)

contingency_table3<-table(y_3_u, gender_wb_u)
print(contingency_table3)
prop_gender_wb_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_gender_wb_u_y3)
barplot(prop_gender_wb_u_y3, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_gender_wb_u_y3)

contingency_table4<-table(y_4_u, gender_wb_u)
print(contingency_table4)
prop_gender_wb_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_gender_wb_u_y4)
barplot(prop_gender_wb_u_y4, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_gender_wb_u_y4)

contingency_table5<-table(y_5_u, gender_wb_u)
print(contingency_table5)
prop_gender_wb_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_gender_wb_u_y5)
barplot(prop_gender_wb_u_y5, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_gender_wb_u_y5)


2)#contingency for wb rural Neverenrollmentreason
contingency_table1<-table(y_1_r, Neverenrollmentreason_wb_r)
print(contingency_table1)
prop_Neverenrollmentreason_wb_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_Neverenrollmentreason_wb_r_y1)
barplot(prop_gender_wb_r_y1, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_Neverenrollmentreason_wb_r_y1)

contingency_table2<-table(y_2_r, Neverenrollmentreason_wb_r)
print(contingency_table2)
prop_Neverenrollmentreason_wb_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_Neverenrollmentreason_wb_r_y2)
summary(prop_Neverenrollmentreason_wb_r_y2)
prop_Neverenrollmentreason_wb_r_y2_clean <- na.omit(prop_Neverenrollmentreason_wb_r_y2)
chisq.test(prop_Neverenrollmentreason_wb_r_y2_clean)
any(prop_Neverenrollmentreason_wb_r_y2_clean>0)

barplot(prop_gender_wb_r_y2, xlab=" Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Neverenrollmentreason for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_Neverenrollmentreason_wb_r_y2)

contingency_table3<-table(y_3_r, Neverenrollmentreason_wb_r)
print(contingency_table3)
prop_Neverenrollmentreason_wb_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_Neverenrollmentreason_wb_r_y3)
barplot(prop_gender_wb_r_y3, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_Neverenrollmentreason_wb_r_y3)

contingency_table4<-table(y_4_r, Neverenrollmentreason_wb_r)
print(contingency_table4)
prop_Neverenrollmentreason_wb_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_Neverenrollmentreason_wb_r_y4)
barplot(prop_gender_wb_r_y4, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_Neverenrollmentreason_wb_r_y4)

contingency_table5<-table(y_5_r, Neverenrollmentreason_wb_r)
print(contingency_table2)
prop_Neverenrollmentreason_wb_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_Neverenrollmentreason_wb_r_y5)
barplot(prop_gender_wb_r_y5, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_Neverenrollmentreason_wb_r_y5)

#contingency for wb urban Neverenrollmentreason
contingency_table1<-table(y_1_u, Neverenrollmentreason_wb_u)
print(contingency_table1)
prop_Neverenrollmentreason_wb_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_Neverenrollmentreason_wb_u_y1)
barplot(prop_Neverenrollmentreason_wb_u_y1, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4",))
chisq.test(prop_Neverenrollmentreason_wb_u_y1)

contingency_table2<-table(y_2_u, Neverenrollmentreason_wb_u)
print(contingency_table2)
prop_Neverenrollmentreason_wb_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_Neverenrollmentreason_wb_u_y2)
barplot(prop_Neverenrollmentreason_wb_u_y2, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_Neverenrollmentreason_wb_u_y2)

contingency_table3<-table(y_3_u, Neverenrollmentreason_wb_u)
print(contingency_table3)
prop_Neverenrollmentreason_wb_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_Neverenrollmentreason_wb_u_y3)
barplot(prop_Neverenrollmentreason_wb_u_y3, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_Neverenrollmentreason_wb_u_y3)

contingency_table4<-table(y_4_u, Neverenrollmentreason_wb_u)
print(contingency_table4)
prop_Neverenrollmentreason_wb_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_Neverenrollmentreason_wb_u_y4)
barplot(prop_Neverenrollmentreason_wb_u_y4, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Neverenrollmentreason for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_Neverenrollmentreason_wb_u_y4)

contingency_table5<-table(y_5_u, Neverenrollmentreason_wb_u)
print(contingency_table5)
prop_Neverenrollmentreason_wb_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_Neverenrollmentreason_wb_u_y5)
barplot(prop_Neverenrollmentreason_wb_u_y5, xlab="Neverenrollmentreason", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Neverenrollmentreason for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_Neverenrollmentreason_wb_u_y5)

3)#contingency for wb rural Householdtype
contingency_table1<-table(y_1_r, Householdtype_wb_r)
print(contingency_table1)
prop_Householdtype_wb_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_Householdtype_wb_r_y1)
barplot(prop_Householdtype_wb_r_y1, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_Householdtype_wb_r_y1)

contingency_table2<-table(y_2_r, Householdtype_wb_r)
print(contingency_table2)
prop_Householdtype_wb_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_Householdtype_wb_r_y2)
barplot(prop_Householdtype_wb_r_y2, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_Householdtype_wb_r_y2)

contingency_table3<-table(y_3_r, Householdtype_wb_r)
print(contingency_table3)
prop_Householdtype_wb_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_Householdtype_wb_r_y3)
barplot(prop_Householdtype_wb_r_y3, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_Householdtype_wb_r_y3)

contingency_table4<-table(y_4_r, Householdtype_wb_r)
print(contingency_table4)
prop_Householdtype_wb_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_Householdtype_wb_r_y4)
barplot(prop_Householdtype_wb_r_y4, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_Householdtype_wb_r_y4)

contingency_table5<-table(y_5_r, Householdtype_wb_r)
print(contingency_table5)
prop_Householdtype_wb_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_Householdtype_wb_r_y5)
barplot(prop_Householdtype_wb_r_y5, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_Householdtype_wb_r_y5)

#contingency for wb urban Householdtype
contingency_table1<-table(y_1_u, Householdtype_wb_u)
print(contingency_table1)
prop_Householdtype_wb_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_Householdtype_wb_u_y1)
barplot(prop_Householdtype_wb_u_y1, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_Householdtype_wb_u_y1)

contingency_table2<-table(y_2_u, Householdtype_wb_u)
print(contingency_table2)
prop_Householdtype_wb_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_Householdtype_wb_u_y2)
barplot(prop_Householdtype_wb_u_y2, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_Householdtype_wb_u_y2)

contingency_table3<-table(y_3_u, Householdtype_wb_u)
print(contingency_table3)
prop_Householdtype_wb_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_Householdtype_wb_u_y3)
barplot(prop_Householdtype_wb_u_y3, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_Householdtype_wb_u_y3)

contingency_table4<-table(y_4_u, Householdtype_wb_u)
print(contingency_table4)
prop_Householdtype_wb_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_Householdtype_wb_u_y4)
barplot(prop_Householdtype_wb_u_y4, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_Householdtype_wb_u_y4)

contingency_table5<-table(y_5_u, Householdtype_wb_u)
print(contingency_table5)
prop_Householdtype_wb_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_Householdtype_wb_u_y5)
barplot(prop_Householdtype_wb_u_y5, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_Householdtype_wb_u_y5)

4)#contingency for wb rural umpce_quintile
contingency_table1<-table(y_1_r, umpce_quintile_wb_r)
print(contingency_table1)
prop_umpce_quintile_wb_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_umpce_quintile_wb_r_y1)
barplot(prop_umpce_quintile_wb_r_y1, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_umpce_quintile_wb_r_y1)

contingency_table2<-table(y_2_r, umpce_quintile_wb_r)
print(contingency_table2)
prop_umpce_quintile_wb_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_umpce_quintile_wb_r_y2)
barplot(prop_umpce_quintile_wb_r_y2, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_umpce_quintile_wb_r_y2)

contingency_table3<-table(y_3_r, umpce_quintile_wb_r)
print(contingency_table3)
prop_umpce_quintile_wb_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_umpce_quintile_wb_r_y3)
barplot(prop_umpce_quintile_wb_r_y3, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_umpce_quintile_wb_r_y3)

contingency_table4<-table(y_4_r, umpce_quintile_wb_r)
print(contingency_table4)
prop_umpce_quintile_wb_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_umpce_quintile_wb_r_y4)
barplot(prop_umpce_quintile_wb_r_y4, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_umpce_quintile_wb_r_y4)

contingency_table5<-table(y_5_r, umpce_quintile_wb_r)
print(contingency_table5)
prop_umpce_quintile_wb_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_umpce_quintile_wb_r_y5)
barplot(prop_umpce_quintile_wb_r_y5, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
      umpce_quintile for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_umpce_quintile_wb_r_y5)

#contingency for wb urban umpce_quintile
contingency_table1<-table(y_1_u, umpce_quintile_wb_u)
print(contingency_table1)
prop_umpce_quintile_wb_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_umpce_quintile_wb_u_y1)
barplot(prop_umpce_quintile_wb_u_y1, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_umpce_quintile_wb_u_y1)

contingency_table2<-table(y_2_u, umpce_quintile_wb_u)
print(contingency_table2)
prop_umpce_quintile_wb_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_umpce_quintile_wb_u_y2)
barplot(prop_umpce_quintile_wb_u_y2, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_umpce_quintile_wb_u_y2)

contingency_table3<-table(y_3_u, umpce_quintile_wb_u)
print(contingency_table3)
prop_umpce_quintile_wb_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_umpce_quintile_wb_u_y3)
barplot(prop_umpce_quintile_wb_u_y3, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_umpce_quintile_wb_u_y3)

contingency_table4<-table(y_4_u, umpce_quintile_wb_u)
print(contingency_table4)
prop_umpce_quintile_wb_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_umpce_quintile_wb_u_y4)
barplot(prop_umpce_quintile_wb_u_y4, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_umpce_quintile_wb_u_y4)

contingency_table5<-table(y_5_u, umpce_quintile_wb_u)
print(contingency_table5)
prop_umpce_quintile_wb_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_umpce_quintile_wb_u_y5)
barplot(prop_umpce_quintile_wb_u_y5, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_umpce_quintile_wb_u_y5)

5)#contingency for wb rural institution
contingency_table1<-table(y_1_r, institution_wb_r)
print(contingency_table1)
prop_institution_wb_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_institution_wb_r_y1)
barplot(prop_institution_wb_r_y1, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_institution_wb_r_y1)

contingency_table2<-table(y_2_r, institution_wb_r)
print(contingency_table2)
prop_institution_wb_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_institution_wb_r_y2)
barplot(prop_institution_wb_r_y2, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_institution_wb_r_y2)

contingency_table3<-table(y_3_r, institution_wb_r)
print(contingency_table3)
prop_institution_wb_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_institution_wb_r_y3)
barplot(prop_institution_wb_r_y3, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_institution_wb_r_y3)

contingency_table4<-table(y_4_r, institution_wb_r)
print(contingency_table4)
prop_institution_wb_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_institution_wb_r_y4)
barplot(prop_institution_wb_r_y4, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_institution_wb_r_y4)

contingency_table5<-table(y_5_r, institution_wb_r)
print(contingency_table5)
prop_institution_wb_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_institution_wb_r_y5)
barplot(prop_institution_wb_r_y5, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
      institution for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_institution_wb_r_y5)

#contingency for wb urban institution
contingency_table1<-table(y_1_u, institution_wb_u)
print(contingency_table1)
prop_institution_wb_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_institution_wb_u_y1)
barplot(prop_institution_wb_u_y1, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for urban sector of y_1", ylim=c(0,100), beside = TRUE, col=c("orange3","olivedrab4",))
chisq.test(prop_institution_wb_u_y1)

contingency_table2<-table(y_2_u, institution_wb_u)
print(contingency_table2)
prop_institution_wb_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_institution_wb_u_y2)
barplot(prop_institution_wb_u_y2, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_institution_wb_u_y2)

contingency_table3<-table(y_3_u, institution_wb_u)
print(contingency_table3)
prop_institution_wb_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_institution_wb_u_y3)
barplot(prop_institution_wb_u_y3, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_institution_wb_u_y3)

contingency_table4<-table(y_4_u, institution_wb_u)
print(contingency_table4)
prop_institution_wb_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_institution_wb_u_y4)
barplot(prop_institution_wb_u_y4, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_institution_wb_u_y4)

contingency_table5<-table(y_5_u, institution_wb_u)
print(contingency_table5)
prop_institution_wb_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_institution_wb_u_y5)
barplot(prop_institution_wb_u_y5, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_institution_wb_u_y5)

5)#contingency for wb rural institution
contingency_table1<-table(y_1_r, institution_wb_r)
print(contingency_table1)
prop_institution_wb_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_institution_wb_r_y1)
barplot(prop_institution_wb_r_y1, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4",))
chisq.test(prop_institution_wb_r_y1)

contingency_table2<-table(y_2_r, institution_wb_r)
print(contingency_table2)
prop_institution_wb_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_institution_wb_r_y2)
barplot(prop_institution_wb_r_y2, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_institution_wb_r_y2)

contingency_table3<-table(y_3_r, institution_wb_r)
print(contingency_table2)
prop_institution_wb_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_institution_wb_r_y2)
barplot(prop_institution_wb_r_y3, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_institution_wb_r_y3)

contingency_table4<-table(y_4_r, institution_wb_r)
print(contingency_table4)
prop_institution_wb_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_institution_wb_r_y4)
barplot(prop_institution_wb_r_y4, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_institution_wb_r_y4)

contingency_table5<-table(y_5_r, institution_wb_r)
print(contingency_table2)
prop_institution_wb_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_institution_wb_r_y5)
barplot(prop_institution_wb_r_y5, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
      institution for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_institution_wb_r_y5)

#contingency for wb urban institution
contingency_table1<-table(y_1_u, institution_wb_u)
print(contingency_table1)
prop_institution_wb_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_institution_wb_u_y1)
barplot(prop_institution_wb_u_y1, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4",))
chisq.test(prop_institution_wb_u_y1)

contingency_table2<-table(y_2_u, institution_wb_u)
print(contingency_table2)
prop_institution_wb_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_institution_wb_u_y2)
barplot(prop_institution_wb_u_y2, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_institution_wb_u_y2)

contingency_table3<-table(y_3_u, institution_wb_u)
print(contingency_table3)
prop_institution_wb_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_institution_wb_u_y3)
barplot(prop_institution_wb_u_y3, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_institution_wb_u_y3)

contingency_table4<-table(y_4_u, institution_wb_u)
print(contingency_table4)
prop_institution_wb_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_institution_wb_u_y4)
barplot(prop_institution_wb_u_y4, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       institution for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_institution_wb_u_y4)

contingency_table5<-table(y_5_u, institution_wb_u)
print(contingency_table5)
prop_institution_wb_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_institution_wb_u_y5)
barplot(prop_institution_wb_u_y5, xlab="institution", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        institution for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_institution_wb_u_y5)


# Load the data for India
library(readxl)
survey_data_r<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx",sheet="India_Rural_New")
y_1_r<-ifelse(survey_data_r$enrolled=="yes",1,0)
print(y_1_r)
y_2_r<-ifelse(survey_data_r$grade_class_comp<4,1,0)
print(y_2_r)
y_3_r<-ifelse(survey_data_r$grade_class_comp>4 & survey_data_r$grade_class_comp<8,1,0)
print(y_3_r)
y_4_r<-ifelse(survey_data_r$grade_class_comp>8 & survey_data_r$grade_class_comp<10,1,0)
print(y_4_r)
y_5_r<-ifelse(survey_data_r$grade_class_comp>=10,1,0)
print(y_5_r)

#explanatory variables for rural sector of India
sector_r<-survey_data_r$sector.x#factor1
gender_r<-survey_data_r$gender#factor2
print(gender_r)
Neverenrollmentreason_wb_r<-survey_data_wb_r$never_enrol_reason
Householdtype_r<-survey_data_r$hhtype#factor3
umpce_quintile_r<-survey_data_r$`Column 2`#factor4
print(umpce_quintile_r)
umpce_quintile_factor_r <- factor(umpce_quintile_r, levels = 1:5)
print(umpce_quintile_factor_r)
institution_wb_r<-survey_data_wb_r$institution_type_last
weights_r<-round(survey_data_r$wgt_combined.x)
print(weights_r)

library(readxl)
survey_data_u<-read_excel("C:/Users/TANISHA/Downloads/Kajal Ma'am/Sample_Survey_UMPCE.xlsx",sheet="India_Urban _New")
y_1_u<-ifelse(survey_data_u$enrolled=="yes",1,0)
print(y_1_u)
y_2_u<-ifelse(survey_data_u$grade_class_comp<4,1,0)
print(y_2_u)
y_3_u<-ifelse(survey_data_u$grade_class_comp>4 & survey_data_u$grade_class_comp<8,1,0)
print(y_3_u)
y_4_u<-ifelse(survey_data_u$grade_class_comp>8 & survey_data_u$grade_class_comp<10,1,0)
print(y_4_u)
y_5_u<-ifelse(survey_data_u$grade_class_comp>=10,1,0)
print(y_5_u)

#explanatory variables for urban sector of India
sector_u<-survey_data_u$sector.x#factor1
gender_u<-survey_data_u$gender#factor2
print(gender_u)
Neverenrollmentreason_u<-survey_data_u$never_enrol_reason
Householdtype_u<-survey_data_u$hhtype#factor4
umpce_quintile_u<-survey_data_u$`Column 2`#factor5
print(umpce_quintile_u)
umpce_quintile_factor_u <- factor(umpce_quintile_u, levels = 1:5)
print(umpce_quintile_factor_u)
institution_wb_u<-survey_data_wb_u$institution_type_last
weights_u<-round(survey_data_u$wgt_combined.x)
print(weights_u)


1) #contingency for India rural gender
contingency_table1<-table(y_1_r,gender_r)
print(contingency_table1)
prop_gender_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_gender_r_y1)
barplot(prop_gender_r_y1, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_1", ylim=c(0,100), beside = TRUE, col=c("orange3","olivedrab4"))
chisq.test(prop_gender_r_y1)

contingency_table2<-table(y_2_r, gender_r)
print(contingency_table2)
prop_gender_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_gender_r_y2)
barplot(prop_gender_r_y2, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_gender_r_y2)

contingency_table3<-table(y_3_r, gender_r)
print(contingency_table3)
prop_gender_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_gender_r_y3)
barplot(prop_gender_r_y3, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_gender_r_y3)

contingency_table4<-table(y_4_r, gender_r)
print(contingency_table4)
prop_gender_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_gender_r_y4)
barplot(prop_gender_r_y4, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_gender_r_y4)

contingency_table5<-table(y_5_r, gender_r)
print(contingency_table5)
prop_gender_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_gender_r_y5)
barplot(prop_gender_r_y5, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_gender_r_y5)

#contingency for India urban gender
contingency_table1<-table(y_1_u, gender_u)
print(contingency_table1)
prop_gender_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_gender_u_y1)
barplot(prop_gender_u_y1, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_gender_u_y1)

contingency_table2<-table(y_2_u, gender_u)
print(contingency_table2)
prop_gender_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_gender_u_y2)
barplot(prop_gender_u_y2, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_gender_u_y2)

contingency_table3<-table(y_3_u, gender_u)
print(contingency_table3)
prop_gender_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_gender_u_y3)
barplot(prop_gender_u_y3, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_gender_u_y3)

contingency_table4<-table(y_4_u, gender_u)
print(contingency_table4)
prop_gender_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_gender_u_y4)
barplot(prop_gender_u_y4, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_gender_u_y4)

contingency_table5<-table(y_5_u, gender_u)
print(contingency_table5)
prop_gender_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_gender_u_y5)
barplot(prop_gender_u_y5, xlab="Gender", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        genders for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_gender_u_y5)



2)#contingency for India rural Householdtype
contingency_table1<-table(y_1_r, Householdtype_r)
print(contingency_table1)
prop_Householdtype_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_Householdtype_r_y1)
barplot(prop_Householdtype_r_y1, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_Householdtype_r_y1)

contingency_table2<-table(y_2_r, Householdtype_r)
print(contingency_table2)
prop_Householdtype_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_Householdtype_r_y2)
barplot(prop_Householdtype_r_y2, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_Householdtype_r_y2)

contingency_table3<-table(y_3_r, Householdtype_r)
print(contingency_table3)
prop_Householdtype_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_Householdtype_r_y3)
barplot(prop_Householdtype_r_y3, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_Householdtype_r_y3)

contingency_table4<-table(y_4_r, Householdtype_r)
print(contingency_table4)
prop_Householdtype_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_Householdtype_r_y4)
barplot(prop_Householdtype_r_y4, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_Householdtype_r_y4)

contingency_table5<-table(y_5_r, Householdtype_r)
print(contingency_table5)
prop_Householdtype_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_Householdtype_r_y5)
barplot(prop_Householdtype_r_y5, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_Householdtype_r_y5)

#contingency for India urban Householdtype
contingency_table1<-table(y_1_u, Householdtype_u)
print(contingency_table1)
prop_Householdtype_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_Householdtype_u_y1)
barplot(prop_Householdtype_u_y1, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_Householdtype_u_y1)

contingency_table2<-table(y_2_u, Householdtype_u)
print(contingency_table2)
prop_Householdtype_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_Householdtype_u_y2)
barplot(prop_Householdtype_u_y2, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_Householdtype_u_y2)

contingency_table3<-table(y_3_u, Householdtype_u)
print(contingency_table3)
prop_Householdtype_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_Householdtype_u_y3)
barplot(prop_Householdtype_u_y3, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_Householdtype_u_y3)

contingency_table4<-table(y_4_u, Householdtype_u)
print(contingency_table4)
prop_Householdtype_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_Householdtype_u_y4)
barplot(prop_Householdtype_u_y4, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       Householdtype for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_Householdtype_u_y4)

contingency_table5<-table(y_5_u, Householdtype_u)
print(contingency_table5)
prop_Householdtype_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_Householdtype_u_y5)
barplot(prop_Householdtype_u_y5, xlab="Householdtype", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        Householdtype for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_Householdtype_u_y5)

3)#contingency for India rural umpce_quintile
contingency_table1<-table(y_1_r, umpce_quintile_r)
print(contingency_table1)
prop_umpce_quintile_r_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_umpce_quintile_r_y1)
barplot(prop_umpce_quintile_r_y1, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for rural sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_umpce_quintile_r_y1)

contingency_table2<-table(y_2_r, umpce_quintile_r)
print(contingency_table2)
prop_umpce_quintile_r_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_umpce_quintile_r_y2)
barplot(prop_umpce_quintile_r_y2, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for rural sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_umpce_quintile_r_y2)

contingency_table3<-table(y_3_r, umpce_quintile_r)
print(contingency_table3)
prop_umpce_quintile_r_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_umpce_quintile_r_y3)
barplot(prop_umpce_quintile_r_y3, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for rural sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_umpce_quintile_r_y3)

contingency_table4<-table(y_4_r, umpce_quintile_r)
print(contingency_table4)
prop_umpce_quintile_r_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_umpce_quintile_r_y4)
barplot(prop_umpce_quintile_r_y4, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for rural sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_umpce_quintile_r_y4)

contingency_table5<-table(y_5_r, umpce_quintile_r)
print(contingency_table5)
prop_umpce_quintile_r_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_umpce_quintile_r_y5)
barplot(prop_umpce_quintile_r_y5, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
      umpce_quintile for rural sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_umpce_quintile_r_y5)

#contingency for India urban umpce_quintile
contingency_table1<-table(y_1_u, umpce_quintile_u)
print(contingency_table1)
prop_umpce_quintile_u_y1<-prop.table(contingency_table1, margin = 2)*100
print(prop_umpce_quintile_u_y1)
barplot(prop_umpce_quintile_u_y1, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for urban sector of y_1", ylim=c(0,100), beside = TRUE,  col=c("orange3","olivedrab4"))
chisq.test(prop_umpce_quintile_u_y1)

contingency_table2<-table(y_2_u, umpce_quintile_u)
print(contingency_table2)
prop_umpce_quintile_u_y2<-prop.table(contingency_table2, margin = 2)*100
print(prop_umpce_quintile_u_y2)
barplot(prop_umpce_quintile_u_y2, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for urban sector of y_2", ylim=c(0,100), beside = TRUE, col=c("plum4","brown4"))
chisq.test(prop_umpce_quintile_u_y2)

contingency_table3<-table(y_3_u, umpce_quintile_u)
print(contingency_table3)
prop_umpce_quintile_u_y3<-prop.table(contingency_table3, margin = 2)*100
print(prop_umpce_quintile_u_y3)
barplot(prop_umpce_quintile_u_y3, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for urban sector of y_3", ylim=c(0,100), beside = TRUE, col=c("bisque4","seagreen"))
chisq.test(prop_umpce_quintile_u_y3)

contingency_table4<-table(y_4_u, umpce_quintile_u)
print(contingency_table4)
prop_umpce_quintile_u_y4<-prop.table(contingency_table4, margin = 2)*100
print(prop_umpce_quintile_u_y4)
barplot(prop_umpce_quintile_u_y4, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
       umpce_quintile for urban sector of y_4", ylim=c(0,100), beside = TRUE, col=c("peru","cyan4"))
chisq.test(prop_umpce_quintile_u_y4)

contingency_table5<-table(y_5_u, umpce_quintile_u)
print(contingency_table5)
prop_umpce_quintile_u_y5<-prop.table(contingency_table5, margin = 2)*100
print(prop_umpce_quintile_u_y5)
barplot(prop_umpce_quintile_u_y5, xlab="umpce_quintile", ylab="% of individuals", main=" Proportion (per 100) of students in different 
        umpce_quintile for urban sector of y_5", ylim=c(0,100), beside = TRUE, col=c("maroon","grey45"))
chisq.test(prop_umpce_quintile_u_y5)

















