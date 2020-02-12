library(dplyr)
library(mice)

load(file=paste0('data_1993.rdata')) 


# subsetted data
dat93_subset<- dat93 %>% dplyr::select(c(A1, A2, A3, A4, A5, A6, A7, A8, A9_C, A9_E, C1_A, 
                                         C1_B, C6, C7, C8, D4, D5, E1_A, E1_B,E1_C,E1_D,E1_E,
                                         E1_F,E1_G,E1_H,E1_I,E1_J,E1_K,E1_L, E1_M, E12A, F1,
                                         F5, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, 
                                         G13, G14, G15))
df = dat93_subset

## Handle response variable and missing values
df$drinks_binary = ifelse(df$C6==1,0,1)

# remove missing C6 values- 40 values
df = df[!is.na(df$C6),]

# remove mis matched missing for C7, C8- 35 values
notmatching = which((!is.na(df$C8)&is.na(df$C7))|(is.na(df$C8)&!is.na(df$C7)))
df = df[-notmatching,]

# set NA to 0
df$C8[is.na(df$C8)] = 0
df$C7[is.na(df$C7)] = 0

# change C8 to be mean of each range
ranges = matrix(c(1,2,3,5,6,9,10,19,20,39,40,50),byrow=T,ncol=2)
mean_ranges = rowMeans(ranges)
for (j in 1:length(mean_ranges)){
  df$C7[df$C7==j] = mean_ranges[j]
}

df$num_of_drinks = df$C7 * df$C8 # maybe need to round up to be whole number
# MAYBE CHECK WITH C6

## Missing Data

colSums(is.na(df))
## drop all missing data, about 2000 entries
# loc_missing = which(is.na(df),arr.ind=T)
# df = df[-unique(loc_missing),]

# is gpa == 10? set to NA so it gets imputed
df$F5[df$F5==10] = NA

# mice missing
imp = mice(df,m=3, method = "pmm")
df = mice::complete(imp)


## Handle As
# group ages
df$age_15_17 = ifelse(df$A1==15|df$A1==16|df$A1==17,1,0)
df$age_18_20 = ifelse(df$A1==18|df$A1==19|df$A1==20,1,0)
df$age_21_22 = ifelse(df$A1==21|df$A1==22,1,0)
df$age_23up = ifelse(df$A1==23|df$A1==24|df$A1==25|df$A1==26,1,0)

# change gender to 0,1
df$gender = ifelse(df$A2==2,1,0)

# # 0,1 for transfer
# df$transfer = ifelse(df$A4==2|df$A4==3,1,0)

# rename year in school
df$year_in_school = df$A3

# where do you live
df$subfree_on_housing = ifelse(df$D4 != 1,1,0)
df$on_housing = ifelse((df$A5==1|df$A5==2|df$A5==4|df$A5==5)&df$D4==1,1,0)
df$off_housing = ifelse((df$A5==3|df$A5==6)&df$D4==1,1,0)

# who do you liv with
df$no_roommate = ifelse(df$A6==1,1,0)
df$roommate = ifelse(df$A6==2,1,0)
df$weird_roomate = ifelse(df$A6==3|df$A6==4|df$A6==5|df$A6>=6,1,0)

# 0,1 for frat
df$greek = ifelse(df$A8==1,1,0)

# A9 questions
df$academic_important = ifelse(df$A9_C==1|df$A9_C==2,1,0)
df$religion_important = ifelse(df$A9_E==1|df$A9_E==2,1,0)

#########################################


## Handle Cs
df$prohib_medical = ifelse(df$C1_A==1,1,0)
df$prohib_religion = ifelse(df$C1_B==1,1,0)

#########################################


## Handle Es

# E1, other drug use
df$tobac = ifelse(df$E1_L==4|df$E1_M==4,1,0)
df$marijuana = ifelse(df$E1_A==4,1,0)
drugs_loc1 = which(colnames(df)=="E1_B")
drugs_loc2 = which(colnames(df)=="E1_K")
df$hard_drugs = ifelse(rowSums(df[,drugs_loc1:drugs_loc2]==4)>=1,1,0)

# meeting w alc group
df$aa_attendance = ifelse(df$E12A == 1,0,1)

#########################################

## Handle Fs

# happiness
df$happy = ifelse(df$F1 == 1|df$F1==2,1,0)
df$somewhat_happy = ifelse(df$F1==3,1,0)
df$not_happy = ifelse(df$F1==4,1,0)

# GPA
# convert to numeric
gpa_value = seq(from= 1, to = 4, by = .3333333)
# no D+
gpa_value = gpa_value[-2]
gpa_value = rev(gpa_value)
df$gpa = df$F5
for (j in 1:9){
  df$gpa[df$F5==j]=gpa_value[j]
}

#########################################


## Handle Gs

# relationship status
df$single = ifelse(df$G1==1&df$D5==2,1,0)
df$relationship = ifelse(df$G1==1&df$D5==1,1,0)
df$married = ifelse(df$G1==2,1,0)
df$div_sep_wid = ifelse(df$G1==3|df$G1==4|df$G1==5,1,0)

# race
df$hispanic = ifelse(df$G2==1,1,0)
df$white = ifelse(df$G3==1&df$G2==2,1,0)
df$black = ifelse(df$G3==2&df$G2==2,1,0)
df$asian = ifelse(df$G3==3&df$G2==2,1,0)
df$native = ifelse(df$G3==4&df$G2==2,1,0)
df$other = ifelse(df$G3==5&df$G2==2,1,0)

# religion
df$religion_none = ifelse(df$G4==1,1,0)
df$religion_catholic = ifelse(df$G4==2,1,0)
df$religion_jewish = ifelse(df$G4==3,1,0)
df$religion_moslem = ifelse(df$G4==4,1,0)
df$religion_prot = ifelse(df$G4==5,1,0)
df$religion_other = ifelse(df$G4==6,1,0)


df$weight = df$G6
range(df$G6)
range(df$G7)
df$height = df$G7

# create hs drinking metric
drinking_temp = df$G8
drinking_temp[df$G8==1] = 0
for (j in 1:length(mean_ranges)){
  drinking_temp[df$G8==(j+1)] = mean_ranges[j]
}

occasions_temp = df$G9
for (j in 1:10){
  occasions_temp[df$G9==j]=j-1
}

df$HS_num_of_drinks = drinking_temp * occasions_temp # maybe need to round up to be whole number

df$mom_heavy_drinker = ifelse(df$G12==7|df$G12==6,1,0)
df$dad_heavy_drinker = ifelse(df$G11==7|df$G11==6,1,0)

# create max parent education variable
# set unknowns to 0
df$G14[df$G14==5|df$G14==6]=0
df$G15[df$G15==5|df$G15==6]=0
max_parent_edu = pmax(df$G14,df$G15)
df$max_parent_edu_DK = ifelse(max_parent_edu==0,1,0)
df$max_parent_edu_someHS = ifelse(max_parent_edu==1,1,0)
df$max_parent_edu_HS = ifelse(max_parent_edu==2,1,0)
df$max_parent_edu_someCol = ifelse(max_parent_edu==3,1,0)
df$max_parent_edu_col = ifelse(max_parent_edu==4,1,0)


## drop all original variables
G15_loc = which(colnames(df)=="G15")
df = df[,-c(1:G15_loc)]
save(df, file="df.rdata")
