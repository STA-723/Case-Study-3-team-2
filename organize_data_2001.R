# imports
library(dplyr)
library(mice)

# data subset
load('data_2001.rdata')
data01_subset <- dat01 %>% dplyr::select(c(G9, G10, G11, A6, B8, A7A, A7B, A7C, A7D, G2, G3A, G3B, G3C, G3D,
                                           G3E, A1, A2, A3, G1, G4, A5, A8C, A8H, B2, C19A, C19B, C19C, C19D,
                                           C19E, C19F, C19G, C19H, C22A, C22K, E1A, E1B, E1C, E1D, E1E, E1F,
                                           E1G, E1H, E1I, E1J, E1K, E1L, E1M, E1N, E1O, E1P, E1Q, E1R, E1S,
                                           E1T, E25, F1, F5, G7, G8, G14, G15, G17, G18, C10, C11, C12))
df <- data01_subset

# handle response variable and missing values
df$drinks_binary = ifelse(df$C10==1, 0, 1)

# remove missing C10 values
df <- df[!is.na(df$C10),]

# remove mismatched missingness for C11, C12
notmatching = which((!is.na(df$C12) & is.na(df$C11)) | (is.na(df$C12) & !is.na(df$C11)))
df <- df[-notmatching,]

# set NA to 0
df$C11[is.na(df$C11)] <- 0
df$C12[is.na(df$C12)] <- 0

# change C11 to be mean of each range
ranges = matrix(c(1, 2, 3, 5, 6, 9, 10, 19, 20, 39, 40, 50), byrow=T, ncol=2)
mean_ranges = rowMeans(ranges)
for(j in 1:length(mean_ranges)) {
  df$C11[df$C11==j] = mean_ranges[j]
}

df$num_of_drinks = df$C11 * df$C12 # maybe need to round up to whole number

# missing Data
colSums(is.na(df))

# set gpa == 10 to NA to get imputed
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
df$gender = ifelse(df$A2==0,1,0)

# rename year in school
df$year_in_school = df$A3

# where do you live
df$subfree_on_housing = ifelse(df$B8 == 1,1,0)
df$on_housing = ifelse((df$A6==1|df$A6==2|df$A6==3)&df$B8!=1,1,0)
df$off_housing = ifelse((df$A6==4|df$A6==5|df$A6==6)&df$B8!=1,1,0)

# who do you live with
df$no_roommate = ifelse(df$A7A==1,1,0)
df$roommate = ifelse(df$A7B==1,1,0)
df$weird_roomate = ifelse(df$A7C==1|df$A7D==1,1,0)

# 0,1 for frat
df$greek = ifelse(df$A5==1,1,0)

# A9 questions
df$academic_important = ifelse(df$A8C==1|df$A8C==2,1,0)
df$religion_important = ifelse(df$A8H==1|df$A8H==2,1,0)

#########################################


## Handle Cs
df$prohib_medical = ifelse(df$C22A==1|df$C22A==2,1,0)
df$prohib_religion = ifelse(df$C22K==1|df$C22K==2,1,0)

#########################################


## Handle Es

# E1, other drug use
df$tobac = ifelse(df$E1P==4|df$E1Q==4|df$E1R==4|df$E1S==4|df$E1T==4,1,0)
df$marijuana = ifelse(df$E1A==4,1,0)
drugs_loc1 = which(colnames(df)=="E1B")
drugs_loc2 = which(colnames(df)=="E1O")
df$hard_drugs = ifelse(rowSums(df[,drugs_loc1:drugs_loc2]==4)>=1,1,0)

# meeting w alc group
df$aa_attendance = ifelse(df$E25 == 1,1,0)

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
df$single = ifelse(df$G1==1,1,0)
df$relationship = 0
df$married = ifelse(df$G1==2,1,0)
df$div_sep_wid = ifelse(df$G1==3|df$G1==4|df$G1==5,1,0)

# race
df$hispanic = ifelse(df$G2==1,1,0)
df$white = ifelse(df$G3A==1&df$G2==0,1,0)
df$black = ifelse(df$G3B==1&df$G2==0,1,0)
df$asian = ifelse(df$G3C==1&df$G2==0,1,0)
df$native = ifelse(df$G3D==1&df$G2==0,1,0)
df$other = ifelse(df$G3E==1&df$G2==0,1,0)

# religion
df$religion_none = ifelse(df$G4==1,1,0)
df$religion_catholic = ifelse(df$G4==2,1,0)
df$religion_jewish = ifelse(df$G4==3,1,0)
df$religion_moslem = ifelse(df$G4==4,1,0)
df$religion_prot = ifelse(df$G4==5,1,0)
df$religion_other = ifelse(df$G4==6,1,0)


# change weight to be middle of range
ranges = matrix(c(105, 117, 118, 128, 129, 135, 136, 146, 147, 160, 161, 170, 171, 190, 191, 210), byrow=T, ncol=2)
mean_ranges2 = rowMeans(ranges)
for(j in 1:length(mean_ranges2)) {
  df$G7[df$G7==j] = mean_ranges2[j]
}
df$weight = df$G7

# change height to be middle of range
ranges = matrix(c(62, 63, 64, 64, 65, 65, 66, 66, 67, 69, 70, 70, 71, 72, 73, 74), byrow=T, ncol=2)
mean_ranges3 = rowMeans(ranges)
for(j in 1:length(mean_ranges3)) {
  df$G8[df$G8==j] = mean_ranges3[j]
}
df$height = df$G8

# create hs drinking metric
drinking_temp = df$G9
drinking_temp[df$G9==0] = 0
for (j in 1:length(mean_ranges)){
  drinking_temp[df$G9==j] = mean_ranges[j]
}

occasions_temp = df$G10
for (j in 0:9){
  occasions_temp[df$G9==j]=j
}

df$HS_num_of_drinks = drinking_temp * occasions_temp # maybe need to round up to be whole number

df$mom_heavy_drinker = ifelse(df$G15==7|df$G15==6,1,0)
df$dad_heavy_drinker = ifelse(df$G14==7|df$G14==6,1,0)

# create max parent education variable
# set unknowns to 0
df$G17[df$G17==5|df$G17==6]=0
df$G18[df$G18==5|df$G18==6]=0
max_parent_edu = pmax(df$G17,df$G18)
df$max_parent_edu_DK = ifelse(max_parent_edu==0,1,0)
df$max_parent_edu_someHS = ifelse(max_parent_edu==1,1,0)
df$max_parent_edu_HS = ifelse(max_parent_edu==2,1,0)
df$max_parent_edu_someCol = ifelse(max_parent_edu==3,1,0)
df$max_parent_edu_col = ifelse(max_parent_edu==4,1,0)


## drop all original variables
C12_loc = which(colnames(df)=="C12")
df = df[,-c(1:C12_loc)]
save(df, file="df.rdata")
