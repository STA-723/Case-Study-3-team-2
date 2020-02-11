library(dplyr)
years = c(1993,1997,1999,2001)

for (x in 1:length(years)){
  load(file=paste0('data_',years[x],'.rdata')) 
}

dat93$study_year = years[1]
dat97$study_year = years[2]
dat99$study_year = years[3]
dat01$study_year = years[4]

# explore
dim(dat93)
dim(dat01)
## all have different numbers of variables. Could drop the different columns 
## and cbind all to have more data

# load in 93 subsetted data
dat93_subset<- dat93 %>% dplyr::select(c(A1, A2, A3, A4, A5, A6, A7, A8, A9_C, A9_E, C1_A, C1_B, C7, C8, E1_A, E1_B,E1_C,E1_D,E1_E,E1_F,E1_G,E1_H,E1_I,E1_J,E1_K,E1_L, E1_M, F5, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, G13, G14, G15))
df = dat93_subset


# explore response variable
notmatching = which((!is.na(df$C8)&is.na(df$C7))|(is.na(df$C8)&!is.na(df$C7)))
subset(df[notmatching,],select=c(C8,C7))
# doesn't make sense. drop these
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


# Missing Data
colSums(is.na(df))
## drop all missing data, about 2000 entries
loc_missing = which(is.na(df),arr.ind=T)
df = df[-unique(loc_missing),]


## Handle As
# rename
df$age = df$A1
# change gender to 0,1
df$gender = ifelse(df$A2==2,1,0)
# 0,1 for transfer
df$transfer = ifelse(df$A4==2|df$A4==3,1,0)
# 0,1 for frat
df$greek = ifelse(df$A8==1,1,0)
### TO DO- HANDLE A6, A7, A9

## Handle Cs
df$prohib_medical = ifelse(df$C1_A==1,1,0)
df$prohib_religion = ifelse(df$C1_B==1,1,0)

## Handle Es
df$tobac = ifelse(df$E1_L==4|df$E1_M==4,1,0)
df$marijuana = ifelse(df$E1_A==4,1,0)
drugs_loc1 = which(colnames(df)=="E1_B")
drugs_loc2 = which(colnames(df)=="E1_K")
df$hard_drugs = ifelse(rowSums(df[,drugs_loc1:drugs_loc2]==4)>=1,1,0)

## Handle Gs
df$single = ifelse(df$G1==1,1,0)
df$married = ifelse(df$G1==2,1,0)
df$divorced = ifelse(df$G1==3,1,0)
df$separated = ifelse(df$G1==4,1,0)
df$widowed = ifelse(df$G1==5,1,0)

df$hispanic = ifelse(df$G2==1,1,0)
df$white = ifelse(df$G3==1,1,0)
df$black = ifelse(df$G3==2,1,0)
df$asian = ifelse(df$G3==3,1,0)
df$native = ifelse(df$G3==4,1,0)
df$other = ifelse(df$G3==5,1,0)

df$raised_religous = ifelse(df$G4!=1,1,0)

df$healthy_now = ifelse(df$G5==1|df$G5==2|df$G5==3,1,0)

df$weight = df$G6
range(df$G6)
range(df$G7)
df$height = df$G7

# create hs drinking metric
df$G8[df$G8==0] = 1
for (j in 1:length(mean_ranges)){
  df$G8[df$G8==(j+1)] = mean_ranges[j+1]
}

for (j in 1:10){
  df$G9[df$G9==j]=j-1
}

df$HS_num_of_drinks = df$G8 * df$G9 # maybe need to round up to be whole number

df$parent_heavy_drinker = ifelse(df$G11==5|df$G11==6|df$G12==5|df$G12==6,1,0)

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




