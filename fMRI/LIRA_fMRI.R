
if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)}

pacman::p_load(tidyverse, dplyr, plyr, car, misty)

analysis_path <- file.path('~/OBIWAN/DERIVATIVES/BEHAV') 
data_path <- file.path('~/Desktop/SwitchDrive/LIRA/data/HED.RData') 
output <- read.table("~/Desktop/SwitchDrive/LIRA/fMRI/output.txt")

df = as_tibble(cbind(str_sub(output$V1,10,12), str_sub(output$V1,14,16)))
colnames(df) <- c("id", "session")
df$session = ifelse(df$session == "pre", 0, 1)

setwd(analysis_path)

load(data_path)

#keep only fMRI datya available
fMRI_HED = merge(HED, df, by = c("id", "session"))

# `%notin%` <- Negate(`%in%`)
# #remove missing fmri data
# fMRI_HED = HED %>% filter(id %in% c(205, 206, 207, 211, 215, 218, 221, 227, 229, 230, 231, 232, 244, 248, 251, 252, 253, 254, 262, 268, 202, 203, 204, 209, 213, 217, 220, 224, 225, 235, 236, 237, 238, 239, 241, 246, 250, 259, 264, 265, 266, 269, 270));

#fMRI_HED = fMRI_HED %>% filter(session %in% c(0));

#filter(id %notin% c(201, 208, 210, 214, 216, 219, 222, 223, 226, 228, 233, 234, 240, 242, 245, 247, 249, 256, 258, 263, 267))


# INPUT FOR FMRI ------------------------------------------------------------------- 
fMRI_HED = fMRI_HED[order(fMRI_HED$session, fMRI_HED$id, fMRI_HED$trial),]
fMRI_HED$trial  = as.numeric(fMRI_HED$trial)
IDX = unique(fMRI_HED$id)
init <- 1:length(fMRI_HED$id)
fMRI_HED$InputFile = init
for(i in 1:length(init)){
  if (as.numeric(as.character(fMRI_HED$session[i])) == 0) {
    if (fMRI_HED$trial[i] > 9) {
      fMRI_HED$InputFile[i] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-06_full/sub-obese',fMRI_HED$id[i],'/pre/output/','beta_00', fMRI_HED$trial[i],'.nii', sep ='')
    }
    else {fMRI_HED$InputFile[i] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-06_full/sub-obese',fMRI_HED$id[i],'/pre/output/','beta_000', fMRI_HED$trial[i],'.nii', sep ='')}
  }
  else{ 
    if (fMRI_HED$trial[i] > 9) {
      fMRI_HED$InputFile[i] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-06_full/sub-obese',fMRI_HED$id[i],'/post/output/','beta_00', fMRI_HED$trial[i],'.nii', sep ='')
    }
    else {fMRI_HED$InputFile[i] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-06_full/sub-obese',fMRI_HED$id[i],'/post/output/','beta_000', fMRI_HED$trial[i],'.nii', sep ='')}
    }
}
colnames(fMRI_HED)[colnames(fMRI_HED) == 'id'] <- 'Subj'

fMRI_HED$Subj = droplevels(fMRI_HED$Subj); 
fMRI_HED$condition = as.factor(revalue(fMRI_HED$condition, c("-1"="Neutral", "1"="Reward"))); 
fMRI_HED$session = as.factor(revalue(as.factor(fMRI_HED$session), c("0"="Pre", "1"="Post"))); 
fMRI_HED$intervention = as.factor(revalue(as.factor(fMRI_HED$intervention), c("0"="Placebo", "1"="Treatment"))); 
fMRI_HED$trialxcondition = scale(as.numeric(fMRI_HED$trialxcondition), scale = F)
fMRI_HED$perceived_liking = scale(fMRI_HED$perceived_liking, scale = F)
fMRI_HED$perceived_intensity = scale(fMRI_HED$perceived_intensity, scale = F)
fMRI_HED$piss = scale(fMRI_HED$piss, scale = F)

fMRI_HED = select(fMRI_HED, c(Subj, session, intervention, perceived_liking, perceived_intensity, condition, trialxcondition, trial, age, gender, BMI_diff,  reelin_diff, GLP_diff, piss, hungry, InputFile))

#imput mean (0) for the two covariate (MAR) so we can still compute modle with them (missing values fot 2 participant 262 and 232)
#PAV.means$thirsty[is.na(PAV.means$thirsty)] <- 0 ; PAV.means$hungry[is.na(PAV.means$hungry)] <- 0 


path <-'~/OBIWAN/DERIVATIVES/GLM/AFNI/hedonicreactivity/LIRA'
write.table(fMRI_HED, (file.path(path, "HED_LMER.txt")), row.names = F, sep="\t")

#, BMI_diff,  reelin_diff, GLP_diff











# if(!require(pacman)) {
#   install.packages("pacman")
#   library(pacman)}
# 
# pacman::p_load(tidyverse, dplyr, plyr, car, misty)
# 
# analysis_path <- file.path('~/OBIWAN/DERIVATIVES/BEHAV') 
# data_path <- file.path('~/Desktop/SwitchDrive/LIRA/data/HED.RData') 
# 
# setwd(analysis_path)
# 
# load(data_path)
# `%notin%` <- Negate(`%in%`)
# #remove missing fmri data
# fMRI_HED = HED %>% filter(id %in% c(205, 206, 207, 211, 215, 218, 221, 227, 229, 230, 231, 232, 244, 248, 251, 252, 253, 254, 262, 268, 202, 203, 204, 209, 213, 217, 220, 224, 225, 235, 236, 237, 238, 239, 241, 246, 250, 259, 264, 265, 266, 269, 270));
# 
# fMRI_HED = fMRI_HED %>% filter(session %in% c(0));
#   
#   #filter(id %notin% c(201, 208, 210, 214, 216, 219, 222, 223, 226, 228, 233, 234, 240, 242, 245, 247, 249, 256, 258, 263, 267))
# 
# 
# # INPUT FOR FMRI ------------------------------------------------------------------- 
# fMRI_HED = fMRI_HED[order(fMRI_HED$id, fMRI_HED$trial ),]
# 
# IDX = unique(fMRI_HED$id)
# init <- 1:length(fMRI_HED$id)
# fMRI_HED$InputFile = init
# for(i in 1:length(init)){
#   if (as.numeric(fMRI_HED$trial[i]) > 9) {
#     fMRI_HED$InputFile[i] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-05_full/sub-obese',fMRI_HED$id[i],'/output/','beta_00', fMRI_HED$trial[i],'.nii', sep ='')
#   }
#   else {fMRI_HED$InputFile[i] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-05_full/sub-obese',fMRI_HED$id[i],'/output/','beta_000', fMRI_HED$trial[i],'.nii', sep ='')}
# }
# colnames(fMRI_HED)[colnames(fMRI_HED) == 'id'] <- 'Subj'
# 
# fMRI_HED$condition = as.factor(revalue(fMRI_HED$condition, c("-1"="Neutral", "1"="Reward"))); 
# fMRI_HED$trialxcondition = scale(as.numeric(fMRI_HED$trialxcondition), scale = F)
# fMRI_HED$perceived_liking = scale(fMRI_HED$perceived_liking, scale = F)
# 
# fMRI_HED = select(fMRI_HED, c(Subj, perceived_liking, condition, trialxcondition, InputFile))
# path <-'~/OBIWAN/DERIVATIVES/GLM/AFNI/hedonicreactivity/LIRA'
# write.table(fMRI_HED, (file.path(path, "HED_LMER.txt")), row.names = F, sep="\t")


# for id in  202 203 204 209 213 217 220 224 225 235 236 237 238 239 241 246 250 259 264 265 266 269 270
# do
# mv *${id}* placebo/
#   done
# mv sub* treatment/

