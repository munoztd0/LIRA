if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)}

pacman::p_load(tidyverse, dplyr, plyr, car, misty)

analysis_path <- file.path('~/OBIWAN/DERIVATIVES/BEHAV') 
data_path <- file.path('~/Desktop/SwitchDrive/LIRA/data/HED.Rdata') 

setwd(analysis_path)

load(data_path)
`%notin%` <- Negate(`%in%`)
#remove missing fmri data
fMRI_HED = HED.means %>% filter(id %notin% c(201, 208, 210, 214, 216, 219, 222, 223, 226, 228, 233, 234, 240, 242, 245, 247, 249, 256, 258, 263, 267))
fMRI_HED$lik = scale(fMRI_HED$lik)
fMRI_HED$interXLik = ifelse(fMRI_HED$intervention == 0, -fMRI_HED$lik, fMRI_HED$lik)

# INPUT FOR FMRI ------------------------------------------------------------------- 

IDX = unique(fMRI_HED$id)
init <- 1:length(fMRI_HED$id)
fMRI_HED$InputFile = init
for(i in 1:length(IDX)){
  fMRI_HED$InputFile[fMRI_HED$condition == '1' & fMRI_HED$session == 0 & fMRI_HED$id == IDX[i]] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-01_0/group/sub-obese',IDX[i],'_con_0003.nii', sep ='')
  fMRI_HED$InputFile[fMRI_HED$condition == '-1' & fMRI_HED$session == 0 & fMRI_HED$id == IDX[i]] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-01_0/group/sub-obese',IDX[i],'_con_0004.nii', sep ='')
  fMRI_HED$InputFile[fMRI_HED$condition == '1' & fMRI_HED$session == 1 & fMRI_HED$id == IDX[i]] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-01_1/group/sub-obese',IDX[i],'_con_0003.nii', sep ='')
  fMRI_HED$InputFile[fMRI_HED$condition == '-1' & fMRI_HED$session == 1 & fMRI_HED$id == IDX[i]] <- paste('/home/davidM/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/GLM-01_1/group/sub-obese',IDX[i],'_con_0004.nii', sep ='')
}
colnames(fMRI_HED)[colnames(fMRI_HED) == 'id'] <- 'Subj'

fMRI_HED$condition = as.factor(revalue(fMRI_HED$condition, c("-1"="Neutral", "1"="Reward"))); 

path <-'~/OBIWAN/DERIVATIVES/GLM/AFNI/hedonicreactivity/'
write.table(fMRI_HED, (file.path(path, "HED_LME_cov.txt")), row.names = F, sep="\t")


# for id in  202 203 204 209 213 217 220 224 225 235 236 237 238 239 241 246 250 259 264 265 266 269 270
# do
# mv *${id}* placebo/
#   done
# mv sub* treatment/
