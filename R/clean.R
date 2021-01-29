#----clean----
#subset only obese
tables <- c("PAV","INST","PIT","HED", "intern")
dflist <- lapply(mget(tables),function(x)subset(x, group == 'obese'))
list2env(dflist, envir=.GlobalEnv)


#exclude participants (242 really outlier everywhere, 256 can't do the task, 114 & 228 REALLY hated the solution and thus didn't "do" the conditioning) & 123 and 124 have imcomplete data
# dflist <- lapply(mget(tables),function(x)filter(x, id %notin% c(242, 256, 114, 228, 123, 124)))
# list2env(dflist, envir=.GlobalEnv)


#create idXsession
dflist <- lapply(mget(tables),function(x) idXses(x))
list2env(dflist, envir=.GlobalEnv)

#merge with info
tables = tables[-length(tables)] # remove intern
dflist <- lapply(mget(tables),function(x)merge(x, info, by = "id"))
list2env(dflist, envir=.GlobalEnv)

#merge with medic
medic = subset(medic, session =="third")
dflist <- lapply(mget(tables),function(x)merge(x, medic, by = c("id", "session"), all.x = T))
list2env(dflist, envir=.GlobalEnv)

# creates internal states variables for each data
listA = 2:5
def = function(data, number){
  baseINTERN = subset(intern, phase == number)
  data = merge(x = get(data), y = baseINTERN[ , c("thirsty", 'hungry',  'piss', 'id', 'session')], by = c("id", 'session'), all.x=TRUE)
  return(data)
}
dflist = mapply(def,tables,listA)
list2env(dflist, envir=.GlobalEnv)

#but center age by id !
df = ddply(PAV,~id,summarise,age_Z=mean(age), bmi1=mean(BMI_t1), bmi_dif=mean(BMI_t2-BMI_t1), bmi_diff=mean(BMI_t2-BMI_t1)); df$age_Z = scale(df$age_Z)
dflist <- lapply(mget(tables),function(x)merge(x, df, by = "id"))
list2env(dflist, envir=.GlobalEnv)

#center covariates
numer <- c("thirsty", "hungry",  "piss", "OEA", "PEA","X2.AG","AEA","Leptin",  "Resistin","adiponectin","MCP","TNFalpha","reelin","glucagon", "Ghrelin","obestatin","GLP1","insulin","Fast_glu","BMI_t1", "bmi_diff")
dflist <- lapply(mget(tables),function(x) x %>% group_by %>% mutate_at(numer, scale))
list2env(dflist, envir=.GlobalEnv)


#remove outliers from covariates (+- 3 SD)
df_dict <- data.frame(variable = numer, out_low = rep(-3,length(numer)),  out_high = rep(3,length(numer)))

for (var in df_dict$variable) {
  PAV[[var]][PAV[[var]] < df_dict[df_dict$variable == var, ]$out_low | PAV[[var]] > df_dict[df_dict$variable == var, ]$out_high] <- NaN
  INST[[var]][INST[[var]] < df_dict[df_dict$variable == var, ]$out_low | INST[[var]] > df_dict[df_dict$variable == var, ]$out_high] <- NaN
  HED[[var]][HED[[var]] < df_dict[df_dict$variable == var, ]$out_low | HED[[var]] > df_dict[df_dict$variable == var, ]$out_high] <- NaN
  PIT[[var]][PIT[[var]] < df_dict[df_dict$variable == var, ]$out_low | PIT[[var]] > df_dict[df_dict$variable == var, ]$out_high] <- NaN
}



# prepro RT PAV -----------------------------------------------------------

# get times in milliseconds 
PAV$RT    <- PAV$RT * 1000

#Preprocessing
PAV$condition <- droplevels(PAV$condition, exclude = "Baseline")
acc_bef = mean(PAV$ACC, na.rm = TRUE) # 0.87
full = length(PAV$RT)

##shorter than 100ms and longer than 3sd+mean
PAV.clean <- filter(PAV, RT >= 100) # min RT is 
PAV.clean <- ddply(PAV.clean, .(idXsession), transform, RTm = mean(RT))
PAV.clean <- ddply(PAV.clean, .(idXsession), transform, RTsd = sd(RT))
PAV.clean <- filter(PAV.clean, RT <= RTm+3*RTsd) 

# calculate the dropped data in the preprocessing
clean = length(PAV.clean$RT)
dropped = full-clean
(dropped*100)/full #13.26754

# clean PAV --------------------------------------------------------------

PAV = PAV.clean; PAV$ageF = PAV$age; PAV$age = PAV$age_Z
# define as.factors
fac <- c("id", "trial", "condition", "session", "intervention","trialxcondition", "gender"); PAV[fac] <- lapply(PAV[fac], factor)

#revalue all catego
PAV$session = as.factor(revalue(PAV$session, c(second="0", third="1"))) #change value of session
PAV$condition = as.factor(revalue(PAV$condition, c(CSminus="-1", CSplus="1"))); #PAV$condition <- factor(PAV$condition, levels = c("1", "-1"))#change value of condition

PAV.means <- aggregate(PAV[,c(numer, "bmi1", "bmi_dif", "ageF", "age", "liking", "RT")] , by = list(PAV$id, PAV$condition,PAV$session,PAV$intervention, PAV$gender), FUN = 'mean',na.action = na.omit)

colnames(PAV.means) <- c('id','condition','session','intervention', 'gender', numer, "bmi1", "bmi_dif", "ageF", "age", "liking", "RT")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values fot 2 participant 262 and 232)
PAV.means$thirsty[is.na(PAV.means$thirsty)] <- 0 
PAV.means$hungry[is.na(PAV.means$hungry)] <- 0 

# clean INST --------------------------------------------------------------
INST$age = INST$age_Z

# define as.factors
fac <- c("id", "trial", "session", "intervention", "gender"); INST[fac] <- lapply(INST[fac], factor)

#revalue all catego
INST$session = as.factor(revalue(INST$session, c(second="0", third="1"))) #change value of session
INST$spline = ifelse(as.numeric(INST$trial) > 5, 1, -1); INST$spline = as.factor(INST$spline) #create spline at fifth trial

INST.means <- aggregate(INST[,c(numer, "age", "grips")] , by = list(INST$id,INST$spline, INST$session,INST$intervention, INST$gender), FUN = 'mean',na.action = na.omit)

colnames(INST.means) <- c('id', 'spline','session','intervention', 'gender', numer, "age", "grips")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values for 3 participant 239, 258, 231)
INST.means$thirsty[is.na(INST.means$thirsty)] <- 0 
INST.means$hungry[is.na(INST.means$hungry)] <- 0 



# clean PIT --------------------------------------------------------------

PIT$age = PIT$age_Z

# define as.factors
fac <- c("id", "trial", "condition", "session", "intervention","trialxcondition", "gender"); PIT[fac] <- lapply(PIT[fac], factor)

#revalue all catego
PIT$session = as.factor(revalue(PIT$session, c(second="0", third="1"))) #change value of session
PIT$condition = as.factor(revalue(PIT$condition, c(CSminus="-1", CSplus="1"))); #PIT$condition <- factor(PIT$condition, levels = c("1", "-1"))#change value of condition

PIT.means <- aggregate(PIT[,c(numer, "age", "AUC")] , by = list(PIT$id, PIT$condition,PIT$session,PIT$intervention, PIT$gender), FUN = 'mean',na.action = na.omit)

colnames(PIT.means) <- c('id','condition','session','intervention', 'gender', numer, "age", "AUC")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values fot 2 participant 229 and 238)
PIT.means$thirsty[is.na(PIT.means$thirsty)] <- 0 
PIT.means$hungry[is.na(PIT.means$hungry)] <- 0 


# clean HED --------------------------------------------------------------


HED$age = HED$age_Z

#create and center fam and int covariate
HED$lik = HED$perceived_familiarity #rename
df = ddply(HED,.(id,session),summarise,fam=mean(perceived_familiarity), int=mean(perceived_intensity)); df$fam = scale(df$fam); df$int = scale(df$int)
HED = merge(HED, df, by = c("id", "session"))

# define as.factors
fac <- c("id", "trial", "condition", "session", "intervention","trialxcondition", "gender"); HED[fac] <- lapply(HED[fac], factor)

#revalue all catego
HED$session = as.factor(revalue(HED$session, c(second="0", third="1"))) #change value of session
HED$condition = as.factor(revalue(HED$condition, c(Empty="-1", MilkShake="1")));#HED$condition <- factor(HED$condition, levels = c("1", "-1"))#change value of condition

HED.means <- aggregate(HED[,c(numer, "fam", "int", "age", "lik")] , by = list(HED$id, HED$condition,HED$session,HED$intervention, HED$gender), FUN = 'mean', na.action = na.omit)

colnames(HED.means) <- c('id','condition','session','intervention', 'gender', numer, "fam", "int", "age", "lik")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values fr 1 participant 217)
HED.means$thirsty[is.na(HED.means$thirsty)] <- 0 
HED.means$hungry[is.na(HED.means$hungry)] <- 0 

# ALL ---------------------------------------------------------------------

#factorize ID
tables <- c("PAV.means","INST.means","PIT.means","HED.means")
dflist <- lapply(mget(tables),function(x)facID(x))
list2env(dflist, envir=.GlobalEnv)

save(PAV.means, file = "data/PAV.Rdata")
save(INST.means, file = "data/INST.Rdata")
save(PIT.means, file = "data/PIT.Rdata")
save(HED.means, file = "data/HED.Rdata")

#create df for weight loss
df = subset(PAV.means, session == "1"); df = subset(df, condition == "1")
df$intervention = as.factor(revalue(as.factor(df$intervention), c("0"="Placebo", "1"="Liraglutide")));#using pav.means but oculd be any other

# OLD ---------------------------------------------------------------------



# 
# # create baseline diff
# 
# First = subset(INST.means, spline == "0"); Last = subset(INST.means, spline == "1");  diff = First; diff$diff_base = First$base - Last$base
# INST.means = merge(x = INST.means, y = diff[ , c("diff_base", 'id')], by = "id", all.x=TRUE)
# 
# # gather PIT --------------------------------------------------------------------
# 
# PIT.means <- aggregate(PIT$AUC, by = list(PIT$id, PIT$condition, PIT$session), FUN='mean') # extract means
# PIT.means = spread(PIT.means, Group.3, x)
# colnames(PIT.means) <- c('id', 'condition','baseline', 'AUC')
# 
# #remove the baseline (we just use it for fMRI analysis)
# PIT.means =  subset(PIT.means, condition != 'BL') 
# 
# PIT.means = na.omit(PIT.means); # remove dropout participants
# 
# # create baseline diff
# Empty = subset(PIT.means, condition == "CSminus"); Milkshake = subset(PIT.means, condition == "CSplus"); diff = Empty;
# diff$diff_base = Milkshake$baseline - Empty$baseline
# PIT.means = merge(x = PIT.means, y = diff[ , c("diff_base", 'id')], by = "id", all.x=TRUE)
# 
# 
# # gather HED --------------------------------------------------------------
# 
# HED.means <- aggregate(list(liking=HED$perceived_liking, intensity=HED$perceived_intensity, familiarity=HED$perceived_familiarity), by = list(HED$id, HED$condition, HED$session), FUN='mean') # extract means
# HED.means = HED.means %>% gather(variable, value, (liking:familiarity)) %>%  unite(var, variable,Group.3) %>% spread(var, value)
# colnames(HED.means) <- c('id','condition', 'baseline_fam', 'familiarity', 'baseline_int', 'intensity', 'baseline_lik', 'liking')
# 
# HED.means = na.omit(HED.means); # remove dropout participants
# 
# 
# # create Intensity and Familiarity diff
# Empty = subset(HED.means, condition == "Empty"); Milkshake = subset(HED.means, condition == "MilkShake"); diff = Empty;
# diff$int = Milkshake$intensity - Empty$intensity; diff$fam = Milkshake$familiarity - Empty$familiarity;
# HED.means = merge(x = HED.means, y = diff[ , c("int", "fam", 'id')], by = "id", all.x=TRUE)
# HED.means = HED.means %>% group_by %>% mutate_at(c("int", "fam"), scale)
# 
# # create baseline diff
# diff$diff_base = Milkshake$baseline_lik - Empty$baseline_lik
# HED.means = merge(x = HED.means, y = diff[ , c("diff_base", 'id')], by = "id", all.x=TRUE)
# 
# 
# #merge with info
# tables = c('PAV.means', 'INST.means', 'PIT.means', 'HED.means')
# dflist <- lapply(mget(tables),function(x)merge(x, info, by = "id"))
# list2env(dflist, envir=.GlobalEnv)
# 
# #merge with medic
# dflist <- lapply(mget(tables),function(x)merge(x, medic, by = "id"))
# list2env(dflist, envir=.GlobalEnv)
# 
# # creates diff BMI for each data
# dflist <- lapply(mget(tables),function(x) diffX(x))
# list2env(dflist, envir=.GlobalEnv)
# 
# # creates internal states variables for each data
# listA = 2:5
# dflist = mapply(internal,tables,listA)
# list2env(dflist, envir=.GlobalEnv)
# 
# PAV.means$BMI1 = PAV.means$BMI_t1 # keep it unstandadized for later
# 
# #center covariates
# dflist <- lapply(mget(tables),function(x) x %>% group_by %>% mutate_at(c("thirsty", "hungry", "age", "diff_BMIz", "BMI_t1", "diff_base"), scale))
# list2env(dflist, envir=.GlobalEnv)
# 
# #imput mean (0 since its mean centered) for the two participant that have missing covariate (MAR) data so we can still use them in ANCOVA (this happens only for thirsty and hungry) in PAV 232 // 231 & 239 in INST // 229 in PIT // 
# tables <- c("PAV.means", "INST.means", "PIT.means")
# dflist <- lapply(mget(tables),function(x) imput(x))
# list2env(dflist, envir=.GlobalEnv)
# 
# 
# 
# # clean PAV --------------------------------------------------------------
# 
# # define as.factors
# fac <- c("id", "condition", "gender", "intervention")
# PAV.means[fac] <- lapply(PAV.means[fac], factor)
# 
# #revalue all catego
# PAV.means$condition = as.factor(revalue(PAV.means$condition, c(CSminus="-1", CSplus="1"))); PAV.means$condition <- factor(PAV.means$condition, levels = c("1", "-1"))#change value of condition
# 
# 
# # clean PIT --------------------------------------------------------------
# 
# # define as factors
# PIT.means[fac] <- lapply(PIT.means[fac], factor)
# 
# #revalue all catego
# PIT.means$condition = as.factor(revalue(PIT.means$condition, c(CSminus="-1", CSplus="1"))); PIT.means$condition <- factor(PIT.means$condition, levels = c("1", "-1"))#change value of condition
# 
# 
# # clean HED ---------------------------------------------------------------
# 
# # define as.factors
# HED.means[fac] <- lapply(HED.means[fac], factor)
# 
# #revalue all catego
# HED.means$condition = as.factor(revalue(HED.means$condition, c(MilkShake="1", Empty="-1"))) #change value of condition
# HED.means$condition <- relevel(HED.means$condition, "1") # Make MilkShake first
# 
# 
# # clean INST -------------------------------------------------------------
# 
# #defne factors
# fac <- c("id", "gender", "intervention")
# INST.means[fac] <- lapply(INST.means[fac], factor)
# #revalue all catego




