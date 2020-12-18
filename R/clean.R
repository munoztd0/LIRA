#----clean----
#subset only pretest
tables <- c("PAV","INST","PIT","HED", "intern")
dflist <- lapply(mget(tables),function(x)subset(x, group == 'obese'))
list2env(dflist, envir=.GlobalEnv)
intern = subset(intern, session == 'third') #only last session


#exclude participants (242 really outlier everywhere, 256 can't do the task, 114 & 228 REALLY hated the solution and thus didn't "do" the conditioning) & 123 and 124 have imcomplete data
# dflist <- lapply(mget(tables),function(x)filter(x, id %notin% c(242, 256, 114, 228, 123, 124)))
# list2env(dflist, envir=.GlobalEnv)


# prepro RT PAV -----------------------------------------------------------

# get times in milliseconds 
PAV$RT    <- PAV$RT * 1000

#Preprocessing
PAV$condition <- droplevels(PAV$condition, exclude = "Baseline")
acc_bef = mean(PAV$ACC, na.rm = TRUE) #0.93
full = length(PAV$RT)

##shorter than 100ms and longer than 3sd+mean
PAV.clean <- filter(PAV, RT >= 100) # min RT is 
PAV.clean <- ddply(PAV.clean, .(id), transform, RTm = mean(RT))
PAV.clean <- ddply(PAV.clean, .(id), transform, RTsd = sd(RT))
PAV.clean <- filter(PAV.clean, RT <= RTm+3*RTsd) 

# calculate the dropped data in the preprocessing
clean = length(PAV.clean$RT)
dropped = full-clean
(dropped*100)/full


# gather PAV --------------------------------------------------------------------

PAV.means <- aggregate(list(RT=PAV.clean$RT, liking=PAV.clean$liking), by = list(PAV.clean$id, PAV.clean$condition, PAV.clean$session), FUN='mean') # extract means
PAV.means = PAV.means %>% gather(variable, value, (RT:liking)) %>%  unite(var, variable,Group.3) %>% spread(var, value)
colnames(PAV.means) <- c('id','condition', 'baseline_lik', 'liking', 'baseline_RT', 'RT')
PAV.means = filter(PAV.means, id %notin% c(230)) # remove 230 because it doesnt have CS minus condition
PAV.means = na.omit(PAV.means); # remove dropout participants

# create baseline diff
Empty = subset(PAV.means, condition == "CSminus"); Milkshake = subset(PAV.means, condition == "CSplus"); diff = Empty;
diff$diff_base_lik = Milkshake$baseline_lik - Empty$baseline_lik; diff$diff_base = Empty$baseline_RT - Milkshake$baseline_RT; # reverse for RT because we are looking at latency
PAV.means = merge(x = PAV.means, y = diff[ , c("diff_base_lik", "diff_base", 'id')], by = "id", all.x=TRUE)

PAV.means = PAV.means %>% group_by %>% mutate_at(c('diff_base_lik'), scale)

# gather INST -------------------------------------------------------------
INST$spline = as.factor(ifelse(INST$trial > 5, 0, 1)) # create spline factor (learning phase <5, then >5)
INST.means <- aggregate(INST$grips, by = list(INST$id, INST$spline, INST$session), FUN='mean') # extract means
INST.means = spread(INST.means, Group.3, x)
colnames(INST.means) <- c('id','spline','baseline', 'grips')

INST2 <- aggregate(INST$grips, by = list(INST$id, INST$trial, INST$session), FUN='mean') # extract means
INST2 = spread(INST2, Group.3, x)
colnames(INST2) <- c('id','trial','baseline', 'grips')
# tmp = lspline(INST.means$trial, 5); INST.means$ls1 = tmp[,1] ; INST.means$ls2 = tmp[,2]
INST.means = na.omit(INST.means); # remove dropout participants

# create baseline diff

First = subset(INST.means, spline == "0"); Last = subset(INST.means, spline == "1");  diff = First; diff$diff_base = First$base - Last$base
INST.means = merge(x = INST.means, y = diff[ , c("diff_base", 'id')], by = "id", all.x=TRUE)

# gather PIT --------------------------------------------------------------------

PIT.means <- aggregate(PIT$AUC, by = list(PIT$id, PIT$condition, PIT$session), FUN='mean') # extract means
PIT.means = spread(PIT.means, Group.3, x)
colnames(PIT.means) <- c('id', 'condition','baseline', 'AUC')

#remove the baseline (we just use it for fMRI analysis)
PIT.means =  subset(PIT.means, condition != 'BL') 

PIT.means = na.omit(PIT.means); # remove dropout participants

# create baseline diff
Empty = subset(PIT.means, condition == "CSminus"); Milkshake = subset(PIT.means, condition == "CSplus"); diff = Empty;
diff$diff_base = Milkshake$baseline - Empty$baseline
PIT.means = merge(x = PIT.means, y = diff[ , c("diff_base", 'id')], by = "id", all.x=TRUE)


# gather HED --------------------------------------------------------------

HED.means <- aggregate(list(liking=HED$perceived_liking, intensity=HED$perceived_intensity, familiarity=HED$perceived_familiarity), by = list(HED$id, HED$condition, HED$session), FUN='mean') # extract means
HED.means = HED.means %>% gather(variable, value, (liking:familiarity)) %>%  unite(var, variable,Group.3) %>% spread(var, value)
colnames(HED.means) <- c('id','condition', 'baseline_fam', 'familiarity', 'baseline_int', 'intensity', 'baseline_lik', 'liking')

HED.means = na.omit(HED.means); # remove dropout participants


# create Intensity and Familiarity diff
Empty = subset(HED.means, condition == "Empty"); Milkshake = subset(HED.means, condition == "MilkShake"); diff = Empty;
diff$int = Milkshake$intensity - Empty$intensity; diff$fam = Milkshake$familiarity - Empty$familiarity;
HED.means = merge(x = HED.means, y = diff[ , c("int", "fam", 'id')], by = "id", all.x=TRUE)

# create baseline diff
diff$diff_base = Milkshake$baseline_lik - Empty$baseline_lik
HED.means = merge(x = HED.means, y = diff[ , c("diff_base", 'id')], by = "id", all.x=TRUE)


#merge with info
tables = c('PAV.means', 'INST.means', 'PIT.means', 'HED.means')
dflist <- lapply(mget(tables),function(x)merge(x, info, by = "id"))
list2env(dflist, envir=.GlobalEnv)

# creates diff BMI for each data
dflist <- lapply(mget(tables),function(x) diffX(x))
list2env(dflist, envir=.GlobalEnv)

# creates internal states variables for each data
listA = 2:5
dflist = mapply(internal,tables,listA)
list2env(dflist, envir=.GlobalEnv)

PAV.means$BMI1 = PAV.means$BMI_t1 # keep it unstandadized for later

#center covariates
dflist <- lapply(mget(tables),function(x) x %>% group_by %>% mutate_at(c("thirsty", "hungry", "age", "diff_BMIz", "BMI_t1", "diff_base"), scale))
list2env(dflist, envir=.GlobalEnv)

#imput mean (0 since its mean centered) for the two participant that have missing covariate (MAR) data so we can still use them in ANCOVA (this happens only for thirsty and hungry) in PAV 232 // 231 & 239 in INST
tables <- c("PAV.means", "INST.means")
dflist <- lapply(mget(tables),function(x) imput(x))
list2env(dflist, envir=.GlobalEnv)



# clean PAV --------------------------------------------------------------

# define as.factors
fac <- c("id", "condition", "gender", "intervention")
PAV.means[fac] <- lapply(PAV.means[fac], factor)

#revalue all catego
PAV.means$condition = as.factor(revalue(PAV.means$condition, c(CSminus="-1", CSplus="1"))); PAV.means$condition <- factor(PAV.means$condition, levels = c("1", "-1"))#change value of condition


# clean PIT --------------------------------------------------------------

# define as factors
PIT.means[fac] <- lapply(PIT.means[fac], factor)

#revalue all catego
PIT.means$condition = as.factor(revalue(PIT.means$condition, c(CSminus="-1", CSplus="1"))); PIT.means$condition <- factor(PIT.means$condition, levels = c("1", "-1"))#change value of condition


# clean HED ---------------------------------------------------------------

# define as.factors
HED.means[fac] <- lapply(HED.means[fac], factor)

#revalue all catego
HED.means$condition = as.factor(revalue(HED.means$condition, c(MilkShake="1", Empty="-1"))) #change value of condition
HED.means$condition <- relevel(HED.means$condition, "1") # Make MilkShake first


# clean INST -------------------------------------------------------------

#defne factors
fac <- c("id", "gender", "intervention")
INST.means[fac] <- lapply(INST.means[fac], factor)
#revalue all catego




