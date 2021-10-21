#----clean----

#subset only obese
tables <- c("PAV","INST","PIT","HED", "intern")
dflist <- lapply(mget(tables),function(x)subset(x, group == 'obese'))
list2env(dflist, envir=.GlobalEnv)

#exclude participants (242 really outlier everywhere, 256 can't do the task, 228 REALLY hated the solution and thus didn't "do" the conditioning)
# dflist <- lapply(mget(tables),function(x)filter(x, id %notin% c(242, 256, 228))
# list2env(dflist, envir=.GlobalEnv)

#center covariates paste0(names(PAV), collapse="','")
medic$ageF = medic$age; medic$weightLoss = -(medic$BMI_diff); medic$bmi1 = medic$BMI_V1; medic$dif_interv = medic$Date_diff #keep uncentered for descriptive stats + reverse BMI_diff so it in terms of actual weight loss and not weight gain
biomed <- c('age','Date_diff','BW_diff','BMI_diff','WC_diff','Insulin_diff','X2.AG_diff','reelin_diff','MCP_diff','TNFalpha_diff','GLP_diff','Ghrelin_diff','Glu_diff','HOMA_IR_diff', 'AEA_diff', 'OEA_diff', 'PEA_diff', 'BMI_V1')
medic = medic %>% group_by %>% mutate_at(biomed, scale)

#remove outliers from biomedical (+- 3 SD)
df_dict <- data.frame(variable = biomed, out_low = rep(-3,length(biomed)),  out_high = rep(3,length(biomed)))
for (var in df_dict$variable) {
  medic[[var]][medic[[var]] < df_dict[df_dict$variable == var, ]$out_low | medic[[var]] > df_dict[df_dict$variable == var, ]$out_high] <- NaN}

#merge with medic
tables = tables[-length(tables)]; # remove intern
dflist <- lapply(mget(tables),function(x)merge(x, medic, by = "id"))
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

#center covariates
numer <- c('thirsty','hungry')
dflist <- lapply(mget(tables),function(x) x %>% group_by %>% mutate_at(numer, scale))
list2env(dflist, envir=.GlobalEnv)

covariate = c(biomed, numer)


# prepro RT PAV -----------------------------------------------------------

# get times in milliseconds 
PAV$RT    <- PAV$RT * 1000

#Preprocessing
PAV$condition <- droplevels(PAV$condition, exclude = "Baseline")
acc_bef = mean(PAV$ACC, na.rm = TRUE) # 0.87
full = length(PAV$RT)

##shorter than 100ms and longer than 3sd+mean
PAV.clean <- filter(PAV, RT >= 100) # min RT is 
PAV.clean <- ddply(PAV.clean, .(id, session), transform, RTm = mean(RT), RTsd = sd(RT))
PAV.clean <- filter(PAV.clean, RT <= RTm+3*RTsd) 

# calculate the dropped data in the preprocessing
clean = length(PAV.clean$RT)
dropped = full-clean
(dropped*100)/full #13.26754

# clean PAV --------------------------------------------------------------

PAV = PAV.clean
# define as.factors
fac <- c("id", "trial", "condition", "session", "intervention","trialxcondition", "gender"); PAV[fac] <- lapply(PAV[fac], factor)

#revalue all catego
PAV$session = PAV$session = ifelse(PAV$session == "second", -1, 1)
  #as.factor(revalue(PAV$session, c(second="-1", third="1"))) #change value of session
PAV$condition = ifelse(PAV$condition == "CSminus", -1, 1) 
PAV$gender = ifelse(PAV$gender == "0", -1, 1) 
PAV$intervention = ifelse(PAV$intervention == "0", -1, 1)  #change value of intervention
#PAV$condition = as.factor(revalue(PAV$condition, c(CSminus="-1", CSplus="1")));


PAV.means <- aggregate(PAV[,c(covariate, "weightLoss", "ageF",  "bmi1", "liking", "RT")] , by = list(PAV$id, PAV$condition,PAV$session,PAV$intervention, PAV$gender), FUN = 'mean',na.action = na.omit)

colnames(PAV.means) <- c('id','condition','session','intervention', 'gender', covariate,"weightLoss", "ageF", "bmi1", "liking", "RT")


#imput mean (0) for the two covariate (MAR) so we can get BF (missing values fot 2 participant 262 and 232)
#PAV.means$thirsty[is.na(PAV.means$thirsty)] <- 0 ; PAV.means$hungry[is.na(PAV.means$hungry)] <- 0 

# clean INST --------------------------------------------------------------
INST$Trial = as.numeric(INST$trial)
x = lspline(INST$Trial, 5); INST$Trial1 = x[,1]; INST$Trial2 = x[,2]; 

# define as.factors
fac <- c("id", "session", "intervention", "gender"); INST[fac] <- lapply(INST[fac], factor)

#revalue all catego
INST$gender = INST$gender = ifelse(INST$gender == "0", -1, 1)
INST$session = INST$session = ifelse(INST$session == "second", -1, 1)
#as.factor(revalue(PAV$session, c(second="-1", third="1"))) #change value of session
INST$intervention = ifelse(INST$intervention == "0", -1, 1)  #change value of intervention
INST$trialZ = scale(INST$trial)
# INST$session = as.factor(revalue(INST$session, c(second="0", third="1"))) #change value of session

# get the averaged dataset
INST.means <- aggregate(INST[,c(covariate, "grips")] , by = list(INST$id,INST$trial, INST$session,INST$intervention, INST$gender), FUN = 'mean',na.action = na.omit)

colnames(INST.means) <- c('id','trial','session','intervention', 'gender', covariate, "grips")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values for 3 participant 239, 258, 231)
#INST.means$thirsty[is.na(INST.means$thirsty)] <- 0 ; INST.means$hungry[is.na(INST.means$hungry)] <- 0 

INST.means$Trial = as.numeric(INST.means$trial)
x = lspline(INST.means$Trial, 5); INST.means$Trial1 = x[,1]; INST.means$Trial2 = x[,2]; 

dfTrial = ddply(INST,.(trial,session),summarise,grips=mean(grips)); dfTrial$Trial = scale(as.numeric(dfTrial$trial))

dfTrial$phasis = ifelse(dfTrial$Trial >	-1.07212710 , "1", "0")
dfTrial$T2 = ifelse(dfTrial$Trial > 0, dfTrial$Trial^2, -dfTrial$Trial^2)

# clean PIT --------------------------------------------------------------

# define as.factors
fac <- c("id", "trial", "condition", "session", "intervention","trialxcondition", "gender"); PIT[fac] <- lapply(PIT[fac], factor)

PIT.base =  subset(PIT, condition == 'BL'); PIT.csp =  subset(PIT, condition == 'CSplus'); PIT.csm =  subset(PIT, condition == 'CSminus')
PIT.csp = PIT.csp %>% arrange(desc(id)); PIT.csm =PIT.csm %>% arrange(desc(id)); PIT.base =PIT.base %>% arrange(desc(id)) #order by id
#PIT.csp$AUC = PIT.csp$AUC - PIT.base$AUC; PIT.csm$AUC = PIT.csm$AUC - PIT.base$AUC
PIT.clean = rbind(PIT.csp, PIT.csm) #bind together

#revalue all catego
PIT.clean$condition = ifelse(PIT.clean$condition == "CSminus", -1, 1) 
PIT.clean$gender = PIT.clean$gender = ifelse(PIT.clean$gender == "0", -1, 1)
PIT.clean$session = PIT.clean$session = ifelse(PIT.clean$session == "second", -1, 1)
PIT.clean$intervention = ifelse(PIT.clean$intervention == "0", -1, 1)

PIT.means <- aggregate(PIT.clean[,c(covariate, "AUC")] , by = list(PIT.clean$id, PIT.clean$condition,PIT.clean$session,PIT.clean$intervention, PIT.clean$gender), FUN = 'mean',na.action = na.omit)

colnames(PIT.means) <- c('id','condition','session','intervention', 'gender', covariate, "AUC")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values fot 2 participant 229 and 238)
#PIT.means$thirsty[is.na(PIT.means$thirsty)] <- 0 ; PIT.means$hungry[is.na(PIT.means$hungry)] <- 0 


PIT.p <- summarySEwithin(PIT.clean,
                         measurevar = "AUC",
                         withinvars = c("trialxcondition","condition", "session"),
                         idvar = "id")

PIT.p$trial <- as.numeric(PIT.p$trialxcondition)
PIT.p = select(PIT.p, c('trial', 'N' , 'AUC', 'sd', 'se', 'ci', 'condition',"session"))
PIT.p$condition <- relevel(PIT.p$condition, "1") # Make MilkShake first


PIT.group <- summarySEwithin(PIT.clean,
                         measurevar = "AUC",
                         withinvars = c("trialxcondition","condition", "session"),
                         betweenvars = "intervention",
                         idvar = "id")

PIT.group$trial <- as.numeric(PIT.group$trialxcondition)
PIT.group = select(PIT.group, c('trial', 'N' , 'AUC', 'sd', 'se', 'ci', 'condition', 'intervention',"session"))
PIT.group$condition <- relevel(PIT.group$condition, "1") # Make MilkShake first



# clean HED --------------------------------------------------------------


# define as.factors
fac <- c("id", "trial", "condition", "session", "intervention","trialxcondition", "gender"); HED[fac] <- lapply(HED[fac], factor)

# recode as contr.sum dummy coding
HED$intervention = ifelse(HED$intervention == "0", 1,-1) #change value of group
HED$gender = ifelse(HED$gender == "0", -1, 1) #change value of gender
HED$condition = ifelse(HED$condition == "MilkShake", 1,-1); #change value of condition
HED$session = ifelse(HED$session == "second", 1,-1); #change value of condition


# create Intensity and Familiarity diff
bs = ddply(HED, .(id, condition), summarise, int = mean(perceived_intensity, na.rm = TRUE), fam = mean(perceived_familiarity, na.rm = TRUE)) 
Empty = subset(bs, condition == "-1"); Milkshake = subset(bs, condition == "1"); diff = Empty;
diff$int = Milkshake$int - Empty$int; diff$fam = Milkshake$fam - Empty$fam;
HED = merge(x = HED, y = diff[ , c("int", "fam", 'id')], by = "id", all.x=TRUE)

#center covariates
numer <- c("fam", "int")
HED = HED %>% group_by %>% mutate_at(numer, scale)
HED$intensity = HED$int; HED$familiarity = HED$fam


HED.means <- aggregate(HED[,c(covariate, "fam", "int", "perceived_liking")] , by = list(HED$id, HED$condition,HED$session,HED$intervention, HED$gender), FUN = 'mean', na.action = na.omit)

colnames(HED.means) <- c('id','condition','session','intervention', 'gender', covariate, "fam", "int", "perceived_liking")

#imput mean (0) for the two covariate (MAR) so we can get BF (missing values fr 1 participant 217)
#HED.means$thirsty[is.na(HED.means$thirsty)] <- 0 ; HED.means$hungry[is.na(HED.means$hungry)] <- 0 


HED.p <- summarySEwithin(HED,
                         measurevar = "perceived_liking",
                         withinvars = c("trialxcondition","condition", "session"),
                         idvar = "id")

HED.p$trial <- as.numeric(HED.p$trialxcondition)
HED.p = select(HED.p, c('trial', 'N' , 'perceived_liking', 'sd', 'se', 'ci', 'condition',"session"))
HED.p$condition <- relevel(HED.p$condition, "1") # Make MilkShake first


HED.group <- summarySEwithin(HED,
                             measurevar = "perceived_liking",
                             withinvars = c("trialxcondition","condition", "session"),
                             betweenvars = "intervention",
                             idvar = "id")

HED.group$trial <- as.numeric(HED.group$trialxcondition)
HED.group = select(HED.group, c('trial', 'N' , 'perceived_liking', 'sd', 'se', 'ci', 'condition', 'intervention',"session"))
HED.group$condition <- relevel(HED.group$condition, "1") # Make MilkShake first


# ALL ---------------------------------------------------------------------

#factorize ID
tables <- c("PAV.means","PIT.means","HED.means")
dflist <- lapply(mget(tables),function(x)facID(x))
list2env(dflist, envir=.GlobalEnv)

save(PAV.means, file = "data/PAV.Rdata")
save(INST.means, file = "data/INST.Rdata")
save(PIT.means, file = "data/PIT.Rdata")
save(HED.means, file = "data/HED.Rdata")

#create df for AFNI
dfHED = HED.means
dfHED[is.na(dfHED)] <- 0 
save(dfHED, file = "data/HED_fmri.Rdata")

# internHED = subset(intern, phase ==5)
# dfx = x = Reduce(function(x,y) merge(x = x, y = y, by = "id"), 
#                  list(df, internHED, info)); dfx[is.na(dfx)] <- 0 
# x = Reduce(function(x,y) merge(x = x, y = y, by = c("id","session")), 
#            list(dfl, dfm, dfx))
# 
# tables = c('x') 
# numer <- c("thirsty", "hungry",  "piss", "OEA", "PEA","X2.AG","AEA","Leptin",  "Resistin","adiponectin","MCP","TNFalpha","reelin","glucagon", "Ghrelin","obestatin","GLP1","insulin","Fast_glu","BMI_t1", "bmi_diff")
# dflist <- lapply(mget(tables),function(x) x %>% group_by %>% mutate_at(numer, scale))
# list2env(dflist, envir=.GlobalEnv); x$age = x$age_Z
# 
# dfHED = select(x, -c(age_Z, bmi1, bmi_dif, task, phase, idXsession, BMI_t2, group))
# dfHED$session = as.factor(revalue(as.factor(dfHED$session), c("second"="0", "third"="1"))); dfHED1 = dfHED;  dfHED2 = dfHED; dfHED1$condition = "1"; dfHED2$condition = "-1"; dfHED = rbind(dfHED1, dfHED2); dfHED[is.na(dfHED)] <- 0 
# 
# save(dfHED, file = "data/HED_fmri.Rdata")
# 

# create df for weight loss
df = subset(PAV.means, session == "1"); df = subset(df, condition == "1")
df$AGE = df$ageF; df$BMI = df$bmi1 ; df$GENDER = df$gender; df$INTERVENTION = df$intervention
#df$intervention = ifelse(df$intervention == -1, -1, 1) #change value of intervention
#df$gender = ifelse(df$gender == 0, -1, 1) #change value of gender

df$INTERVENTION = as.factor(revalue(as.factor(df$INTERVENTION), c("-1"="Placebo", "1"="Liraglutide")))#using pav.means but oculd be any other
df$GENDER = as.factor(revalue(as.factor(df$GENDER), c("-1"="Men", "1"="Women")))

med <- gather(df, "feature", "n", 8:22)


#biomed = numer[3:14]; 
dfmed = na.omit(medic[,c('intervention',biomed[3:14])]) #create df for var selec


#inter score for fmri Plots
diffPRE = subset(HED_fMRI, session == 'pre') ; diffPOST = subset(HED_fMRI, session == 'post')
inter = diffPRE; inter$OFC_inter =  diffPOST$OFC_score - diffPRE$OFC_score; inter$HF_inter =  diffPOST$HF_score - diffPRE$HF_score

inter$id = as.factor(inter$id); inter$intervention = as.factor(inter$intervention)

inter = filter(inter, id %notin% c(246)) #remove huge outlier because it biases the whole further results on mediation and weigthloss -> you can check it ou via:
# ggplot(inter, aes(x= BMI_diff, y=intervention, label=id))+
#   geom_point() +geom_text(aes(label=id),hjust=1, vjust=0)
weightloss = inter$BMI_diff; #reverse in terms of wight loss not weigth gain

ind <- sapply(inter, is.numeric)
inter[ind] <- lapply(inter[ind], scale)
inter$Intervention = as.numeric(inter$intervention); inter$HF  = as.vector(inter$HF_inter); inter$`Weight Loss` = -as.vector(weightloss) #reverse to have it in positive terms
