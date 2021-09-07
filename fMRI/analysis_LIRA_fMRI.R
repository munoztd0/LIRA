
if(!require(pacman)) {
  install.packages("pacman")
  install.packages("devtools")
  library(pacman)
}

pacman::p_load(tidyverse, dplyr, plyr, Rmisc, sjPlot, afex, cowplot, ggpubr, psych, mediation)

# get tool
devtools::source_gist("2a1bb0133ff568cbe28d", 
                      filename = "geom_flat_violin.R")
# Set path
home_path       <- '~/OBIWAN'

# Set working directory
analysis_path <- file.path(home_path, 'CODE/ANALYSIS/BEHAV/ForPaper')
figures_path  <- file.path(home_path, 'DERIVATIVES/FIGURES/BEHAV/') 
setwd(analysis_path)

subj = c(202, 203, 204, 209, 213, 217, 220, 224, 225, 235, 236, 237, 238, 239, 241, 246, 250, 259, 264, 265, 266, 269, 270, 205, 206, 207, 211, 215, 218, 221, 227, 229, 230, 231, 232, 244, 248, 251, 252, 253, 254, 262, 268)

#datasets dictory
data_path <- file.path(home_path,'DERIVATIVES/BEHAV') 

#cov  <- read.delim(file.path(data_path,'covariate_LIRA_old.txt'), header = T, sep ='') #
cov  <- read.delim(file.path(data_path,'medic.csv'), header = T, sep =',') #

# PLOT --------------------------------------------------------------------

averaged_theme <- theme_bw(base_size =32, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 32, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        legend.position=c(.9,.9),
        plot.title  = element_text(size = 32, hjust = 0.5),
        legend.title  = element_text(size = 12),
        legend.text  = element_text(size = 10),
        legend.key.size = unit(1, "cm"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_blank(), #element_line(size=.2, color="lightgrey") ,
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 32),
        axis.title.y = element_text(size =  32),
        axis.line = element_line(size = 0.5),
        panel.border = element_blank())

pal = viridis::inferno(n=5) # specialy conceived for colorblindness
pal[6] = "#21908CFF" # add one

# hpp ---------------------------------------------------------------------


PREplaceboREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_PHC_0_placebo_betas.csv", header=T); PREplaceboREW$session = '0'
PREplaceboNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_PHC_0_placebo_betas.csv", header=T); PREplaceboNEU$session = '0'
POSTplaceboREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_PHC_1_placebo_betas.csv", header=T); POSTplaceboREW$session = '1'
POSTplaceboNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_PHC_1_placebo_betas.csv", header=T); POSTplaceboNEU$session = '1'

PREtreatmentREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_PHC_0_treatment_betas.csv", header=T); PREtreatmentREW$session = '0'
PREtreatmentNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_PHC_0_treatment_betas.csv", header=T); PREtreatmentNEU$session = '0'
POSTtreatmentREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_PHC_1_treatment_betas.csv", header=T); POSTtreatmentREW$session = '1'
POSTtreatmentNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_PHC_1_treatment_betas.csv", header=T); POSTtreatmentNEU$session = '1'

PRE_REW = rbind(PREplaceboREW, PREtreatmentREW)
POST_REW = rbind(POSTplaceboREW, POSTtreatmentREW)

PRE_NEU = rbind(PREplaceboNEU, PREtreatmentNEU)
POST_NEU = rbind(POSTplaceboNEU, POSTtreatmentNEU)


diffPRE = PRE_REW; diffPRE$HF_score =  PRE_REW$betas - PRE_NEU$betas; diffPRE$id =subj;  diffPRE$session = 'pre'
diffPOST = POST_REW; diffPOST$HF_score =  POST_REW$betas - POST_NEU$betas; diffPOST$id =subj; diffPOST$session = 'post'

diff1 = rbind(diffPRE, diffPOST)








# mOFC --------------------------------------------------------------------


PREplaceboREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_OFC_0_placebo_betas.csv", header=T); PREplaceboREW$session = '0'
PREplaceboNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_OFC_0_placebo_betas.csv", header=T); PREplaceboNEU$session = '0'
POSTplaceboREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_OFC_1_placebo_betas.csv", header=T); POSTplaceboREW$session = '1'
POSTplaceboNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_OFC_1_placebo_betas.csv", header=T); POSTplaceboNEU$session = '1'

PREtreatmentREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_OFC_0_treatment_betas.csv", header=T); PREtreatmentREW$session = '0'
PREtreatmentNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_OFC_0_treatment_betas.csv", header=T); PREtreatmentNEU$session = '0'
POSTtreatmentREW <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/reward/mask_OFC_1_treatment_betas.csv", header=T); POSTtreatmentREW$session = '1'
POSTtreatmentNEU <- read.delim("~/OBIWAN/DERIVATIVES/GLM/SPM/hedonicreactivity/ROI/neutral/mask_OFC_1_treatment_betas.csv", header=T); POSTtreatmentNEU$session = '1'

PRE_REW = rbind(PREplaceboREW, PREtreatmentREW)
POST_REW = rbind(POSTplaceboREW, POSTtreatmentREW)

PRE_NEU = rbind(PREplaceboNEU, PREtreatmentNEU)
POST_NEU = rbind(POSTplaceboNEU, POSTtreatmentNEU)


diffPRE = PRE_REW; diffPRE$OFC_score =  PRE_REW$betas - PRE_NEU$betas; diffPRE$id =subj;  diffPRE$session = 'pre'
diffPOST = POST_REW; diffPOST$OFC_score =  POST_REW$betas - POST_NEU$betas; diffPOST$id =subj; diffPOST$session = 'post'

diff2 = rbind(diffPRE, diffPOST)


diff1$OFC_score = diff2$OFC_score

df = merge(diff1, cov , by= 'id'); df$intervention = as.factor(df$intervention)
df = df[-c(2)]

write.csv(df, "/home/davidM/Desktop/SwitchDrive/LIRA/data/HED_fmri.csv", row.names =F)



#interscore for later
diffPRE = subset(diff, session == 'pre') ; diffPOST = subset(diff, session == 'post')
diffSES = diffPRE; diffSES$inter =  diffPOST$score - diffPRE$score

dft = merge(diffSES, cov , by= 'id'); 
`%notin%` <- Negate(`%in%`); dftout1 = filter(dft, id %notin% c(246)) #filter outlier
dftout1$intervention    = as.factor(dftout1$intervention)
weightloss = dftout1$BMI_diff; dftout1$id = as.factor(dftout1$id)
ind <- sapply(dftout1, is.numeric)
dftout1[ind] <- lapply(dftout1[ind], scale)
dftout1$weightloss = weightloss


# good one ----------------------------------------------------------------


sp = ggplot(dftout1, aes(x = weightloss, y = inter, color = intervention)) +
  stat_ellipse(aes(group=intervention)) +  geom_point(alpha = .3, size = 3) + 
  geom_smooth(method= lm,aes(color = NULL), alpha = 0.5 ) +
  scale_color_manual( labels = c("Placebo", "Liraglutide"), values=c("0"=pal[1], "1"=pal[6])) + theme_bw()+ 
  stat_cor(aes(color=NULL, label = paste(..rr.label.., sep = "~`,`~")),label.x = -6, label.y = -5)+ labs(color = "Intervention")   + 
  guides(color = guide_legend(override.aes = list(fill=NA, size = c(3,3), linetype = c(0, 0),label = "") ) ) +   ylab('Beta estimates (a.u.)') +   xlab('Weight loss (\u0394 BMI)') +   theme(axis.title.x = element_text(size = 24), axis.title.y = element_text(size =  24), legend.position = c(0.9, 0.8)) + 
  border()   

xplot <- ggdensity(dftout1, "weightloss", fill = "intervention") +   scale_fill_manual(values=c("0"= pal[1], "1"=pal[6])) 
yplot <- ggdensity(dftout1, "inter", fill = "intervention")  + scale_fill_manual(values=c("0"= pal[1], "1"=pal[6])) + rotate()

# Cleaning the plots
sp <- sp 
yplot <- yplot + clean_theme() + rremove("legend") 
xplot <- xplot + clean_theme() + rremove("legend")

fig = plot_grid(xplot, NULL, sp, yplot,ncol = 2, align = "hv", rel_widths = c(2, 1), rel_heights = c(1, 2))
fig
#calculate exclusion BF01 for each effect
test = extractBF(generalTestBF( inter ~ + intervention*BMI_diff + age, data= dftout1, whichRandom = 'id', neverExclude =  'id', whichModels ="top")); BF = 1/test[1]; BF #switch to BF10 inclusion)); BF = 1/test[1] #switch to BF10 inclusion

aov_car(data = dftout1, inter ~ intervention*BMI_diff + age + Error(id), factorize = F)



# mediation ---------------------------------------------------------------
dftout1$Intervention = as.numeric(dftout1$intervention); dftout1$HPP  = as.vector(dftout1$inter); dftout1$`Weight Loss` = -as.vector(dftout1$BMI_diff) #reverse to have it in positive terms

#step 1
fit.total = lm(`Weight Loss` ~ intervention+age, data =dftout1)
#step 2
fit.mediator = lm(HPP ~intervention+age, data=dftout1)
#step3
fit.dv = lm(`Weight Loss` ~intervention+HPP+age, data=dftout1)
#step4
set.seed(123) #set random seed
results = mediation::mediate(fit.mediator, fit.dv, treat= 'intervention', mediator='HPP')
summary(results)

#with Lavaan
# model = "\n#Regressions
#           HPP ~ a*intervention
#           `Weight Loss` ~ b*HPP +  c*intervention + age 
#           
#           #Defined Parameters:
#           ie := a*b
#           de := c\n"
# 
# fit=lavaan::sem(model,dftout1)
# summary(fit)

#just to plot
medi =  psych::mediate(`Weight Loss` ~ Intervention + (HPP) -age, data = dftout1, plot=F) 
psych::mediate.diagram(medi, show.c = F, main= "")
text(3, 6.5, "A ***",  cex = .8); text(7, 6.5, "B *",  cex = .8);  text(5.8, 4.9, "***",  cex = .8); text(5, 7.6, "AB *",  cex = .8)

# plot --------------------------------------------------------------------


figure = annotate_figure(fig, bottom = text_grob("intervention: p = 0.003  // weigth loss: p = 0.017",  vjust = -0.5, hjust = -1, x = 0.4, face = "italic", size = 10))

figure

cairo_pdf(file.path(figures_path,'Figure_HEDONIC_hpp_bmi_fMRI.pdf'))
print(figure)
dev.off()



# AVERAGED EFFECT -------------
dfH <- summarySEwithin(df,
                       measurevar = "score",
                       withinvars = "session", 
                       idvar = "id")

pal = viridis::inferno(n=5); pal[6] = "#21908CFF" # add one # specialy conceived for colorblindness

labels <- c("0" = "Placebo", "1" = "Liraglutide")

dfH$cond <- ifelse(dfH$session == "pre", -0.25, 0.25)
df$cond <- ifelse(df$session == "pre", -0.25, 0.25)
set.seed(666)
df <- df %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                    grouping = interaction(id, cond))

pp <- ggplot(df, aes(x = cond, y = score,  fill = intervention, color = intervention)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(data = dfH, alpha = 0.5) +
  geom_line(aes(x = condjit, group=id), alpha = 0.2) + 
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(group = session, fill = intervention, color = NA))+
  geom_point(aes(x = condjit), alpha = .3) +
  
  geom_crossbar(data = dfH, aes(y = score, ymin=score-se, ymax=score+se), width = 0.2 , alpha = 0.1)+
  ylab('Beta estimates (a.u.)') +
  xlab('Parahippocampal cortex') +
  #scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) +
  scale_x_continuous(labels=c("Pre", "Post"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
  scale_fill_manual(values=c("0"= pal[1], "1"=pal[6]), guide = 'none') +
  scale_color_manual(values=c("0"=pal[1], "1"=pal[6]), guide = 'none') +
  theme_bw()+ facet_wrap(~intervention, labeller=labeller(intervention = labels))


ppp <- pp + averaged_theme
ppp

cairo_pdf(file.path(figures_path,'Figure_HEDONIC_hpp.pdf'))
print(ppp)
dev.off()



df = merge(diff, cov , by= 'id'); df$intervention = as.factor(df$intervention)

aov_car(data = df, score ~ intervention*session + Error(id/session), factorize = F)


dft = merge(diffSES, cov , by= 'id');
`%notin%` <- Negate(`%in%`); dftout2 = filter(dft, id %notin% c(246))

aov_car(data = dftout2, inter ~ + intervention*bmi + intervention*age + Error(id), factorize = F)

dftout1$OFC = dftout2
write.csv(dftout2, "/home/davidM/Desktop/SwitchDrive/LIRA/data/HED_fmri_OFC.csv", row.names =F)

# plot --------------------------------------------------------------------


dfH$cond <- ifelse(dfH$session == "pre", -0.25, 0.25)
df$cond <- ifelse(df$session == "pre", -0.25, 0.25)
set.seed(666)
df <- df %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
                    grouping = interaction(id, cond))

pp <- ggplot(df, aes(x = cond, y = score,  fill = intervention, color = intervention)) +
  geom_hline(yintercept = 0, lty=2) + 
  geom_point(data = dfH, alpha = 0.5) +
  geom_line(aes(x = condjit, group=id), alpha = 0.2) + 
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(group = session, fill = intervention, color = NA))+
  geom_point(aes(x = condjit), alpha = .3) +
  
  geom_crossbar(data = dfH, aes(y = score, ymin=score-se, ymax=score+se), width = 0.2 , alpha = 0.1)+
  ylab('Beta estimates (a.u.)') +
  xlab('medial OFC') +
  #scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(0,100, by = 20)), limits = c(-0.5,100.5)) +
  scale_x_continuous(labels=c("Pre", "Post"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
  scale_fill_manual(values=c("0"= pal[1], "1"=pal[6]), guide = 'none') +
  scale_color_manual(values=c("0"=pal[1], "1"=pal[6]), guide = 'none') +
  theme_bw()+ facet_wrap(~intervention, labeller=labeller(intervention = labels))


ppp <- pp + averaged_theme
ppp

cairo_pdf(file.path(figures_path,'Figure_HEDONIC_ofc.pdf'))
print(ppp)
dev.off()

# 
# 
# 
# 
# 
# 
# 
# labels <- c("intervention = 0" = "Placebo", "intervention = 1" = "Liraglutide")
# 
# 
# dft$intervention = as.factor(dft$intervention)
# # AVERAGED EFFECT
# dfH <- summarySE(dftout1,  measurevar = "inter",
#                  groupvars = "intervention")
# 
# dfH$cond <- ifelse(dfH$intervention == "0", -0.25, 0.25)
# dftout1$cond <- ifelse(dftout1$intervention == "0", -0.25, 0.25)
# set.seed(666)
# dftout1 <- dftout1 %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
#                               grouping = interaction(id, cond))
# 
# 
# labels <- c("0" = "Placebo", "1" = "Liraglutide")
# # pg <- ggplot_build(plot1); x1 = pg$data[[1]]; x2 = pg$data[[2]];
# # x2$intervention = as.factor(revalue(as.factor(x2$group), c("1"="0", "2"="1")))
# # x1$intervention = as.factor(revalue(as.factor(x1$group), c("1"="0", "2"="1")))
# 
# 
# dft$intervention = as.factor(dft$intervention)
# # AVERAGED EFFECT
# dfH <- summarySE(dft,  measurevar = "inter",
#                  groupvars = "intervention")
# 
# dfH$cond <- ifelse(dfH$intervention == "0", -0.25, 0.25)
# dft$cond <- ifelse(dft$intervention == "0", -0.25, 0.25)
# set.seed(666)
# dft <- dft %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
#                       grouping = interaction(id, cond))
# 
# 
# pp <- ggplot(dft, aes(x = cond, y = inter, 
#                       fill = intervention, color = intervention)) +
#   geom_hline(yintercept = 0) + 
#   geom_point(data = dfH, alpha = 0.5) +
#   geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = intervention, color = NA))+
#   geom_point(aes(x = condjit), alpha = .3,) +
#   geom_crossbar(data = dfH, aes(y = inter, ymin=inter-se, ymax=inter+se), width = 0.2 , alpha = 0.1)+
#   ylab('Beta estimates (a.u.)') +
#   xlab('') +
#   scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(-4,4, by = 1)), limits = c(-5,5)) +
#   scale_x_continuous(labels=c("Placebo", "Liraglutide"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
#   scale_fill_manual(values=c("0"= pal[1], "1"=pal[6]), guide = 'none') +
#   scale_color_manual(values=c("0"=pal[1], "1"=pal[6]), guide = 'none') +
#   theme_bw()+   ggtitle('Averaged over PHC Cluster') 
# 
# 
# ppp1 <- pp + averaged_theme
# ppp1
# 
# 
# 
# 
# 
# # geom_line(data=x1, aes(x=x,y=y)) +
# #   geom_ribbon(data=x2, aes(x=x,y=y, ymin = ymin, ymax = ymax), alpha = 0.3, color = NA) 
# # overall -----------------------------------------------------------------ggarrange(ppp1, ppp0, labels = c("", ""),      common.legend = F)
# 
# 
# 
# 
# 
# # dd ----------------------------------------------------------------------
# 
# 
# # AVERAGED EFFECT
# 
# pp <- ggplot(dftout1, aes(x = bmi, y = inter)) +
#   geom_smooth(method= lm) +
#   #geom_hline(yintercept = 0, linetype = 2) + 
#   #geom_point(data = dfH,aes(x =0), alpha = 0.5, posi) +
#   #geom_flat_violin(aes(x =0), scale = "count", trim = FALSE, alpha = .2)+
#   geom_point(aes(color=intervention),alpha = .3) + 
#   geom_density()
#   stat_ellipse(aes(group=intervention,color=intervention)) +
# 
#   geom_crossbar(data = dfH, aes(x= 0, y = inter, ymin=inter-se, ymax=inter+se), width = 0.2 , alpha = 0.1)+
#   ylab('Beta estimates (a.u.)') +
#   xlab('Fasting glucose') +
#   scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(-3,3, by = 1)), limits = c(-3,3,3.3)) +
#   #scale_x_continuous(expand = c(0, 0), breaks = c(seq.int(-1.5,1.5, by = 1)), limits = c(-1.6,1.6)) +
#   #scale_x_continuous(labels=c("Placebo", "Liraglutide"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
#   scale_fill_manual(values=c("0"= pal[1], "1"=pal[6]), guide = 'none') +
#   scale_color_manual(values=c("0"=pal[1], "1"=pal[6]), guide = 'none') +
#   theme_bw()+ facet_wrap(~intervention, labeller=labeller(intervention = labels)) +   ggtitle('Averaged over whole paraHPP') 
# 
# ppp1 <- pp + averaged_theme
# #+ theme(axis.ticks.x = element_blank(), axis.text.x = element_text(color="white"))
# ppp1
# 
# 
# bf1 = ttestBF(formula = inter ~ intervention, data = dft); bf1
# t1 = t.test(formula = inter ~ intervention, data = dft); t1
# 

# mod2 = lm(data = dft, inter ~ intervention*GLP1 + intervention*Fast_glu + intervention*bmi + intervention*gender + intervention*age + intervention*piss + intervention*thirsty + intervention*hungry + intervention*insulin + intervention*AEA + intervention*PEA + intervention*OEA )
# 
# anova(mod2)
# 
# #interact_plot(mod2,pred = "Fast_glu", modx = "intervention", plot.points = T) 
# 
# # PLOT --------------------------------------------------------------------
# 
# 
# dft$intervention = as.factor(dft$intervention)
# # AVERAGED EFFECT
# dfH <- summarySE(dft,  measurevar = "inter",
#                  groupvars = "intervention")
# 
# dfH$cond <- ifelse(dfH$intervention == "0", -0.25, 0.25)
# dft$cond <- ifelse(dft$intervention == "0", -0.25, 0.25)
# set.seed(666)
# dft <- dft %>% mutate(condjit = jitter(as.numeric(cond), 0.3),
#                       grouping = interaction(id, cond))
# 
# 
# pp <- ggplot(dft, aes(x = cond, y = inter, 
#                       fill = intervention, color = intervention)) +
#   geom_hline(yintercept = 0) + 
#   geom_point(data = dfH, alpha = 0.5) +
#   geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(fill = intervention, color = NA))+
#   geom_point(aes(x = condjit), alpha = .3,) +
#   geom_crossbar(data = dfH, aes(y = inter, ymin=inter-se, ymax=inter+se), width = 0.2 , alpha = 0.1)+
#   ylab('') +
#   xlab('') +
#   ggtitle('Averaged over whole mOFC') +
#   scale_y_continuous(expand = c(0, 0), breaks = c(seq.int(-2,3, by = 1)), limits = c(-2.3,3.3)) +
#   scale_x_continuous(labels=c("Placebo", "Liraglutide"),breaks = c(-.25,.25), limits = c(-.5,.5)) +
#   scale_fill_manual(values=c("0"= pal[1], "1"=pal[6]), guide = 'none') +
#   scale_color_manual(values=c("0"=pal[1], "1"=pal[6]), guide = 'none') +
#   theme_bw()
# 
# ppp2 <- pp + averaged_theme
# ppp2
# 
# 
# 
# bf2 = ttestBF(formula = inter ~ intervention, data = dft); bf2
# t2 = t.test(formula = inter ~ intervention, data = dft); t2 
# 
# 
# 
# 
