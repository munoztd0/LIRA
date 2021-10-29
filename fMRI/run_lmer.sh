cd /home/davidM/OBIWAN/DERIVATIVES/GLM/AFNI/hedonicreactivity/LIRA/
#LM4 = HED5

3dLMEr -prefix LME_7 \
-jobs 30       \
-model 'condition*perceived_liking*session*intervention+BMI_diff+reelin_diff+GLP_diff+age+gender+piss+hungry+(condition*session|Subj)+(1|trialxcondition)'        \
-mask mask.nii \
-dbgArgs \
-resid resid_LME_7.nii   \
-qVars  'perceived_liking,trialxcondition,age,BMI_diff,reelin_diff,GLP_diff,piss,hungry'                            \
-gltCode Rew_Neu  'condition : 1*Reward -1*Neutral'            \
-gltCode Rew_lik  'condition : 1*Reward perceived_liking :'     \
-gltCode Rew_Neu_lik  'condition : 1*Reward -1*Neutral perceived_liking :'     \
-gltCode Rew_pre_lik  'condition : 1*Reward session : 1*Pre perceived_liking :'     \
-gltCode Rew_Neu_pre  'condition : 1*Reward -1*Neutral session : 1*Pre'        \
-gltCode Rew_Neu_pre_lik  'condition : 1*Reward -1*Neutral session : 1*Pre perceived_liking :'     \
-gltCode inter 'condition : 1*Reward session : 1*Post -1*Pre intervention : 1*Treatment -1*Placebo perceived_liking :' \
-gltCode inter_1 'condition : 1*Reward session : 1*Post -1*Pre intervention : 1*Placebo -1*Treatment perceived_liking :' \
-gltCode inter2 'condition : 1*Reward -1*Neutral session : 1*Post -1*Pre intervention : 1*Treatment -1*Placebo perceived_liking :' \
-gltCode inter2_1 'condition : 1*Reward -1*Neutral session : 1*Post -1*Pre intervention : 1*Placebo -1*Treatment perceived_liking :' \
-dataTable @HED_LMER.txt\

#-qVarCenters '0,0,0'  
#+BMI_diff+reelin_diff+GLP_diff+age+gender+ ,BMI_diff,reelin_diff,GLP_diff
3dLMEr -prefix test \
-jobs 20       \
-model 'condition+(1|Subj)'        \
-mask mask.nii \                       \
-dbgArgs    \
-dataTable @LMER_test.txt \

-resid res_test.nii \
nohup bash -x HED8.txt |& tee diary8.txt &

#AFNItoNIFTI -prefix test lme+tlrc[i]
for i in  8 10 12
do
3dAFNItoNIFTI -prefix LME_8_con${i}_z LME_8+tlrc[${i}]
fslmaths LME_8_con${i}_z -ztop -add -1 -mul -1 LME_8_con${i}_1-p # convert to pvalue then inverse it for display 
done
