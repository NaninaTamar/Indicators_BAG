## Setup
source("setup_NA.R")
sim_data = readRDS("Data/sim_data2.rds")

## Data management
sim_out = 
  do.call("rbind",sim_data) %>%
  tbl_df() %>%
  gather("day","cases",5:64) %>%
  mutate(day=as.numeric(gsub("X","",day))) %>%
  mutate(combi_id=paste0(n,"-",sim)) %>%
  arrange(combi_id) %>%
  group_by(combi_id) %>%
  mutate(
    lag1=lag(cases,1,default=0),
    lag2=lag(cases,2,default=0),
    lag3=lag(cases,3,default=0),
    lag4=lag(cases,4,default=0),
    lag5=lag(cases,5,default=0),
    lag6=lag(cases,6,default=0),
    lag7=lag(cases,7,default=0),
    lag8=lag(cases,8,default=0), 
    lag9=lag(cases,9,default=0),
    lag10=lag(cases,10,default=0),
    lag11=lag(cases,11, default=0),
    lag12=lag(cases,12, default=0),
    lag13=lag(cases,13, default = 0),
    lag14=lag(cases,14, default = 0), 
    lag15=lag(cases,15, default = 0),
    lag16=lag(cases,16, default = 0),
    delta_abs_cases=cases-lag1,
    delta_rel_cases=delta_abs_cases/lag1,
    delta_rel_cases=ifelse(is.na(delta_rel_cases), 0, 
                           ifelse(delta_rel_cases>1e9,NA,delta_rel_cases)),
    delta_rel_cases3=delta_abs_cases/((lag1+lag2+lag3)/3),
    delta_rel_cases3=ifelse(is.na(delta_rel_cases3), 0, 
                            ifelse(delta_rel_cases3>1e9,0,delta_rel_cases3)),
    delta_rel_cases5=delta_abs_cases/((lag1+lag2+lag3+lag4+lag5)/5),
    delta_rel_cases5=ifelse(is.na(delta_rel_cases5), 0,
                            ifelse(delta_rel_cases5>1e9,NA,delta_rel_cases5)),
    delta_rel_cases7=delta_abs_cases/((lag1+lag2+lag3+lag4+lag5+lag6+lag7)/7),
    delta_rel_cases7=ifelse(is.na(delta_rel_cases7), 0, 
                            ifelse(delta_rel_cases7>1e9,NA,delta_rel_cases7)), 
    prop_cases=cases/lag1,
    prop_cases=ifelse(is.na(prop_cases), 0,
                      ifelse(prop_cases>1e9, NA, prop_cases)),
    prop_cases3=cases/((lag1+lag2+lag3)/3),
    prop_cases3=ifelse(is.na(prop_cases3), 0,
                       ifelse(prop_cases3>1e9, NA, prop_cases3)),
    prop_cases5=cases/((lag1+lag2+lag3+lag4+lag5)/5), 
    prop_cases5=ifelse(is.na(prop_cases5), 0, 
                       ifelse(prop_cases5>1e9, NA, prop_cases5)),
    prop_cases7=cases/((lag1+lag2+lag3+lag4+lag5+lag6+lag7)/7), 
    prop_cases7=ifelse(is.na(prop_cases7), 0, 
                       ifelse(prop_cases7>1e9, NA, prop_cases7)), 
    sign_delta_abs=sign(delta_abs_cases),
    sign_lag1=lag(sign_delta_abs, 1, default = 0),
    sign_lag2=lag(sign_delta_abs, 2, default = 0),
    sign_lag3=lag(sign_delta_abs, 3, default = 0),
    sign_lag4=lag(sign_delta_abs, 4, default = 0),
    sign_lag5=lag(sign_delta_abs, 5, default = 0),
    sign_lag6=lag(sign_delta_abs, 6, default = 0),
    sign_lag7=lag(sign_delta_abs, 7, default = 0),
    sign_lag8=lag(sign_delta_abs, 8, default = 0),
    sign_lag9=lag(sign_delta_abs, 9, default = 0),
    sign_lag10=lag(sign_delta_abs, 10, default = 0),
    cumsum_cases_rev = rev(cumsum(rev(cases))),
    three_day_average=(cases+lag1+lag2)/3, 
    numb_3day_av_greater_abs10 = as.numeric((three_day_average>lag3) + 
                                              (three_day_average>lag4) + (three_day_average>lag5) + 
                                             (three_day_average>lag6) + (three_day_average>lag7) +
                                             (three_day_average>lag8) + (three_day_average>lag9)+ 
                                             (three_day_average>lag10) + (three_day_average>lag11) + 
                                             (three_day_average>lag12)),
    seven_day_average=(cases+lag1+lag2+lag3+lag4+lag5+lag6)/7, 
    seven_day_av_g=cut(seven_day_average, breaks = c(0,10,30,200), include.lowest = TRUE),
    numb_7day_av_greater_abs10 = as.numeric((seven_day_average>lag7) +
                                              (seven_day_average>lag8) + (seven_day_average>lag9) +
                                              (seven_day_average>lag10) + (seven_day_average>lag11) +
                                              (seven_day_average>lag12) + (seven_day_average>lag13) +
                                              (seven_day_average>lag14) + (seven_day_average>lag15) +
                                              (seven_day_average>lag16)), 
    numb_7day_av_greater_abs7 = as.numeric((seven_day_average>lag7) +
                                              (seven_day_average>lag8) + (seven_day_average>lag9) +
                                              (seven_day_average>lag10) + (seven_day_average>lag11) +
                                              (seven_day_average>lag12) + (seven_day_average>lag13)), 
    min_3day = pmin(cases, lag1, lag2),
    min_3day_greater_abs10 = as.numeric((min_3day>lag3) + 
      (min_3day>lag4) + (min_3day>lag5) + 
      (min_3day>lag6) + (min_3day>lag7) +
      (min_3day>lag8) + (min_3day>lag9)+ 
      (min_3day>lag10) + (min_3day>lag11) + 
      (min_3day>lag12)),
    max_3day = pmax(cases, lag1, lag2), 
    max_3_day_greater_abs10 =as.numeric((max_3day>lag3) + 
                                          (max_3day>lag4) + (max_3day>lag5) + 
                                          (max_3day>lag6) + (max_3day>lag7) +
                                          (max_3day>lag8) + (max_3day>lag9)+ 
                                          (max_3day>lag10) + (max_3day>lag11) + 
                                          (max_3day>lag12)),
    numb_greater_abs3=as.numeric((cases>lag1) + (cases>lag2) + (cases>lag3)),
    numb_greater_abs5=as.numeric((cases>lag1) + (cases>lag2) + (cases>lag3) + (cases>lag4) + (cases>lag5)), 
    numb_greater_abs7=as.numeric((cases>lag1) + (cases>lag2) + (cases>lag3) + (cases>lag4) + (cases>lag5) + (cases>lag6) + (cases>lag7)), 
    numb_greater_abs10=as.numeric((cases>lag1) + (cases>lag2) + (cases>lag3) + (cases>lag4) + (cases>lag5)+(cases>lag6)+(cases>lag7) + (cases>lag8) +(cases>lag9)+(cases>lag10)), 
    numb_increases_abs5=as.numeric((sign_delta_abs==1) + (sign_lag1==1) + (sign_lag2==1) + (sign_lag3==1) + (sign_lag4==1) + (sign_lag5==1)), 
    numb_increases_abs7=as.numeric((sign_delta_abs==1) + (sign_lag1==1) + (sign_lag2==1) + (sign_lag3==1)+(sign_lag4==1)+(sign_lag5==1) + (sign_lag6==1) + (sign_lag7==1)), 
    numb_increases_abs10=as.numeric((sign_delta_abs==1) + (sign_lag1==1) + (sign_lag2==1) + (sign_lag3==1)+(sign_lag4==1)+(sign_lag5==1) + (sign_lag6==1) + (sign_lag7==1) +
                                      +(sign_lag8==1) + (sign_lag9==1) + (sign_lag10==1))) %>% 
  rowwise() %>%
  mutate(median_7_day=median(cases, lag1, lag2, lag3, lag4, lag5,lag6)) %>%
  group_by(combi_id) %>%
  mutate(numb_median7_greater_abs7 = as.numeric((median_7_day>lag7) + (median_7_day>lag8) + (median_7_day>lag9) +
                                              (median_7_day>lag10) + (median_7_day>lag11) + (median_7_day>lag12) + 
                                              (median_7_day>lag13)), 
         numb_median7_greater_abs10 = as.numeric((median_7_day>lag7) + (median_7_day>lag8) + (median_7_day>lag9) +
                                                  (median_7_day>lag10) + (median_7_day>lag11) + (median_7_day>lag12) + 
                                                  (median_7_day>lag13) + (median_7_day>lag14) + (median_7_day>lag15) +
                                                  (median_7_day>lag16))) %>% filter(day<=45,day>30)
 

# png("../output/trajectories.png", width=7, height=6, units = 'in', res = 300)
ggplot(sim_out) +
  geom_line(aes(x=day,y=cases,group=combi_id,colour=R0),size=0.1,alpha=.1) +
  facet_grid(p~seed,scales="free") +
  labs(x="Day",y="Cases")
# dev.off()

## Idea 1: increase in absolute cases from the last day
roc1 = get_roc(data=sim_out,response="R0",predictor="delta_abs_cases")

plot_allR(roc = roc1) + ggtitle("1-day absolute increase")
plot_specificR(roc = roc1, R0spec = 1) + ggtitle("1-day absolute increase, R=1")


## Idea 2: increase in relative cases from the last day
roc2 = get_roc(data=sim_out,response="R0",predictor="delta_rel_cases")

plot_allR(roc = roc2) + ggtitle("1-day relative increase")
# plot_specificR(roc = roc2, R0spec = 1) + ggtitle("1-day relative increase, R=1")


## Idea 3: increase in relative cases from the average in the last 5 days
roc3 = get_roc(data=sim_out,response="R0",predictor="delta_rel_cases5")

plot_allR(roc = roc3) + ggtitle("relative incrase compared to average of last 5 days")
plot_specificR(roc = roc3, R0spec = 1) + ggtitle("relative incrase compared to average of last 5 days, R=1")

## Idea 4: absolute number of cases
roc4 = get_roc(data=sim_out,response="R0",predictor="cases")

plot_allR(roc = roc4) + ggtitle("Absolute number of cases")
plot_specificR(roc = roc4, R0spec = 1) + ggtitle("Absolute number of cases, R=1")

## Idea 5: Proportional increase compared to day before

roc5 = get_roc(data=sim_out,response="R0",predictor="prop_cases")

plot_allR(roc = roc5) + ggtitle("Proportional 1-day increase")

## Idea 6: Proportional increase compared to average of last 7days

roc6 = get_roc(data=sim_out,response="R0",predictor="prop_cases7")

plot_allR(roc = roc6) + ggtitle("Proportional increase compared to average of last 7 days")

## Idea 7 Greater than how many of the cases in the last 7 days

roc7 = get_roc(data=sim_out,response="R0",predictor="numb_greater_abs7")

plot_allR(roc = roc7) + ggtitle("Greater than how many of cases in last 7 days")
plot_specificR(roc = roc7, R0spec = 1) + ggtitle("Greater than how many of the cases in the last 7 days, R=1")


## Idea 8: Greater than how many cases in the 10 previous days:

roc8 = get_roc(data=sim_out,response="R0",predictor="numb_greater_abs10")

plot_allR(roc = roc8) + ggtitle("Greater than how many of cases in last 10 days")
plot_specificR(roc = roc8, R0spec = 1) + ggtitle("Greater than how many of the cases in the last 10 days, R=1")


## Idea 9: How many day-to-day increases within last 5 days

roc9 = get_roc(data=sim_out,response="R0",predictor="numb_increases_abs5")

plot_allR(roc = roc9) + ggtitle("Number of day-to-day increases within last 5 days")
plot_specificR(roc = roc9, R0spec = 1) + ggtitle("Number of day-to-day increases within last 5 days, R=1")

## Idea 10: How many day-to-day increases within last 7 days

roc10 = get_roc(data=sim_out,response="R0",predictor="numb_increases_abs7")

plot_allR(roc = roc10) + ggtitle("Number of day-to-day increases within last 7 days")
plot_specificR(roc = roc10, R0spec = 1) + ggtitle("Number of day-to-day increases within last 7 days, R=1")


## Idea 10b: How many day-to-day increases within last 10 days

roc10b = get_roc(data=sim_out,response="R0",predictor="numb_increases_abs10")


plot_allR(roc = roc10b) + ggtitle("Number of day-to-day increases within last 10 days")

#png("../output/idea10b_sens_spec_2.png", width=10, height=9, units = 'in', res = 300)
plot_specificR(roc = roc10b, R0spec = 1) + ggtitle("Number of day-to-day increases within last 10 days, R=1")
#dev.off()

## Idea 11: Average of last three days greater than how many cases of the 10 previous days:

roc11 = get_roc(data=sim_out, response="R0", predictor="numb_3day_av_greater_abs10")

plot_allR(roc = roc11) + ggtitle("3-day average greater than how many cases of prev. 10 days")
#png("../output/idea11_sens_spec_2.png", width=10, height=9, units = 'in', res = 300)
plot_specificR(roc = roc11, R0spec = 1) + ggtitle("3-day average greater than how many cases of prev. 10 days, R=1")
#dev.off()



## Idea 12: Minimum of last three day greater than how many cases of the 10 previous days:

roc12 = get_roc(data=sim_out,response="R0",predictor="min_3day_greater_abs10")

plot_allR(roc = roc12) + ggtitle("3-day minimum greater than how many cases of prev. 10 days")
plot_specificR(roc = roc12, R0spec = 1) + ggtitle("3-day minimum greater than how many cases of prev. 10 days, R=1")


## Idea 12b: Maximum of last three day greater than how many cases of the 10 previous days:

roc12b = get_roc(data=sim_out,response="R0",predictor="max_3_day_greater_abs10")

plot_allR(roc = roc12b) + ggtitle("3-day maximum greater than how many cases of prev. 10 days")
plot_specificR(roc = roc12b, R0spec = 1) + ggtitle("3-day maximum greater than how many cases of prev. 10 days, R=1")


## Idea 13: Median of last seven days greater than how many cases of the 7 previous days:

roc13 = get_roc(data=sim_out,response="R0",predictor="numb_median7_greater_abs7")

plot_allR(roc = roc13) + ggtitle("7-day median greater than how many cases of prev. 7 days")
plot_specificR(roc = roc13, R0spec = 1) + ggtitle("7-day median greater than how many cases of prev. 7 days, R=1")

## Idea 14: Median of last 7 days greater than how many cases of the 10 previous days:

roc14 = get_roc(data=sim_out,response="R0",predictor="numb_median7_greater_abs10")

plot_allR(roc = roc14) + ggtitle("7-day median greater than how many cases of prev. 10 days")
plot_specificR(roc = roc14, R0spec = 1) + ggtitle("7-day median greater than how many cases of prev. 10 days, R=1")


## Idea 15: Average of last 7 days greater than how many cases of the 7 previous days:

roc15 = get_roc(data=sim_out,response="R0",predictor="numb_7day_av_greater_abs7")

plot_allR(roc = roc15) + ggtitle("7-day mean greater than how many cases of prev. 7 days")
plot_specificR(roc = roc15, R0spec = 1) + ggtitle("7-day mean greater than how many cases of prev. 7 days, R=1")


## Idea 16: Average of last 7 days greater than how many cases of the 10 previous days:

roc16 = get_roc(data=sim_out,response="R0",predictor="numb_7day_av_greater_abs10")

plot_allR(roc = roc16) + ggtitle("7-day mean greater than how many cases of prev. 10 days")
plot_specificR(roc = roc16, R0spec = 1) + ggtitle("7-day mean greater than how many cases of prev. 10 days, R=1")

## average of reported cases:

reported <- sim_out %>%
  group_by(p, seed) %>%
  summarise(aver_cases=mean(cases))

reported

reported <- sim_out %>%
  group_by(p, seed, R0) %>%
  summarise(aver_cases=mean(cases))


### also by seven day average


## We could also exclude all epidemics thathose that die out:

sim_out2 <- sim_out %>% filter(cumsum_cases_rev!=0)

reported2 <- sim_out2 %>%
  group_by(p,seed, R0) %>%
  summarise(mean_cases=mean(cases))

# # 
# # 
# # roc1 = expand.grid(threshold=seq(1,50,by=1),
# #             p=unique(sim_out$p),
# #             seed=unique(sim_out$seed),
# #             sensitivity=NA,
# #             specificity=NA)
# # for(i in 1:dim(roc1)[[1]]) {
# #   roc1[i,"specificity"] = mean(sim_out[sim_out$p==roc1[i,"p"] & sim_out$seed==roc1[i,"seed"] & sim_out$R0<1,"delta_abs_cases"]<roc1[i,"threshold"])
# #   roc1[i,"sensitivity"] = mean(sim_out[sim_out$p==roc1[i,"p"] & sim_out$seed==roc1[i,"seed"] & sim_out$R0>=1,"delta_abs_cases"]>=roc1[i,"threshold"])
# # }
# # roc1_summary = group_by(roc1,threshold) %>%
# #   summarise(mean_sst=mean(sensitivity),median_sst=median(sensitivity),low_sst=quantile(sensitivity,0.25),high_sst=quantile(sensitivity,0.75),
# #             mean_spe=mean(specificity),median_spe=median(specificity),low_spe=quantile(specificity,0.25),high_spe=quantile(specificity,0.75))
# # ggplot(roc1_summary) +
# #   geom_step(aes(x=1-low_spe,y=low_sst),linetype=3) +
# #   geom_step(aes(x=1-high_spe,y=high_sst),linetype=3) +
# #   geom_step(aes(x=1-median_spe,y=median_sst)) +
# #   geom_label(aes(x=1-median_spe-0.03,y=(median_sst)+.03,label=threshold)) +
# #   geom_segment(x=0,y=0,xend=1,yend=1,linetype=2,color="grey",size=.2) +
# #   coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
# #   scale_x_continuous(labels=function(x) paste0((1-x)*100,"%"),breaks=c(0,0.25,.5,.75,1)) +
# #   scale_y_continuous(labels=scales::percent) +
# #   labs(x="Specificity",y="Sensitivity")
# 
# 
# ## Idea 2: relative increase from the last day
# roc2 = expand.grid(threshold=seq(0,5,by=.1),
#                    p=unique(sim_out$p),
#                    seed=unique(sim_out$seed),
#                    sensitivity=NA,
#                    specificity=NA)
# for(i in 1:dim(roc2)[[1]]) {
#   roc2[i,"specificity"] = mean(sim_out[sim_out$p==roc2[i,"p"] & sim_out$seed==roc2[i,"seed"] & sim_out$R0<1,"delta_rel_cases"]<roc2[i,"threshold"])
#   roc2[i,"sensitivity"] = mean(sim_out[sim_out$p==roc2[i,"p"] & sim_out$seed==roc2[i,"seed"] & sim_out$R0>=1,"delta_rel_cases"]>=roc2[i,"threshold"])
# }
# roc2_summary = group_by(roc2,threshold) %>%
#   summarise(mean_sst=mean(sensitivity),median_sst=median(sensitivity),low_sst=quantile(sensitivity,0.25),high_sst=quantile(sensitivity,0.75),
#             mean_spe=mean(specificity),median_spe=median(specificity),low_spe=quantile(specificity,0.25),high_spe=quantile(specificity,0.75))
# ggplot(roc2_summary) +
#   geom_step(aes(x=1-low_spe,y=low_sst),linetype=3) +
#   geom_step(aes(x=1-high_spe,y=high_sst),linetype=3) +
#   geom_step(aes(x=1-median_spe,y=median_sst)) +
#   geom_label(aes(x=1-median_spe-0.03,y=(median_sst)+.03,label=paste0("+",100*threshold,"%"))) +
#   geom_segment(x=0,y=0,xend=1,yend=1,linetype=2,color="grey",size=.2) +
#   coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
#   scale_x_continuous(labels=function(x) paste0((1-x)*100,"%"),breaks=c(0,0.25,.5,.75,1)) +
#   scale_y_continuous(labels=scales::percent) +
#   labs(x="Specificity",y="Sensitivity")
# 
# ## Idea 3: relative increase from the last 3 days
# roc3 = expand.grid(threshold=c(-Inf,0,.1,.2,.3,.4,.5,.7,1,2,Inf),
#                    p=unique(sim_out$p),
#                    seed=unique(sim_out$seed),
#                    sensitivity=NA,
#                    specificity=NA)
# for(i in 1:dim(roc3)[[1]]) {
#   roc3[i,"specificity"] = mean(sim_out[sim_out$p==roc3[i,"p"] & sim_out$seed==roc3[i,"seed"] & sim_out$R0<1,"delta_rel_cases3"]<roc3[i,"threshold"])
#   roc3[i,"sensitivity"] = mean(sim_out[sim_out$p==roc3[i,"p"] & sim_out$seed==roc3[i,"seed"] & sim_out$R0>=1,"delta_rel_cases3"]>=roc3[i,"threshold"])
# }
# plot_roc(roc3)
# 
# ## Idea 3: relative increase from the last 3 days
# roc4 = expand.grid(threshold=c(-Inf,0,.1,.2,.3,.4,.5,.7,1,2,Inf),
#                    p=unique(sim_out$p),
#                    seed=unique(sim_out$seed),
#                    sensitivity=NA,
#                    specificity=NA)
# for(i in 1:dim(roc4)[[1]]) {
#   roc4[i,"specificity"] = mean(sim_out[sim_out$p==roc4[i,"p"] & sim_out$seed==roc4[i,"seed"] & sim_out$R0<1,"delta_rel_cases7"]<roc4[i,"threshold"])
#   roc4[i,"sensitivity"] = mean(sim_out[sim_out$p==roc4[i,"p"] & sim_out$seed==roc4[i,"seed"] & sim_out$R0>=1,"delta_rel_cases7"]>=roc4[i,"threshold"])
# }
# plot_roc(roc4)
# 
# roc4bis=roc(response=sim_out$R0>=1,predictor=sim_out$delta_rel_cases7)
# plot(roc4bis)
# auc(roc4bis)
# ci(roc4bis, of = "sp")
