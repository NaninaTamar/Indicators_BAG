
cases_agg <- readRDS("../Data/cases_June22.RDS") %>% 
  group_by(report_dt) %>%
  tally() %>%
  mutate(lag1=lag(n,1,default = 0), 
         lag2=lag(n,2,default = 0),
         lag3=lag(n,3,default = 0),
         lag4=lag(n,4,default = 0),
         lag5=lag(n,5,default = 0),
         lag6=lag(n,6,default = 0),
         lag7=lag(n,7,default = 0),
         lag8=lag(n,8,default = 0),
         lag9=lag(n,9,default = 0),
         lag10=lag(n,10,default = 0),
         lag11=lag(n,10,default = 0),
         lag12=lag(n,10,default = 0),
         sign_delta_abs=sign(n-lag1),
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
         three_day_average=(n+lag1+lag2)/3, 
         numb_3day_av_greater_abs10 = as.numeric((three_day_average>lag3) + 
                                                   (three_day_average>lag4) + (three_day_average>lag5) + 
                                                   (three_day_average>lag6) + (three_day_average>lag7) +
                                                   (three_day_average>lag8) + (three_day_average>lag9)+ 
                                                   (three_day_average>lag10) + (three_day_average>lag11) + 
                                                   (three_day_average>lag12)),
         threshold_3day_av_abs10 = as.numeric(numb_3day_av_greater_abs10<=6),
         threshold_3day_av_abs10_kons = as.numeric(numb_3day_av_greater_abs10<=7), 
         numb_greater_abs10=
           as.numeric((n>lag1) + (n>lag2) + (n>lag3) + (n>lag4) + (n>lag5) + (n>lag6) + (n>lag7) + (n>lag8) +(n>lag9)+(n>lag10)), 
         threshold_greater_abs10 = as.numeric(numb_greater_abs10<=5), 
         delta_abs_cases=n-lag1,
         delta_rel_cases5=delta_abs_cases/((lag1+lag2+lag3+lag4+lag5)/5),
         delta_rel_cases5=ifelse(is.na(delta_rel_cases5), 0,
                                 ifelse(delta_rel_cases5>1e9,NA,delta_rel_cases5)), 
         threshold_rel_cases5 = as.numeric(delta_rel_cases5<=0.01), 
         numb_increases_abs7=as.numeric((sign_delta_abs==1) + (sign_lag1==1) + (sign_lag2==1) + (sign_lag3==1)+
                                          (sign_lag4==1)+(sign_lag5==1) + (sign_lag6==1) + (sign_lag7==1)), 
         threshold_increases_abs7=as.numeric(numb_increases_abs7<=3),
         numb_increases_abs10=as.numeric((sign_delta_abs==1) + (sign_lag1==1) + (sign_lag2==1) + (sign_lag3==1)+
                                          (sign_lag4==1)+(sign_lag5==1) + (sign_lag6==1) + (sign_lag7==1) +
                                           (sign_lag8==1) + (sign_lag9==1) + (sign_lag10==1)), 
         threshold_increases_abs7=as.numeric(numb_increases_abs7<=3), 
         threshold_increases_abs10=as.numeric(numb_increases_abs10<=5))

summary(cases_agg)


#png("idea8_applied.png", width=6, height=4, units = 'in', res = 300)
ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_greater_abs10))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases")
#dev.off() 



#png("idea3_applied.png", width=6, height=4, units = 'in', res = 300)
ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_rel_cases5))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases")
#dev.off() 


#png("idea10_applied.png", width=6, height=4, units = 'in', res = 300)
ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_increases_abs7))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases")
#dev.off() 


#png("../output/idea11_applied.png", width=6, height=4, units = 'in', res = 300)
ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_3day_av_abs10))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases")
#dev.off()


ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_3day_av_abs10_kons))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases")



#png("../output/idea10b_applied.png", width=6, height=4, units = 'in', res = 300)
ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_increases_abs10))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases")
#dev.off()


#### by cantons


cases_agg_ktn <- readRDS("cases_June22.RDS") %>% 
  group_by(ktn, report_dt) %>%
  tally() %>%
  mutate(lag1=lag(n,1,default = 0), 
         lag2=lag(n,2,default = 0),
         lag3=lag(n,3,default = 0),
         lag4=lag(n,4,default = 0),
         lag5=lag(n,5,default = 0),
         lag6=lag(n,6,default = 0),
         lag7=lag(n,7,default = 0),
         lag8=lag(n,8,default = 0),
         lag9=lag(n,9,default = 0),
         lag10=lag(n,10,default = 0),
         lag11=lag(n,10,default = 0),
         lag12=lag(n,10,default = 0),
         sign_delta_abs=sign(n-lag1),
         sign_lag1=lag(sign_delta_abs, 1, default = 0),
         sign_lag2=lag(sign_delta_abs, 2, default = 0),
         sign_lag3=lag(sign_delta_abs, 3, default = 0),
         sign_lag4=lag(sign_delta_abs, 4, default = 0),
         sign_lag5=lag(sign_delta_abs, 5, default = 0),
         sign_lag6=lag(sign_delta_abs, 6, default = 0),
         sign_lag7=lag(sign_delta_abs, 7, default = 0),
         three_day_average=(n+lag1+lag2)/3, 
         numb_3day_av_greater_abs10 = as.numeric((three_day_average>lag3) + 
                                                   (three_day_average>lag4) + (three_day_average>lag5) + 
                                                   (three_day_average>lag6) + (three_day_average>lag7) +
                                                   (three_day_average>lag8) + (three_day_average>lag9)+ 
                                                   (three_day_average>lag10) + (three_day_average>lag11) + 
                                                   (three_day_average>lag12)),
         threshold_3day_av_abs10 = as.numeric(numb_3day_av_greater_abs10<=6), 
         numb_greater_abs10=
           as.numeric((n>lag1) + (n>lag2) + (n>lag3) + (n>lag4) + (n>lag5) + (n>lag6) + (n>lag7) + (n>lag8) +(n>lag9)+(n>lag10)), 
         threshold_greater_abs10 = as.numeric(numb_greater_abs10<=5), 
         delta_abs_cases=n-lag1,
         delta_rel_cases5=delta_abs_cases/((lag1+lag2+lag3+lag4+lag5)/5),
         delta_rel_cases5=ifelse(is.na(delta_rel_cases5), 0,
                                 ifelse(delta_rel_cases5>1e9,NA,delta_rel_cases5)), 
         threshold_rel_cases5 = as.numeric(delta_rel_cases5<=0.01), 
         numb_increases_abs7=as.numeric((sign_delta_abs==1) + (sign_lag1==1) + (sign_lag2==1) + (sign_lag3==1)+
                                          (sign_lag4==1)+(sign_lag5==1) + (sign_lag6==1) + (sign_lag7==1)), 
         threshold_increases_abs7=as.numeric(numb_increases_abs7<=3))


ggplot(cases_agg_ktn, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_3day_av_abs10))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases") + facet_wrap(~ktn)


ggplot(cases_agg_ktn, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_3day_av_abs10))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases") + facet_wrap(~ktn, scales = "free")

ggplot(cases_agg, aes(x=report_dt)) + 
  stat_count(aes(weight=n, fill=factor(threshold_3day_av_abs10))) + 
  scale_fill_discrete(name = "Re", labels = c(expression("">=1), expression(""<1))) +
  labs(x="Day", y="Reported cases") + facet_wrap(~ktn, scales = "free") + scale_y_log10()





