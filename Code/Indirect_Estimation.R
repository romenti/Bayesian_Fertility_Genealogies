#### Code to implement the indirect estimation method ####


# upload all relevant packages

source('code/upload_packages.R')


# upload data set for indirect estimation

load('Data/indirect_estimation_data_input.RData')

#### Calculate multiplier ####

#### Sweden ####

multiplier_sweden = CW_sweden %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_sweden_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true)


#### England & Wales ####

multiplier_eng = CW_eng %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_eng_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true)


#### France ####

multiplier_france = CW_france %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_france_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true) %>%
  drop_na()

#### Denmark ####

multiplier_denmark = CW_denmark %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_denmark_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true) %>%
  drop_na()

#### Norway ####

multiplier_norway = CW_norway %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_norway_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true) %>%
  drop_na()



#### Netherlands ####

multiplier_netherlands = CW_netherlands %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_netherlands_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true) %>%
  drop_na()

#### USA ####

multiplier_usa = CW_usa %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_usa_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true) %>%
  drop_na()


#### Finland ####

multiplier_finland = CW_finland %>%
  mutate(CW=C/W) %>%
  select(years,country,CW) %>%
  left_join(CW_finland_true %>%
              select(years,country,CW_true)) %>%
  mutate(multiplier=CW/CW_true) %>%
  drop_na()


#### data for multiplier predictions ####


data_reg = rbind(multiplier_denmark,
                 multiplier_sweden,
                 multiplier_finland,
                 multiplier_eng,
                 multiplier_netherlands,
                 multiplier_france,
                 multiplier_usa,
                 multiplier_norway) %>%
  mutate(log_multiplier=log(multiplier),
         country=factor(country),
         time=years-1750)
  



#### Predict multiplier: France ####

data_reg_france = filter(data_reg,years %in% 1751:1826,country %in% c('SWE','FRA'))



model_reg_france = mgcv::gam(log_multiplier~
                               s(time,bs='bs')+
                               s(country,bs='re')+
                               s(country,time,bs='re'),
                             data=data_reg_france )



fra_predictions = exp(predict(model_reg_france,
                              newdata = data.frame(time=seq(1,65,1),
                                                   country=rep('FRA',65))))


multiplier_france = data.frame(years=1751:1910,country=rep('FRA',160),
                               multiplier=c(fra_predictions,multiplier_france$multiplier))




#### Predict multiplier: Denmark ####


data_reg_denmark = data_reg %>%
  filter(country %in% c('DEN','SWE'),years %in% 1751:1845)


model_reg_denmark = mgcv::gam(log_multiplier~
                                s(time,bs='ps')+
                                s(country,bs='re')+
                                s(country,time,bs='re'),
                              data=data_reg_denmark %>%
                                filter(years<=1845))




den_predictions = exp(predict(model_reg_denmark,newdata = data.frame(time=seq(1,84,1),
                                                                     country=rep('DEN',84))))


multiplier_denmark = data.frame(years=1751:1910,country=rep('DEN',160),
                                multiplier=c(den_predictions,multiplier_denmark$multiplier))




#### Predict multiplier: Norway ####



data_reg_norway = data_reg %>%
  filter(country %in% c('NOR','SWE'),years<=1856)


model_reg_norway = mgcv::gam(log_multiplier~
                               s(time,bs='ps')+
                               s(country,bs='re')+
                               s(time,country,bs='re'),
                             data=data_reg_norway)




nor_predictions = exp(predict(model_reg_norway,newdata = data.frame(time=seq(1,95,1),country=rep('NOR',95))))


multiplier_norway = data.frame(years=1751:1910,country=rep('NOR',160),
                               multiplier=c(nor_predictions,multiplier_norway$multiplier))





#### Predict multiplier: USA ####

data_reg_usa = data_reg %>%
  filter(years<=1840,
         country %in% c('USA','SWE'))


model_reg_usa = mgcv::gam(log_multiplier~
                            s(time,bs='ps')+
                            s(country,bs='re')+
                            s(country,time,bs='re'),
                          data= data_reg_usa %>%
                            filter(years<=1840))


usa_predictions = exp(predict(model_reg_usa,newdata = data.frame(time=seq(1,79,1),
                                                                 country=rep('USA',79))))


multiplier_usa = data.frame(years=1751:1910,country=rep('USA',160),
                            multiplier=c(usa_predictions,multiplier_usa$multiplier))




#### Predict multiplier: Netherlands ####


data_reg_nld = data_reg %>%
  filter(years<=1855,
         country %in% c('NLD','SWE'))

model_reg_nld = mgcv::gam(log_multiplier~
                            s(time,bs='cs')+
                            s(country,bs='re')+
                            s(country,time,bs='re'),
                          data=data_reg_nld)


nld_predictions = exp(predict(model_reg_nld,newdata = data.frame(time=seq(1,99,1),
                                                                 country=rep('NLD',99))))


multiplier_nld = data.frame(years=1751:1910,country=rep('NLD',160),
                            multiplier=c(nld_predictions,multiplier_netherlands$multiplier))





#### Predict multiplier: Finland ####




data_reg_finland = data_reg %>%
  filter(years<=1875,
         country %in% c('FIN','SWE'))



model_reg_finland = mgcv::gam(log_multiplier~
                                s(time,bs='cs')+
                                s(country,bs='re')+
                                s(country,time,bs='re'),
                              data=data_reg_finland)


fin_predictions = exp(predict(model_reg_finland,newdata = data.frame(time=seq(1,114,1),
                                                                     country=rep('FIN',114))))


multiplier_finland = data.frame(years=1751:1910,country=rep('FIN',160),
                                multiplier=c(fin_predictions,multiplier_finland$multiplier))





#### iTFR and xTFR: indicators ####



#### Sweden: iTFR and xTFR ####

iTFR_sweden = CW_sweden %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_sweden %>%
              select(years,multiplier)) %>%
  left_join(sweden_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)


xTFR_sweden = CW_sweden %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_sweden %>%
              select(years,multiplier)) %>%
  left_join(sweden_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)


#### England: iTFR and xTFR ####

iTFR_eng = CW_eng %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_eng %>%
              select(years,multiplier)) %>%
  left_join(eng_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)


xTFR_eng = CW_eng %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_eng %>%
              select(years,multiplier)) %>%
  left_join(eng_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)





#### France: iTFR and xTFR ####


iTFR_france = CW_france %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_france %>%
              select(years,multiplier)) %>%
  left_join(france_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)


xTFR_france = CW_france %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_france %>%
              select(years,multiplier)) %>%
  left_join(france_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)




#### Norway: iTFR and xTFR ####


iTFR_norway = CW_norway %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_norway %>%
              select(years,multiplier)) %>%
  left_join(norway_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)

plot(iTFR_norway$iTFR_star)

xTFR_norway = CW_norway %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_norway %>%
              select(years,multiplier)) %>%
  left_join(norway_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)



#### Netherlands: iTFR and xTFR ####


iTFR_netherlands = CW_netherlands %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_nld %>%
              select(years,multiplier)) %>%
  left_join(netherlands_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)



xTFR_netherlands = CW_netherlands %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_nld %>%
              select(years,multiplier)) %>%
  left_join(netherlands_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)




#### Finland: iTFR and xTFR ####


iTFR_finland = CW_finland %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_finland %>%
              select(years,multiplier)) %>%
  left_join(finland_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)



xTFR_finland = CW_finland %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_finland %>%
              select(years,multiplier)) %>%
  left_join(finland_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)


plot(xTFR_finland$xTFR_star)




#### Denmark: iTFR and xTFR ####


iTFR_denmark = CW_denmark %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_denmark %>%
              select(years,multiplier)) %>%
  left_join(denmark_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)



xTFR_denmark = CW_denmark %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_denmark %>%
              select(years,multiplier)) %>%
  left_join(denmark_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)





#### USA: iTFR and xTFR ####


iTFR_usa = CW_usa %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_usa %>%
              select(years,multiplier)) %>%
  left_join(usa_mortality_final) %>%
  mutate(iTFR=7*CW,
         iTFR_hat=7*CW*1/(1-0.75*qx),
         iTFR_star=7*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,iTFR,iTFR_hat,iTFR_star)



xTFR_usa = CW_usa %>%
  mutate(CW=C/W) %>%
  left_join(multiplier_usa %>%
              select(years,multiplier)) %>%
  left_join(usa_mortality_final) %>%
  mutate(p=(W25+W30)/W,
         xTFR=(10.65-12.65*p)*CW,
         xTFR_hat=(10.65-12.65*p)*CW*1/(1-0.75*qx),
         xTFR_star=(10.65-12.65*p)*CW*1/(1-0.75*qx)*1/multiplier) %>%
  select(years,country,xTFR,xTFR_hat,xTFR_star)


#### Save results ####

# save multiplier values and TFR indicators in data sets with all countries


data_iTFR = rbind(iTFR_denmark,iTFR_eng,
      iTFR_finland,iTFR_sweden,
      iTFR_norway,iTFR_netherlands,
      iTFR_usa,iTFR_france) %>%
  select(years,country,iTFR_star)

data_xTFR = rbind(xTFR_denmark,xTFR_eng,
                  xTFR_finland,xTFR_sweden,
                  xTFR_norway,xTFR_netherlands,
                  xTFR_usa,xTFR_france) %>%
  select(years,country,xTFR_star)

data_multiplier = rbind(multiplier_denmark,
                        multiplier_eng %>%
                          select(years,country,multiplier),
                        multiplier_sweden %>%
                          select(years,country,multiplier),
                        multiplier_france,
                        multiplier_nld,
                        multiplier_norway,
                        multiplier_usa,
                        multiplier_finland)
  


save(data_iTFR,data_xTFR,data_multiplier,file='Results/indirect_TFR_results.RData')






