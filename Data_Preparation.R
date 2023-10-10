# Clean the R working environment

rm(list=ls())


#### Load Genealogical Subsample (Focal Subsample) ####

load("~/Library/Mobile Documents/com~apple~CloudDocs/PHD STAT/PhD Dissertation/Data/data_focal.RData")


#### Upload all the relevant packages ####

packages  = c("here", "data.table", "tidyverse", "reshape2","mapdata",
              "purrr","h2o","readxl","cowplot","kableExtra","hrbrthemes",
              "plotly","LearnBayes","assertthat","readr","babynames","viridis",
              "zoo",'corrr','rstan')

library_upload(packages)


#### Final sample selection ####

# Birth Year>=1741
# Death Year<=1900
# Age at Death>=0 & Age at Death<=110

data_final = indicator_demo %>%
  filter( country_birth_final==country_death_final) %>%
  filter(!(is.na(age_death)),
         !(is.na(gender)),
         gender!="unknown",
         death_year>=1741,
         birth_year<=1900)




#### Population Pyramids ####




#### Population Pyramids: USA ####

usa_population = pop_pyramid_calculation(data_final,"USA",year_min=1741,year_max=1900)


#### Population Pyramids: SWEDEN ####

sweden_population = pop_pyramid_calculation(data_final,"SWEDEN",year_min=1741,year_max=1900)



#### Population Exposure ####

usa_population_exposure = pop_exposure(usa_population)


#### Child-Woman Ratios ####

#### Child-Woman Ratios: USA ####

CW_usa = CW_ratio_calculation(usa_population_exposure)


#### Mortality ####


#### USA: Mortality ####

### Hacker (2010)

qx = c(0.1797,0.0394,0.0171,0.0110,0.0087,
         .2036,0.0444,0.0193,0.0124,0.0098,
         0.2190,0.0476,0.0206,0.0133,0.0104,
         0.2166,0.0472,0.0204,0.0131,0.0103,
         0.2119,0.0462,0.0200,0.0129,0.0101,
         0.2286,0.0496,0.0215,0.0138,0.0108,
         0.2465,0.0542,0.0236,0.0151,0.0118,
         0.2071,0.0496,0.0206,0.0132,0.0103,
         0.1753,0.0406,0.0108,0.0116,0.090,
         0.1494,0.0354,0.0158,0.0102,0.0079,
         0.1305,0.0316,0.0142,.0092,0.0071)

years = c(rep(1790,5),rep(1800,5),rep(1810,5),rep(1820,5),rep(1830,5),
         rep(1840,5),rep(1850,5),rep(1860,5),rep(1870,5),rep(1880,5),
         rep(1890,5))

data_usa_mortality = data.frame(years=year,qx=mort)

data_usa_mortality = data_usa_mortality %>%
  mutate(qx_bar = 1-qx) %>%
  group_by(years) %>%
  summarise(qx=1-prod(qx_bar)) %>%
  add_row(years=1900,qx=0.110)

years = 1790:1900 %>% as.data.frame()
names(years) = "years"
data_usa_mortality_true = years %>% 
  left_join(data_usa_mortality,by="years")

usa_true_mort_rates = na.approx(data_usa_mortality_true$qx,data_usa_mortality_true$years)
usa_mort_rates = data.frame(years=1790:1900,qx=usa_true_mort_rates)

usa_mort_rates %>%
  mutate(status=ifelse(years<1800,"Simulated","Hacker (2010)")) %>%
  ggplot(aes(x=years,y=qx,color=status,fill=status))+
  geom_point()+
  geom_vline(xintercept=1800,linetype="dotted")+
  ylim(c(0,0.5))




data_usa_mort_pre = data.frame(years=1751:1790,qx=NA)

for(i in 1:nrow(data_usa_mort_pre)){
  N = nrow(data_usa_mort_pre)
  error = rnorm(1,0,0.01)
  data_usa_mort_pre$qx[N] = usa_mort_rates$qx[1]
  data_usa_mort_pre$qx[N-i] = data_usa_mort_pre$qx[N-(i-1)] + error
}

usa_mortality_final = rbind(data_usa_mort_pre,usa_mort_rates)


#### SWEDEN: Mortality ####

### Source: Human Mortality Database

sweden_true_mort = read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/PHD STAT/PhD Dissertation/
                            Data/familinx/Sweden_LifeTables.txt", sep="") %>%
  filter(Age %in% c("0","1-4")) %>%
  group_by(Year) %>%
  dplyr::mutate(qx=as.numeric(qx)) %>%
  dplyr::mutate(country="SWEDEN") %>%
  dplyr::mutate(qx = 1-(1-qx)*(1-lead(qx))) %>%
  filter(!(is.na(qx))) %>%
  dplyr::select(years=Year,country,qx)





#### Under-reporting in CWR ####

### True Swedish Population Data
### Source: Human Mortality Database

sweden_population_true = read.csv("/Users/riccardoomenti/Library/Mobile Documents/com~apple~CloudDocs/
                                  PHD STAT/PhD Dissertation/FamiLinx/other data sources/Sweden_Population.txt") %>%
  filter(Year<=1900,!(Age %in% c("UNK","TOT"))) %>%
  mutate(gender=ifelse(Sex=="m","male","female"),
         Age=as.numeric(Age),
         Age=case_when(Age<5 ~ 0,
                       Age>=5 & Age<10 ~ 5,
                       Age>=10 & Age<15 ~ 10,
                       Age>=15 & Age<20 ~ 15,
                       Age>=20 & Age<25 ~ 20,
                       Age>=25 & Age<30 ~ 25,
                       Age>=30 & Age<35 ~ 30,
                       Age>=35 & Age<40 ~ 35,
                       Age>=40 & Age<45 ~ 40,
                       Age>=45 & Age<50 ~ 45,
                       Age>=50 & Age<55 ~ 50,
                       Age>=55 & Age<60 ~ 55,
                       Age>=60 & Age<65 ~ 60,
                       Age>=65 & Age<70 ~ 65,
                       Age>=70 & Age<75 ~ 70,
                       Age>=75 & Age<80 ~ 75,
                       Age>=80 ~ 80)) %>%
  select(-Sex) %>%
  group_by(Year,Age,gender) %>%
  rename(years=Year) %>%
  mutate(years = ifelse(years==1751,1751,years)) %>%
  dplyr::summarize(pop_true=sum(Population)) 


child_sweden_true = Sweden_Population %>%
  mutate(Age=ifelse(Age=="110+",110,as.numeric(Age))) %>%
  filter(Age<=4,Year<=1900) %>%
  group_by(Year) %>%
  summarise(C=sum(Total)) 


women_sweden_true = Sweden_Population %>%
  mutate(Age=ifelse(Age=="110+",110,as.numeric(Age))) %>%
  filter(Age>14,Age<50,Year<=1900) %>%
  group_by(Year) %>%
  summarise(W=sum(Female))


pop_sweden_true = Sweden_Population %>%
  mutate(Age=ifelse(Age=="110+",110,as.numeric(Age))) %>%
  filter(Year<=1900) %>%
  group_by(Year) %>%
  summarise(Tot=sum(Total))


CW_sweden_true = child_sweden_true %>%
  left_join(pop_sweden_true,by="Year") %>%
  left_join(women_sweden_true,by="Year") %>%
  mutate(CW_true = C/W,
         C_prop = C/Tot,
         W_prop = W/Tot) %>%
  rename(years=Year)




under_count = CW_sweden %>%
  select(years,CW_smooth) %>%
  left_join(CW_sweden_true,by="years") %>%
  left_join(sweden_true_mort,by="years") %>%
  mutate(multiplier=CW_true/CW_smooth) %>%
  ungroup() %>%
  select(years,multiplier)



multiplier_indicators = CW_sweden %>%
  left_join(sweden_true_fert_interpolated,by="years") %>%
  mutate(W1549 = W15_smooth + W20_smooth + W25_smooth + 
           W30_smooth + W35_smooth + W40_smooth + W45_smooth,
         p2534 = (W25_smooth+W30_smooth)/W1549,
         multiplier_iTFR = TFR*(1-0.75*qx)*1/7*CW^(-1),
         multiplier_xTFR = TFR*CW^(-1)*(1-0.75*qx)*1/(10.65 - 12.55*p2534)) %>%
  ungroup() %>%
  select(years,multiplier_iTFR,multiplier_xTFR)



#### Fertility Estimation ####

#### Fertility Estimation: USA ####

usa_fertility_sample = CW_usa %>%
  left_join(usa_mortality_final, 
            by="years")

usa_fertility_schmertmann = bayesTFR(usa_fertility_sample)

usa_fertility_extended = bayesTFR_new(usa_fertility_sample)

#### Fertility Estimation: SWEDEN ####


usa_fertility_sample = CW_sweden %>%
  left_join(sweden_mor, 
            by="years")

sweden_fertility_schmertmann = bayesTFR(sweden_fertility_sample)

sweden_fertility_extended = bayesTFR_new(sweden_fertility_sample)













