# clean the working environment

rm(list=ls())

##### Functions ####


#### Install and upload libraries ####

library_upload = function (package1, ...) {
  packages = c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages( do.call(library, list(package)) )
      print(paste("library2:",package, "loaded."))
    }
    else {
      tryCatch({
        install.packages(package)
        suppressPackageStartupMessages( do.call(library, list(package)) )
      }, error = function(e) {
      })
    }
  }
}


#### Population counts ####

pop_pyramid_calculation = function(data,country_name,year_min=NA,year_max=NA){
  
  if(is.na(country_name) | is.null(data)){
    return(print("Enter the genealogical data set and the country name"))
  } else{
    pop_final = NULL
    pop_final = data %>%
      filter(!(is.na(gender)),
             gender!="unknown",
             country_birth_final==country_name) %>%
      mutate(years = purrr::map2(birth_year,death_year,~seq(.x,.y,by=1))) %>%
      unnest(years) %>%
      filter(years>=year_min,
             years<=year_max) %>%
      mutate(age = years-birth_year,
             age_class = case_when(age==0 ~ "0",
                                   age>=1 & age<=4 ~ "1-4",
                                   
                                   age>=5 & age<=9 ~ "5-9",
                                   age>=10 & age<=14 ~ "10-14",
                                   age>=15 & age<=19 ~ "15-19",
                                   age>=20 & age<=24 ~ "20-24",
                                   age>=25 & age<=29 ~ "25-29",
                                   age>=30 & age<=34 ~ "30-34",
                                   age>=35 & age<=39 ~ "35-39",
                                   age>=40 & age<=44 ~ "40-44",
                                   age>=45 & age<=49 ~ "45-49",
                                   age>=50 & age<=54 ~ "50-54",
                                   age>=55 & age<=59 ~ "55-59",
                                   age>=60 & age<=64 ~ "60-64",
                                   age>=65 & age<=69 ~ "65-69",
                                   age>=70 & age<=74  ~ "70-74",
                                   age>=75 & age<=79 ~ "75-79",
                                   age>=80  ~ "80+"
             )) %>%
      mutate(age_class = factor(age_class, levels = c("0","1-4","5-9","10-14","15-19",
                                                      "20-24","25-29","30-34","35-39",
                                                      "40-44","45-49","50-54","55-59",
                                                      "60-64","65-69","70-74","75-79",
                                                      "80+")),
             gender = factor(gender,levels = c("male","female"))) %>%
      group_by(years,age_class,gender,.drop=FALSE) %>%
      dplyr::tally() %>%
      #complete(years,age_class,gender, fill = list(n = 0)) %>%
      mutate(country=country_name) %>%
      pivot_wider(names_from = "gender", values_from = "n") %>%
      mutate(total = male+female) %>%
      pivot_longer(!c("years","country","age_class"),names_to = "gender", values_to = "counts")
    return(pop_final)
    
  }
  
}



# absolute population counts by year, sex and country


#### Population exposure ####


pop_exposure = function(pop){
  if(is.null(pop)){
    return(print("Return population pyramid"))
  } else{
    data_exp = pop %>%
      ungroup() %>%
      group_by(age_class,country,gender) %>%
      mutate(expos = (counts+lag(counts))/2)
    return(data_exp)
  }
  
}

# population exposure by year, sex and country


#### Death counts ####

deaths_calculation = function(data_death,country_name,year_min=NA,year_max=NA){
  if(is.na(country_name) | is.null(data)){
    return(print("Enter the genealogical data set and the country name"))
  } else{
    deaths = NULL
    deaths = data_death %>%
      filter(country_death_final==country_name,
             death_year>=year_min,
             death_year<=year_max,
             !(is.na(gender))) %>%
      rename(country=country_death_final) %>%
      mutate(age_death = death_year-birth_year) %>%
      mutate(age_class = case_when(age_death==0 ~ "0",
                                   age_death>=1 & age_death<=4  ~ "1-4",
                                   age_death>=90 ~ "90+",
                                   age_death>=5 & age_death<=9 ~ "5-9",
                                   age_death>=10 & age_death<=14 ~ "10-14",
                                   age_death>=15 & age_death<=19 ~ "15-19",
                                   age_death>=20 & age_death<=24 ~ "20-24",
                                   age_death>=25 & age_death<=29 ~ "25-29",
                                   age_death>=30 & age_death<=34 ~ "30-34",
                                   age_death>=35 & age_death<=39 ~ "35-39",
                                   age_death>=40 & age_death<=44 ~ "40-44",
                                   age_death>=45 & age_death<=49 ~ "45-49",
                                   age_death>=50 & age_death<=54 ~ "50-54",
                                   age_death>=55 & age_death<=59 ~ "55-59",
                                   age_death>=60 & age_death<=64 ~ "60-64",
                                   age_death>=65 & age_death<=69 ~ "65-69",
                                   age_death>=70 & age_death<=74  ~ "70-74",
                                   age_death>=75 & age_death<=79 ~ "75-79",
                                   age_death>=80 & age_death<=84 ~ "80+"
      )) %>%
      mutate(age_class = factor(age_class, levels = c("0","1-4","5-9","10-14","15-19",
                                                      "20-24","25-29","30-34","35-39",
                                                      "40-44","45-49","50-54","55-59",
                                                      "60-64","65-69","70-74","75-79",
                                                      "80+"))) %>%
      group_by(country,age_class,death_year,gender) %>%
      dplyr::tally() %>%
      pivot_wider(names_from = "gender", values_from = "n") %>%
      mutate(male = ifelse(is.na(male),0,male),
             female = ifelse(is.na(female),0,female)) %>%
      mutate(total = female+male) %>%
      mutate(country=country_name) %>%
      pivot_longer(!c("death_year","country","age_class"),names_to = "gender", values_to = "deaths") %>%
      dplyr::select(country,years=death_year,age_class,gender,deaths)
    return(deaths)
  }
}


# number of deaths by year, sex and country

#### Moving-average ####

mean_lag = function(x,k){
  if(length(x)>(k-1)){
    z = sapply(k:length(x),function(i) mean(x[i-0:(k-1)]))
    z = c(rep(NA,k-1),z)
  }
  else{
    z = rep(NA,length(x))
  }
  return(z)
}

# applies a moving average of order k to a vector of counts




#### Singular-Value Decomposition on Human Fertility Collection Data ####

svd_fertility = function(country_names){
  wd = "~/Library/Mobile Documents/com~apple~CloudDocs/PHD STAT/PhD Dissertation/Data/Human Fertility Collection/"
  matrix_overall = numeric()
  matrix_prop = numeric()
  for(i in 1:length(country_names)){
    matrix_temp = read.csv(paste0(wd,country_names[i],"_ASFR.txt")) %>%
      select(years=Year1,Age,ASFR) %>%
      filter(years<=1900,Age<50,Age>14) %>%
      group_by(years,Age) %>%
      mutate(ASFR=as.numeric(ASFR)) %>%
      summarise(ASFR = mean(ASFR)) %>%
      ungroup() %>%
      group_by(years) %>%
      mutate(TFR=sum(ASFR)) %>%
      mutate(age_class = case_when(Age>=15 & Age<=19 ~ "15-19",
                                   Age>=20 & Age<=24 ~ "20-24",
                                   Age>=25 & Age<=29 ~ "25-29",
                                   Age>=30 & Age<=34 ~ "30-34",
                                   Age>=35 & Age<=39 ~ "35-39",
                                   Age>=40 & Age<=44 ~ "40-44",
                                   Age>=45 & Age<=49 ~ "45-49")) %>%
      group_by(years,age_class,TFR) %>%
      summarise(ASFR = mean(ASFR)) %>%
      mutate(phi = ASFR*5/TFR) %>%
      ungroup() %>%
      select(age_class,years,phi) %>%
      pivot_wider(names_from = "years",values_from = "phi") %>%
      select(-age_class) %>%
      as.matrix()
    matrix_overall = cbind(matrix_overall,matrix_temp)
  }
  matrix_prop = matrix_prop = matrix(NA,nrow(matrix_overall),ncol(matrix_overall))
  for(i in 1:nrow(matrix_overall)){
    for(j in 1:ncol(matrix_overall)){
      matrix_prop[i,j] = log(matrix_overall[i,j]/matrix_overall[1,j])
    }
  }
  # apply the singular value decomposition
  svd_results = svd(matrix_prop)
  X_new = matrix(NA,2,7)
  X_new[1,] = round(svd_results$u[,1],6)
  X_new[2,] = round(svd_results$u[,2],6)
  m_new = rowMeans(matrix_prop)
  
  names(m_new) = c("f15","f20","f25","f30","f35","f40","f45")
  svd.constants_new = list(m = m_new,X = X_new)
  return(svd.constants_new)
}

#### Child-Woman Ratio Calculations ####


CW_ratio_calculation = function(pop_data,true_infant_mortality,year_min=1751){
  if(is.null(pop_data) | is.null(true_infant_mortality)){
    return(print("Enter population counts and infant mortality data"))
  } else{
    CW_data = pop_data %>%
      filter(age_class %in% c("0","1-4"),
             gender == "total",
             years>=1741,
             years<=1900) %>%
      select(-gender) %>%
      group_by(years,country) %>%
      dplyr::summarize(C=sum(counts)) %>%
      ungroup() %>%
      group_by(country) %>%
      mutate(C_smooth = mean_lag(C,10)) %>%
      left_join(pop_data  %>%
                  dplyr::filter(age_class %in% c("15-19","20-24","25-29","30-34",
                                                 "35-39","40-44","45-49","50-54"),
                                gender=="female",years>=1742,years<=1900) %>%
                  mutate(age_class = paste0("W",substr(age_class,1,2))) %>%
                  ungroup() %>%
                  dplyr::select(-gender,-counts) %>%
                  pivot_wider(names_from = "age_class",values_from="expos") %>%
                  mutate(W=W15+W20+W25+W30+W35+W40+W45) %>%
                  group_by(country) %>%
                  mutate(W_smooth = mean_lag(W,10),
                         W15_smooth = mean_lag(W15,10),
                         W20_smooth = mean_lag(W20,10),
                         W25_smooth = mean_lag(W25,10),
                         W30_smooth = mean_lag(W30,10),
                         W35_smooth = mean_lag(W35,10),
                         W40_smooth = mean_lag(W40,10),
                         W45_smooth = mean_lag(W45,10)),by=c("years","country")) %>%
      filter(years>=year_min) %>%
      mutate(CW = C/W,
             CW_smooth = C_smooth/W_smooth) %>%
      left_join(true_infant_mortality,by=c("years","country"))
  }
  return(CW_data)
}


