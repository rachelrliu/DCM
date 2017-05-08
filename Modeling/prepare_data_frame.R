#### Prepare data frame for further anlaysis
##
##
## (1) Read Nielsen Data Files and Add External Variables
## (2) Select Category for Further Analysis 
## (3) Generate Other Variables from Nielsen Data

library(data.table)
library(plyr)
library(dplyr)
library(timeDate)
library(lubridate)
library(stringr)


########    (1) Read Nielsen Data Files and Add External Variables      ########
readNielsenData <- function(){
      "Read seperated Neilsen data files into a single dataframe"
      setwd("YOUR DIRECTORY")
      cat("Reading in Neilsen Data Files...\n")
      file_list <- list.files()
      nielsenData <- do.call("rbind",
                             lapply(file_list,
                                    FUN=function(files){fread(files,
                                                              header=TRUE, sep=",",
                                                              select = c(1:11,14:17,26))}))
      
      cat('Preprocessing Neilsen data...\n')
      
      #Reevaluate Attr4
      nielsenData$Attr4 <-  plyr::revalue(nielsenData$Attr4, 
                                          c('PREMIUM SALON'= 'PROFESSIONAL', 
                                            'REG ICE CREAM' = 'ICE CREAM',
                                            'YOGURT' = 'ICE CREAM',
                                            'DAIRY DESSERT' = 'ICE CREAM',
                                            "ICE BAR" = 'ICE CREAM',
                                            "ICE CRM CONE" = 'ICE CREAM',
                                            "ICE CRM BAR" = 'ICE CREAM',
                                            "ICE CRM SANDWICH" = 'ICE CREAM',
                                            'SHERBET' = 'GELATO',
                                            'SORBET' = 'GELATO',
                                            'ICE MILK' = 'ICE CREAM',
                                            'TOFU_SOY' = 'NON-DAIRY DESSERT-OTHER',
                                            "OTHER NON-DAIRY" = 'NON-DAIRY DESSERT-OTHER'
                                          ))
      
      
      
      nielsenData <- nielsenData %>%
            #Form categories
            #analyze ice cream category as a whole
            #subcategories inside Haircare are generated from Attr4 level,
            #subcategories inside other cateogories are generated from Attr3 level
            mutate(CategoryAttr3 = paste(Attr1,Attr2,Attr3,sep = '_'),
                   CategoryAttr4 = paste(Attr1,Attr2,Attr3,Attr4,sep = '_'),
                   Category = ifelse(Attr1 == 'GEN HAIRCARE',CategoryAttr4,
                                     ifelse(Attr1 == 'PKGD ICE CREAM',Attr1,CategoryAttr3))) %>%
            
            #match private label with retailer
            transform(Brand = ifelse(Brand == 'PRIVATE LABEL',paste(Geography,Brand),Brand)) %>%
            
            #format date
            transform(WeekEnding = as.Date(WeekEnding,format = '%Y-%m-%d'))%>%
            mutate(Year = lubridate::year(WeekEnding),
                   Month = lubridate::month( WeekEnding),
                   Week = lubridate::week(WeekEnding))
      
      cat("...Success!\n")
      return(nielsenData)
}


mergeWeather <- function(nielsenData){
      "Merge weather data into Neilsen data"
      setwd("YOUR DIRECTORY")
      
      cat('Reading in Weather data...\n')
      file_list <- list.files()
      weatherData <- do.call("rbind",
                             lapply(file_list,
                                    FUN=function(files){fread(files,
                                                              header=TRUE, 
                                                              sep=",",
                                                              select = 1:4)}))
      weather <- weatherData %>% 
            mutate(WeekEnding = as.Date(date,format = '%m/%d/%Y') + lubridate::days(1), #Statuaday -> Sunday
                   Year = lubridate::year(WeekEnding),
                   Month = lubridate::month(WeekEnding),
                   Week = lubridate::week(WeekEnding)) %>%
            group_by(Year,Month,Week) %>%
            summarize(Temp = mean(avgTemp),
                      Prcp = mean(prcp)) %>%
            ungroup()
      
      merged <- left_join(nielsenData,weather,by = c('Year','Month','Week'))
      
      cat('...Success!\n')
      
      return(merged)
}


mergeAds <- function(nielsenData){
      "Merge Ads data to Neilsen data"
      cat('Reading in Ad Image Data...\n')
      setwd("YOUR DIRECTORY")
      ads <- fread("MSBA_AdImageData.csv",header = TRUE)
      
      cat('Filtering matched retailers...\n')
      ads$Geography <- plyr::revalue(ads$Geography, 
                                     c("Winn Dixie"="WINN DIXIE", 
                                       "Publix Super Markets"="PUBLIX",
                                       "PUBLIX SUPER MARKETS" = "PUBLIX",
                                       "PUBLIX TOTAL CENSUS TRADING AREA" = "PUBLIX",
                                       "Kroger ATL" = "KROGER EX HT",
                                       "KROGER ATL" = "KROGER EX HT",
                                       "Rite Aid" = "RITE AID",
                                       "Dollar General" = "DOLLAR GENERAL",
                                       "Meijer" = "MEIJER",
                                       "Walgreens" = "WALGREENS",
                                       "Target Stores" = "TARGET",
                                       "TARGET STORES" = "TARGET",
                                       "BJ's Wholesale Club" = "BJS",
                                       "Sam's Club" = "SAMS",
                                       "Alb/Sfy TTL" = "ALB_SFWY-CORP",
                                       "Family Dollar" = "FAMILY DOLLAR",
                                       "FAMILY DOLLAR TOTAL CENSUS TA" = "FAMILY DOLLAR",
                                       "SAM'S CLUB" = "SAMS",
                                       "WALMART-US" = "WALMART",
                                       "Walmart-US" ="WALMART"
                                     ))
      
      cat('Extracting Ads page and Ads position...\n')
      pattern = "Page_[[:digit:]]+_[A-za-z]+_"
      ads$prom <- stringr::str_extract(ads$PromoImageName, pattern)
      
      ads <- ads %>% filter(Geography %in% unique(nielsenData$Geography))%>%
            transform(WeekEnding = as.Date(WeekEnding,format = '%Y-%m-%d'))%>%
            mutate(AdImage = strsplit(prom,'_')[[1]][2]) %>%
            select(Geography,
                   UPC,
                   WeekEnding,
                   AdImage) 
      
      cat('Merging Ads Data...\n')
      merged <- left_join(nielsenData,ads,by = c('UPC','Geography','WeekEnding')) %>%
            transform(AdImage = ifelse(is.na(AdImage),0,1))
      cat('...Success!\n')
      
      return(merged)
}


mergeBrandGoogleTrends <- function(nielsenData){
      "Merge Brand Google Trend data to Neilsen data"
      
      cat('Reading in Brand Google Trends...\n')
      setwd("YOUR DIRECTORY")
      file_list <- list.files()
      
      googleTrend <- do.call("rbind",
                             lapply(file_list,
                                    FUN=function(files){fread(files,
                                                              header=TRUE, sep=",")})) %>%
            transform(WeekEnding = as.Date(WeekEnding,format = '%m/%d/%Y'),
                      Value = as.numeric(ifelse(is.na(Value),0,Value)))%>%
            mutate(LagWeekEnding = WeekEnding + lubridate::weeks(3),
                   Year = lubridate::year(LagWeekEnding),
                   Week = lubridate::week(LagWeekEnding))%>%
            group_by(Year,Week,Category,Brand) %>%
            summarize(BrandGoogleTrend = mean(Value,na.rm = T)) %>%
            ungroup()
      
      
      
      cat('Precleaning...\n')
      googleTrend_byAttr2 <- filter(googleTrend,Category %in% c('SHAMPOO','HAIR STYLING','CONDITIONER')) 
      googleTrend_byAttr1 <- filter(googleTrend,!Category %in% c('SHAMPOO','HAIR STYLING','CONDITIONER'))
      
      
      cat('Merging Brand Level Google Trend Data...\n')
      merged <- left_join(nielsenData,googleTrend_byAttr2,by = c('Year' = 'Year',
                                                                 'Week' = 'Week',
                                                                 'Brand' = 'Brand',
                                                                 'Attr2' = 'Category'))
      
      merged<- left_join(merged,googleTrend_byAttr1,by = c('Year' = 'Year',
                                                           'Week' = 'Week',
                                                           'Brand' = 'Brand',
                                                           'Attr1' = 'Category'))
      
      
      merged$BrandGoogleTrend <-rowSums(merged[, c("BrandGoogleTrend.x", "BrandGoogleTrend.y")], na.rm=T)
      cat('...Success!\n')
      
      return(merged)
      
}


mergeRetailerGoogleTrends <- function(nielsenData){
      cat('Reading in Retailer Google Trends...\n')
      setwd("YOUR DIRECTORY")
      
      googleTrend <- read.csv('RetailerGoogleTrend.csv') %>%
            transform(Retailer = as.character(Retailer),
                      WeekEnding = as.Date(WeekEnding,format = '%m/%d/%Y')) %>%
            mutate(LagWeekEnding = WeekEnding + lubridate::weeks(3)) %>%
            rename(RetailerGoogleTrend = Value) %>%
            select(Retailer,RetailerGoogleTrend,LagWeekEnding)
      
      cat('Merging Retailer Google Trends...\n')
      merged <- left_join(nielsenData,googleTrend, by = c('WeekEnding' = 'LagWeekEnding',
                                                          'Geography' = 'Retailer'))
      cat('...Success!\n')
      return(merged)
}


mergeBrandFacebook <- function(nielsenData) {
      cat('Reading in Brand Facebook Posts...\n')
      setwd("YOUR DIRECTORY")
      file_list <- list.files()
      
      facebook <- do.call("rbind",
                          lapply(file_list,
                                 FUN=function(files){fread(files,
                                                           header=TRUE, sep=",")}))%>%
            transform(WeekEnding = as.Date(WeekEnding,format = '%m/%d/%Y')) %>%
            rename(BrandFacebook = Facebook_posts_brands)
      
      cat('Merging Brand Facebook Posts...\n')
      merged = left_join(nielsenData,facebook, by = c('WeekEnding', 'CategoryAttr3'='Category','Brand'))
      cat('...Success!\n')
      return(merged)
      
      
}


mergeRetailerFacebook <- function(nielsenData) {
      cat('Reading in Retailer Facebook Posts...\n')
      setwd("YOUR DIRECTORY")
      
      facebook <- read.csv('fbPosts_retailers.csv',stringsAsFactors = FALSE) %>%
            transform(WeekEnding = as.Date(WeekEnding,format = '%m/%d/%Y')) %>%
            rename(RetailerFacebook = Facebook_posts_retailers) %>%
            distinct()
      
      cat('Merging Retailer Facebook Posts...\n')
      merged <- left_join(nielsenData,facebook, by = c('WeekEnding' = 'WeekEnding',
                                                       'Geography' = 'Geography'))
      cat('...Success!\n')
      return(merged)
}


mergeOtherVariables <- function(nielsenData){
      "Merge Other variables"
      cat('Reading in Channel data...\n')
      
      #-----
      setwd("YOUR DIRECTORY")
      channel <- read.csv('channel.csv')
      cat('Merging Channel data...\n')
      merged1 <- left_join(nielsenData,channel,by = 'Geography')
      cat('...Success!\n')
      
      #-----
      cat('Reading in Holiday data...\n')
      holiday <- read.csv('holidays.csv')
      holiday <- transform(holiday, WeekEnding = as.Date(WeekEnding,format = '%m/%d/%Y'))
      merged2 <- left_join(merged1,holiday,by = 'WeekEnding')
      cat('...Success!\n')
      
      #-----
      cat('Reading in Monthly data...\n')
      monthly <- read.csv('monthly_data.csv')
      cat('Merging Monthly data...\n')
      merged3 <- left_join(merged2,monthly, by = c('Year','Month'))
      cat('...Success!\n')
      
      #-----
      cat('Reading in Google Finance data...\n')
      setwd('E:/Team Folder/After Midterm/External Data/varied_by_week')
      gfinance <- read.csv('google_finance.csv')
      
      cat('Merging Google Finance data...\n')
      merged4 <- left_join(merged3,gfinance,by = c('Year','Week'))
      cat('Success!\n')
      #----
      cat('Reading Dow Jone Index...\n')
      dowJone <- read.csv('DowJonesIndustrialAverage (daily).csv',stringsAsFactors = FALSE) %>%
            transform(Date = as.Date(Date,format = '%m/%d/%Y'))%>%
            mutate(Year = year(Date),
                   Week = week(Date)) %>%
            group_by(Year,Week) %>%
            summarize(DowJones = mean(Close))
      cat('Merging Dow Jone Index...\n')
      merged5 <- left_join(merged4,dowJone,by = c('Year','Week'))
      cat('...Success!\n')
      
      #----
      cat('Reading US Dollar Index...\n')
      usDollar <- read.csv('USDollarIndex (daily).csv',stringsAsFactors = FALSE) %>%
            transform(Date = as.Date(Date,format = '%m/%d/%Y'))%>%
            mutate(Year = year(Date),
                   Week = week(Date)) %>%
            group_by(Year,Week) %>%
            summarize(USDIndex = mean(Close))
      cat('Merging US Doolar Index...\n')
      merged6 <- left_join(merged5,usDollar,by = c('Year','Week'))
      cat('...Success!\n')
      
      #----
      cat('Reading Initial Jobless claim...\n')
      jobless <- read.csv('initial_jobless_claims.csv',stringsAsFactors = FALSE) %>%
            transform(DATE = as.Date(DATE,format = '%m/%d/%Y'))%>%
            mutate(Year = year(DATE),
                   Week = week(DATE)) 
      cat('Mering Initial Jobless claim...\n')
      merged7 <- left_join(merged6,jobless,by = c('Year','Week'))
      cat('...Success!\n')
      
      cat('Final cleaning...\n') #for security
      final <- distinct(merged7)
      cat('...Success!')
      
      return(final)
      
}


readFile <- function(){
      nielsenData <- readNielsenData()
      mergedAds <- mergeAds(nielsenData)
      mergedWeather <- mergeWeather(mergedAds)
      mergedBrandGoogleTrends <- mergeBrandGoogleTrends(mergedWeather)
      mergedRetailerGoogleTrends <- mergeRetailerGoogleTrends(mergedBrandGoogleTrends)
      mergedBrandFacebook <- mergeBrandFacebook(mergedRetailerGoogleTrends)
      mergedRetailerFacebook <- mergeRetailerFacebook(mergedBrandFacebook)
      mergedOtherVariables<- mergeOtherVariables(mergedRetailerFacebook)
      
      
      return(mergedOtherVariables)
      
}



########    (2) Select Category and Top Products for further analysis       ##########

selectCategory <- function(neilsenData){
      "Promt users to select one category from Neilson data for analysis"
      user_input <- ''
      
      cat('Preprocessing Neilson data....\n')
      
      categoryList <- unique(neilsenData$Category)
      #In this case, we only focus on 4 categories
      pattern = "^DEODORANT_|^PERSONAL WASH|^GEN HAIRCARE|^PKGD ICE CREAM"
      
      availableCategoryList <- categoryList[grep(pattern,categoryList)]
      
      
      while(user_input %in% availableCategoryList == FALSE){
            cat('Please enter the category you want to analyze from the list below\n')
            cat(availableCategoryList,sep = '\n')
            user_input <- readline('Enter category name here: ')
      }
      
      selectedCategory <- filter(neilsenData,Category == user_input)
      cat(paste('Category',user_input,'selected!',sep = ' '))
      return(selectedCategory)
}





filterTopUPC <- function(selectedCategory){
      "filter out brands/upcs that make up 80% of the total category share(remove the long tail)"
      topUPCs <- selectedCategory %>% 
            filter(Geography == 'US-XAOC') %>%
            group_by(WeekEnding) %>%
            mutate(share = Dollars/sum(Dollars)) %>% #weekly market share per UPC
            arrange(WeekEnding,desc(share)) %>%
            mutate(cumShares = cumsum(share)) %>%
            ungroup() %>%
            filter(cumShares <= 0.80)
      
      #filter out top UPC for every retailer while keep all UPCs in US-XAOC for market share calculation later
      selectedCategory_topUPC <- filter(selectedCategory, 
                                        (selectedCategory$UPC %in% topUPCs$UPC & Geography != 'US-XAOC')|
                                              (Geography == 'US-XAOC'))
      
      return(selectedCategory_topUPC)
}


########    (3) Generate Other Variables from Nielsen Data      ##########

generateVaraibles <- function(selectedCategory){
      "Generate variables for analysis for selected category"
      cat('Creating variables for analysis...\n')
      
      
      #Clean and create new variables from neilson data
      
      #--Create Size variable
      cat('Creating variable Size....\n')
      size_pattern = "(\\.)?[[:digit:]]+(\\.[[:digit:]]{1,2})?(\\.)? ?OZ|(\\.)?[[:digit:]]+(\\.[[:digit:]]{1,2})?(\\.)? FLOZ|(\\.)?[[:digit:]]+(\\.[[:digit:]]{1,2})?(\\.)?CT|(\\.)?[[:digit:]]+(\\.[[:digit:]]{1,2})?(\\.)?POUND"
      selectedCategory$SizeAndMeasure<- stringr::str_extract(selectedCategory$DESCRIPTION, size_pattern)
      selectedCategory$Size <-  gsub('OZ|CT|POUND| FLOZ', '', selectedCategory$SizeAndMeasure)
      selectedCategory$Measure <- gsub("[[:digit:]]+(\\.[[:digit:]]{1,2})?", '',selectedCategory$SizeAndMeasure)
      
      
      #---Create new product and its carried week
      cat('Create new product related variables ...\n')
      
      NewUPC <- selectedCategory %>% filter(Geography == 'US-XAOC') %>% 
            group_by(UPC) %>% 
            summarise(startWeek = min(WeekEnding,na.rm = T)) %>%
            ungroup() %>%
            filter(startWeek > as.Date('2014-12-27') )
      
      
      selectedCategory <- left_join(selectedCategory,NewUPC,by = 'UPC') %>%
            mutate(NewCarriedWeek = ifelse(is.na(startWeek),0,(WeekEnding - startWeek)/7 +1))
      
      
      #---
      cat('Creating promotion and ACV related variables...\n')
      cleanedCategory <- selectedCategory%>%
            filter(!is.na(Size)) %>% #Remove products without Size characteristics
            #--Create Priceand ACV
            mutate(Price = Dollars / Units,
                   ACV = ifelse(is.na(ACV),0,ACV)) %>%
            
            #--Label Carried Week 
            group_by(Geography,UPC) %>%
            mutate(CarriedWeek = n()) %>%
            ungroup() %>%
            
            #--Regular Product or Not
            mutate(Regular = ifelse(CarriedWeek == 104,1,0)) %>%
            
            #--Create Change in ACV
            arrange(Geography,UPC, WeekEnding) %>%
            group_by(Geography,UPC) %>%
            mutate(ChangeACV = ifelse(is.na(lag(ACV)),0, ACV - lag(ACV))) %>%
            ungroup()
      
      
      
      cat('Calculating weekly market share per UPC per retailer...\n')
      cat('Generaing utility based on market share...\n')
      
      weeklyTotalSales <- cleanedCategory %>% 
            filter(Geography == 'US-XAOC') %>%
            group_by(WeekEnding) %>%
            summarize(TotalDollars = sum(Dollars)) %>%
            ungroup()
      
      
      cleanedCategory_ms <- left_join(cleanedCategory,weeklyTotalSales, by = 'WeekEnding') %>%
            filter(!(Geography %in% c('US-XAOC','US-DRUG','US-FOOD','WALMART-SC','WALMART-NEIGHBOR',
                                      'WALMART-D1'))) %>%
            mutate(Ms = Dollars/TotalDollars) %>%
            group_by(WeekEnding) %>%
            mutate(OutsideMs = 1 - sum(Ms)) %>%
            ungroup() %>%
            mutate(y = log(Ms/OutsideMs)) %>%
            select(y,
                   Category,
                   WeekEnding,
                   Year,
                   Month,
                   Geography,
                   UPC,
                   DESCRIPTION,
                   Attr1,
                   Attr2,
                   Attr3,
                   Attr4,
                   Size,
                   Measure,
                   Brand,
                   Price,
                   PACV_FeatWODisp,
                   PACV_DispWOFeat,
                   PACV_FeatAndDisp,
                   PACV_Discount,
                   ACV,
                   ChangeACV,
                   Ms,
                   OutsideMs,
                   CarriedWeek,
                   NewCarriedWeek,
                   AdImage,
                   Temp,
                   Prcp,
                   Channel,
                   Holiday,
                   InterestRate,
                   FoodCPI,
                   GasCPI,
                   FuelCPI,
                   CCI,
                   DisposableIncome,
                   Google_Domestic_Trends_adverting_and_marketing,
                   Google_Domestic_Trends_air_travel,
                   Google_Domestic_Trends_business_._industrial,
                   Google_Domestic_Trends_credit_cards,
                   Google_Domestic_Trends_durable_goods,
                   Google_Domestic_Trends_finance_._investing,
                   Google_Domestic_Trends_financial_planning,
                   Google_Domestic_Trends_jobs,
                   Google_Domestic_Trends_luxury_goods,
                   Google_Domestic_Trends_mortgage,
                   Google_Domestic_Trends_real_estate,
                   Google_Domestic_Trends_shopping,
                   Google_Domestic_Trends_small_business,
                   Google_Domestic_Trends_travel,
                   Google_Domestic_Trends_unemployment,
                   USDIndex,
                   DowJones,
                   Initial_jobless_claims,
                   BrandGoogleTrend,
                   RetailerGoogleTrend,
                   BrandFacebook,
                   RetailerFacebook)
      
      cat('...Success!\n')        
      
      return(cleanedCategory_ms)     
}







