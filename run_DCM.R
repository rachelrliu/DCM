#### Run DCM as of Nested Logit Model
##
##
##(1) Nest products based on category
##(2) run DCM
##
library(dplyr)
library(lfe)

################          (1) Nest products based on category         ##############
icecreamWithinGroupMs <- function(df){
      "calculate within group market share for ice cream"
      dfNested <- df %>% 
            group_by(WeekEnding,Geography,Channel,Attr2,Attr3,Attr4)%>%
            mutate(log_Ms_Conditional = log(Ms/sum(Ms)))%>% #nested conditional ms
            ungroup() 
      
      return(dfNested)
      
}

deoWithinGroupMs <- functoin(df){
      "calculate within group market share for ice cream"
      dfNested <- df %>% 
            group_by(WeekEnding,Channel,Attr4,Size)%>%
            mutate(log_Ms_Conditional = log(Ms/sum(Ms)))%>% #nested conditional ms
            ungroup() 
      
      return(dfNested)
}


hairWithinGroupMs <- functoin(df){
      "calculate within group market share for general haircare"
      df$SizeGroup <- cut(df$Size,c(-Inf,10,20,30,Inf))
      dfNested <- df %>% 
            group_by(WeekEnding,Channel,SizeGroup)%>%
            mutate(log_Ms_Conditional = log(Ms/sum(Ms)))%>% #nested conditional ms
            ungroup() 
      
      return(dfNested)
}


pwWithinGroupMs <- functoin(df){
      "calculate within group market share for personal wash"
      dfNested <- df %>% 
            group_by(WeekEnding,Channel,Attr4)%>%
            mutate(log_Ms_Conditional = log(Ms/sum(Ms)))%>% #nested conditional ms
            ungroup() 
      
      return(dfNested)
}



################               (2) Run DCM                ################
runModel <- function(dfNested) {
      "run DCM, write out stats summary, and return key estimates"
      
      cate <- unique(dfNested$Category)
      results <- data.frame(Estimate_temp = rep(NA,6),pval = rep(NA,6))
      rownames(results) <- c('beta_Price','beta_PACV_FeatWODisp','beta_PACV_DispWOFeat','beta_PACV_FeatAndDisp',
                             'beta_ACV','sigma')
      
      m <- felm(y ~ Price + PACV_FeatWODisp + PACV_DispWOFeat + PACV_FeatAndDisp  +
                      ACV  + log_Ms_Conditional + AdImage+ NewCarriedWeek + Holiday + Temp + Prcp  +
                      BrandGoogleTrend + RetailerGoogleTrend + BrandFacebook + RetailerFacebook +
                      Google_Domestic_Trends_adverting_and_marketing + 
                      Google_Domestic_Trends_air_travel + Google_Domestic_Trends_business_._industrial+
                      Google_Domestic_Trends_credit_cards + Google_Domestic_Trends_durable_goods+
                      Google_Domestic_Trends_finance_._investing +Google_Domestic_Trends_financial_planning+
                      Google_Domestic_Trends_jobs + Google_Domestic_Trends_luxury_goods +
                      Google_Domestic_Trends_mortgage + Google_Domestic_Trends_real_estate +
                      Google_Domestic_Trends_shopping + Google_Domestic_Trends_small_business +
                      Google_Domestic_Trends_travel + Google_Domestic_Trends_unemployment +
                      DowJones + USDIndex|Geography + UPC + Month, data = dfNested)
      
      results['beta_Price','Estimate_temp'] = m$coefficients[1]
      results['beta_PACV_FeatWODisp','Estimate_temp'] = m$coefficients[2]
      results['beta_PACV_DispWOFeat','Estimate_temp'] = m$coefficients[3]
      results['beta_PACV_FeatAndDisp','Estimate_temp'] = m$coefficients[4]
      results['beta_ACV','Estimate_temp'] = m$coefficients[5]
      results['sigma','Estimate_temp'] = m$coefficients[6]
      
      results['beta_Price','pval'] = m$pval[[1]]
      results['beta_PACV_FeatWODisp','pval'] = m$pval[[2]]
      results['beta_PACV_DispWOFeat','pval'] = m$pval[[3]]
      results['beta_PACV_FeatAndDisp','pval'] = m$coefficients[[4]]
      results['beta_ACV','pval'] = m$coefficients[[5]]
      results['sigma','pval'] = m$coefficients[[6]]
      
      results <- mutate(results, Estimate = ifelse(pval <= 0.05,Estimate_temp,0))
      setwd("E:/Team Folder/After Midterm/Knowledge Transfer/Transferable Code")
      
      sink("output1.txt")
      summary(m)
      sink()
      
      return(results)
}



resultCheck <- function(results){
      if(results[1,'Estimate'] >0 & results[2,'Estimate'] >=0 &
         results[3,'Estimate'] >= 0 & results[4,'Estimate'] >=0&
         results[5,'Estimate'] >=0 & results[6,'Estimate'] >=0){
            cat('Endogeneity problem arises. Please add instrument variables.')
            return(FALSE)
      }
      else return(TRUE)
}


