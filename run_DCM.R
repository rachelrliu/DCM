library(lfe)

createWithinGroupMs <- function(df){
      dfNested <- df %>% 
            group_by(WeekEnding,Geography,Channel,Attr4)%>%
            mutate(log_Ms_Conditional = log(Ms/sum(Ms)))%>% #nested conditional ms
            ungroup() 
      
      return(dfNested)
      
}



runModel <- function(dfNested) {
      cate <- unique(dfNested$Category)
      results <- data.frame(Estimate = rep(NA,6),pval = rep(NA,6))
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
      
      results['beta_Price','Estimate'] = m$coefficients[1]
      results['beta_PACV_FeatWODisp','Estimate'] = m$coefficients[2]
      results['beta_PACV_DispWOFeat','Estimate'] = m$coefficients[3]
      results['beta_PACV_FeatAndDisp','Estimate'] = m$coefficients[4]
      results['beta_ACV','Estimate'] = m$coefficients[5]
      results['sigma','Estimate'] = m$coefficients[6]
      
      results['beta_Price','pval'] = m$pval[[1]]
      results['beta_PACV_FeatWODisp','pval'] = m$pval[[2]]
      results['beta_PACV_DispWOFeat','pval'] = m$pval[[3]]
      results['beta_PACV_FeatAndDisp','pval'] = m$coefficients[[4]]
      results['beta_ACV','pval'] = m$coefficients[[5]]
      results['sigma','pval'] = m$coefficients[[6]]
      
      setwd("E:/Team Folder/After Midterm/Knowledge Transfer/Transferable Code")
      
      sink("output1.txt")
      summary(m)
      sink()
      
      return(results)
}




