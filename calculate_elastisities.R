library(dplyr)
library(purrr) 
library(data.table)


expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


calculate_e <- function(beta,Brand_Geo.x,Brand_Geo.y,Channel.x,Channel.y,Attr4.x,Attr4.y,CdMs.x,Ms.x,Ms.y){
      if(Brand_Geo.x == Brand_Geo.y) # own elasticity
            return(beta*Ms.x *(1/(1-sigma) - sigma/(1-sigma)*CdMs.x - Ms.x))
      if(Brand_Geo.x != Brand_Geo.y & Channel.x == Channel.y & Attr4.x == Attr4.y)#within group elasticity
            return(-beta*Ms.y*(1/(1-sigma)*CdMs.x + Ms.x))
      
      else #outside group elasticity
            return(-beta*Ms.x*Ms.y)
      
}

calculateElasticity <- function(df,results){
      
      beta_Price = results$Estimate[1]
      beta_PACV_FeatWODisp = results$Estimate[2]
      beta_PACV_DispWOFeat = results$Estimate[3]
      beta_PACV_FeatAndDisp = results$Estimate[4]
      beta_ACV = results$Estimate[5]
      sigma = results$Estimate[6]
      
      
      byBrand <- df %>% group_by(Brand,Geography,Channel,Attr4) %>%
            summarise(Ms = median(Ms)) %>%
            ungroup()%>%
            group_by(Channel,Attr4) %>%
            mutate(CdMs = Ms/sum(Ms)) %>%
            ungroup() %>%
            mutate(Brand_Geo = paste(Geography,Brand))
      
      elasticity = expand.grid.df(byBrand,byBrand)
      
      
      e <- elasticity %>% 
            rowwise() %>%
            do({
                  cal_e <- partial(calculate_e,
                                   Brand_Geo.x = .$Brand_Geo.x,
                                   Brand_Geo.y = .$Brand_Geo.y,
                                   Channel.x = .$Channel.x,
                                   Channel.y = .$Channel.y,
                                   Attr4.x = .$Attr4.x,
                                   Attr4.y = .$Attr4.y,
                                   Ms.y = .$Ms.y,
                                   Ms.x = .$Ms.x,
                                   CdMs.x = .$CdMs.x)
                  
                  Price_e = cal_e(beta_Price)
                  Feature_e = cal_e(beta_PACV_FeatWODisp)
                  Disp_e = cal_e(beta_PACV_DispWOFeat)
                  FeatAndDisp_e = cal_e(beta_PACV_FeatAndDisp)
                  ACV_e = cal_e(beta_ACV)
                  
                  data.frame(Price_e = Price_e,
                             Feature_e = Feature_e,
                             Disp_e = Disp_e,
                             FeatAndDisp_e = FeatAndDisp_e,
                             ACV_e = ACV_e)
            })
      
      output <-  cbind(e,elasticity) %>%
            select(
                  Brand.x,
                  Geography.x,
                  Brand.y,
                  Geography.y,
                  Price_e,
                  Feature_e,
                  Disp_e,
                  FeatAndDisp_e ,
                  ACV_e
            )
      
      
      output_own <- filter(output,Brand.x == Brand.y & Geography.x == Geography.y)
      output_cross <- filter(output,Brand.x != Brand.y | Geography.x == Geography.y)
      
      
      setwd("E:/Team Folder/After Midterm/Visualization/Sample Output")
      fwrite(output_own,'output2.csv',row.names = FALSE)
      fwrite(output_cross,'output3.csv',row.names = FALSE)
}
