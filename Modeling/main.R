main <- function(){
      dataFile <- readFile()
      
      
      selectedCategory <- selectCategory(dataFile)
      selectedCategory_topUPC <- filterTopUPC(selectedCategory)
      df <- generateVaraibles(selectedCategory_topUPC)
      
      
      
      if(nrow(dfNested) >0 &  
         sum(is.na(df$y)) == 0 &
         sum(is.infinite(df$y)) == 0 &
         length(unique(df$Geography)) > 1 &
         length(unique(df$UPC)) > 1) {
            
            cate <- unique(df$Attr1)
            if(cate == 'PKGD ICE CREAM') dfNested <- icecreamWithinGroupMs(df)
            if(cate == 'PERSONAL WASH')  dfNested <- pwWithinGroupMs(df)
            if(cate == 'DEODORANT') dfNested <- deoWithinGroupMs(df)
            if(cate == 'GEN HAIRCARE') dfNested <- hairWithinGroupMs(df)
      
            results <- runModel(dfNested)
            if(resultCheck(results)){
                  calculateElasticity(df,results)  
            }
      }
      else print('Not enough data for Modeling.')
}
