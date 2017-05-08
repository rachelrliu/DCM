main <- function(){
      dataFile <- readFile()
      
      
      selectedCategory <- selectCategory(dataFile)
      selectedCategory_topUPC <- filterTopUPC(selectedCategory)
      df <- generateVaraibles(selectedCategory_topUPC)
      cate <- unique(df$Attr1)
      if(cate == 'PKGD ICE CREAM') dfNested <- icecreamWithinGroupMs(df)
      if(cate == 'PERSONAL WASH')  dfNested <- pwWithinGroupMs(df)
      if(cate == 'DEODORANT') dfNested <- deoWithinGroupMs(df)
      if(cate == 'GEN HAIRCARE') dfNested <- hairWithinGroupMs(df)
      
      
      
      if(nrow(dfNested) >0 &  
         sum(is.na(dfNested$y)) == 0 &
         sum(is.infinite(dfNested$y)) == 0 &
         length(unique(dfNested$Geography)) > 1 &
         length(unique(dfNested$UPC)) > 1) {
            
            results <- runModel(dfNested)
            if(resultCheck(results)){
                  calculateElasticity(df,results)  
            }
      }
      else print('Not enough data for Modeling.')
}