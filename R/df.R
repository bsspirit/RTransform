# data.frame 基于某列，进行排序
#' @export
order_df<-function(df,col,decreasing=FALSE){
  df[order(df[,c(col)],decreasing=decreasing),]
}

# 用df2中匹配的值替换df1中值，返回df1
#' @export
replace_df<-function(df1,df2,keys,vals){
  row1<-which(apply(mapply(match,df1[,keys],df2[,keys])>0,1,all))
  row2<-which(apply(mapply(match,df2[,keys],df1[,keys])>0,1,all))
  df1[row1,vals]<-df2[row2,vals]
  return(df1)
}