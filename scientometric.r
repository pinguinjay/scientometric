library(bibliometrix)
files<-c("plaintxt_taiwanps_1.txt","plaintxt_taiwanps_2.txt","plaintxt_taiwanps_3.txt")
twps_df<-convert2df(file = files,dbsource = "wos",format = "plaintext")
netana<-biblioNetwork(#文獻共引分群
  twps_df,
  analysis = "co-citation",
  network = "references",
  n = NULL,
  sep = ";",
  short = FALSE,
  shortlabel = FALSE
  )
net=networkPlot(netana,Title = "reference co-citation network",type="fruchterman",cluster = "edge_betweenness",n=1133)
community_obj<-net[["community_obj"]]#取出分群的list
groupDF<-data.frame(Reference=community_obj$names,
                   WhichGroup=community_obj$membership)
#若沒裝套件要先裝
#install.packages("xlsx")
library(xlsx)
write.xlsx(
  groupDF,
  file="E:/group_full_test.xlsx",#儲存位置可以改
  sheetName = "Sheet1",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)
