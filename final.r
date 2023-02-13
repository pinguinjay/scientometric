library(bibliometrix)
files<-c("MedicalToursim1.bib","MedicalToursim2.bib","MedicalToursim3.bib","MedicalToursim4.bib")
medicaltourism_M<-convert2df(file=files,dbsource = "wos",format = "bibtex")

results <- biblioAnalysis(medicaltourism_M)
summary(results, k = 30, pause = FALSE)
plot(results)

##########作者發表年份數

topKW<-KeywordGrowth(medicaltourism_M,
                     Tag ="AU",
                     sep = ";",
                     top = 10,
                     cdf = TRUE)
library(reshape2)
library(ggplot2)
df<-melt(topKW,id="Year")
ggplot(df,aes(Year,value,group=variable,color=variable))+
  geom_line(size=1)

######作者關鍵字#######
library(bibliometrix)
author_keywords.cooccurrences.matrix<-biblioNetwork(medicaltourism_M,
             analysis = "co-occurrences",
             network = "author_keywords",
             sep = ";",
             remove.terms = c("MEDICAL TOURISM"))
author_keywords.cooccurrences.matrix.df<-as.data.frame.matrix(author_keywords.cooccurrences.matrix)
write.csv(author_keywords.cooccurrences.matrix.df,file = "作者關鍵字矩陣.csv")

#大於5的關鍵字
#install.packages("OpenMx")
library(OpenMx)
diagonal_number<-as.data.frame(diag2vec(author_keywords.cooccurrences.matrix.df))
new_matrix<-cbind(author_keywords.cooccurrences.matrix.df,diagonal_number)
new_matrix_filtering<-subset(new_matrix,new_matrix[,dim(new_matrix)[2]]>=5)
new_matrix_filtering_t<-t(new_matrix_filtering[,-dim(new_matrix_filtering)[2]])
new_matrix_filtering_t_cbind<-cbind(new_matrix_filtering_t,diagonal_number)
new_matrix_filtering_t_cbind_select<-subset(new_matrix_filtering_t_cbind[,-dim(new_matrix_filtering_t_cbind)[2]]>=5)
matrix_final<-new_matrix_filtering_t_cbind_select[,]
#######
M_CS<-conceptualStructure(#策略座標圖
  medicaltourism_M,
  field = "DE",
  ngrams = 4,
  method = "MCA",
  quali.supp = NULL,
  quanti.supp = NULL,
  minDegree = 10,
  clust = 6, #2~8 or "auto" #原本4
  k.max = 6, #min=5 max=20 #原本5
  stemming = TRUE,
  labelsize = 10,
  documents = 2,
  graph = TRUE
)


######主題演化########
library(bibliometrix)
## Not run: 
data(scientometrics, package = "bibliometrixData")
years=c(2005,2011,2015,2019)

nexus <- thematicEvolution(scientometrics,field="ID",years=c(2000),n=100,minFreq=2)

plotThematicEvolution(nexus$Nodes,nexus$Edges)
res <- thematicMap(scientometrics, field = "ID", n = 250, minfreq = 5, size = 0.5, repel = TRUE)
plot(res$map)
## End(Not run)
YEARS<-c(2002,2006,2011,2016)
nexus<-thematicEvolution(
  medicaltourism_M,
  field = "ID",
  years=YEARS,
  n = 250,
  minFreq = 2,
  size = 0.5,
  ngrams = 1,
  stemming = FALSE,
  n.labels = 1,
  repel = TRUE,
  remove.terms = c("medical tourism","medical","tourism","health","study"),
  synonyms = NULL,
  cluster = "leading_eigen"
)
plotThematicEvolution(nexus$Nodes,nexus$Edges)

#
Years=c(2007,2011,2013,2015,2018)
list_df<-timeslice(medicaltourism_M,breaks = Years)
for (i in 1:5) {
  assign(paste0("M", i), list_df[[i]])
}

pre.evolution<-timeslice(medicaltourism_M,breaks = c(2007))
m1<-pre.evolution[[2]]
evolution<-termExtraction(m1,
                          Field = "DE",
                          remove.terms=c("medical tourism","medical","tourism","health","study"))
topKW<-KeywordGrowth(evolution,Tag = "DE_TM",sep=";",top = 25)
library(reshape2)
library(ggplot2)
library(scales)
DF<-melt(topKW,id="Year")
p <- ggplot(DF, aes(x = Year, y = variable, fill = value)) +
  geom_tile() +
  scale_x_continuous(breaks = seq(2010,2020,by=(2))) +
  scale_fill_gradient2(low = muted("blue"), mid = "yellow", high = muted("#FF0000"), midpoint = 23, limits = c(0, 45))
p


######不要的引證圖 用mainpath######
histResults<-histNetwork(M, min.citations = 1, 
                         sep = ";", 
                         network = TRUE, 
                         verbose = TRUE)
histPlot(
  histResults,
  n = 29,
  size = 5,
  labelsize = 5,
  title_as_label = FALSE,
  label = "short",
  verbose = TRUE
)

NetMatrix<-biblioNetwork(  
  M,  
  analysis = "co-occurrences",  
  network = "keywords",  
  n = 1000,  
  sep = ";",  
  short = TRUE,  
  shortlabel = TRUE,  
  remove.terms = NULL,  
  synonyms = NULL
)
net<-networkPlot(
  NetMatrix,  
  normalize = NULL,  
  n = 70,  
  degree = NULL,  
  Title = "keywords co-occurrences Plot",  
  type = "fruchterman",  
  label = TRUE,  
  labelsize = 1,
  label.cex = FALSE,  label.color = FALSE,
  label.n = NULL,
  halo = FALSE,
  cluster = "optimal",
  community.repulsion = 0.1,
  vos.path = NULL,
  size = 3,
  size.cex = FALSE,
  curved = FALSE,
  noloops = TRUE,
  remove.multiple = TRUE,
  remove.isolates = TRUE,
  weighted = NULL,
  edgesize = 1,
  edges.min = 0,
  alpha = 0.5,
  verbose = TRUE
)

