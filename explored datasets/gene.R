#reading in genomic data
#using read.table
gene<-read.table('genomic.tsv')

str(gene)
#changing colnames of dataset
colnames(gene)<-seq(1:64)


library(reshape2)
#reshaping gene from wide format to long format using melt of reshapeR
gene_long.samp<-melt(as.matrix(gene[ 1:200 , ])) 
names(gene_long.samp)<-c('gene','case','value')


#Generating a Heat Map
ggplot(aes(x = case , y = gene, fill=value) , data=gene_long.samp ) + 
  geom_tile()  + 
  #gradientn for creating n color gradient
  #colours of specifying the colours to use for the gradient
  scale_fill_gradientn(colours = colorRampPalette(c('green','blue'))(100))

