setwd('/Users/yanjiacheng/Desktop/毕业数据/OTU等数据信息/')
##初始导包，后续有管道符，下包可识别，否则导入tidyverse或dplyr也行
library(igraph)
library(psych)

##读数，算rp，没能安装WGCNA
fotu = read.table("fOTU.txt",head=T,row.names=1) %>% t()
fsotu = read.table("fsOTU.txt",head=T,row.names=1) %>% t()
botu = read.table("bOTU.txt",head=T,row.names=1) %>% t()
bsotu = read.table("bsOTU.txt",head=T,row.names=1) %>% t()
fbotu = read.table("fbOTU.txt",head=T,row.names=1) %>% t()
fbsotu = read.table("fbsOTU.txt",head=T,row.names=1) %>% t()
#数据太多，psych的corr.test跑不动，主要它有多重校正。此外cor.test()和cor()是R自带函数，cor()只给出r，cor.test()给出r和p
#library(psych)
#occor = corr.test(otu,use="pairwise",method="spearman",adjust="fdr",alpha=.05)
#换Hmisc的rcorr，只能指定算法，用stats::p.adjust()/fdrci::fdr_od();qvalue::qvalue()/fdrtool::fdrtool()校正
library(Hmisc)
foccor <- rcorr(as.matrix(fotu), type = 'spearman')
fsoccor <- rcorr(as.matrix(fsotu), type = 'spearman')
boccor <- rcorr(as.matrix(botu), type = 'spearman')
bsoccor <- rcorr(as.matrix(bsotu), type = 'spearman')
fboccor <- rcorr(as.matrix(fbotu), type = 'spearman')
fbsoccor <- rcorr(as.matrix(fbsotu), type = 'spearman')
library(stats)#基础包不必导，常用BH和bonferroni，后者太严格谨慎使用
#f
foccor.r = foccor$r
foccor.p = p.adjust(foccor$P, method = 'BH', n = length(foccor$P))
foccor.r[foccor.p>0.05|abs(foccor.r)<0.6] = 0
#fs
fsoccor.r = fsoccor$r
fsoccor.p = p.adjust(fsoccor$P, method = 'BH', n = length(fsoccor$P))
fsoccor.r[fsoccor.p>0.05|abs(fsoccor.r)<0.6] = 0
#b
boccor.r = boccor$r
boccor.p = p.adjust(boccor$P, method = 'BH', n = length(boccor$P))
boccor.r[boccor.p>0.05|abs(boccor.r)<0.6] = 0
#bs
bsoccor.r = bsoccor$r
bsoccor.p = p.adjust(bsoccor$P, method = 'BH', n = length(bsoccor$P))
bsoccor.r[foccor.p>0.05|abs(bsoccor.r)<0.6] = 0
#fb
fboccor.r = fboccor$r
fboccor.p = p.adjust(fboccor$P, method = 'BH', n = length(fboccor$P))
fboccor.r[fboccor.p>0.05|abs(fboccor.r)<0.6] = 0
#fbs
fbsoccor.r = fbsoccor$r
fbsoccor.p = p.adjust(fbsoccor$P, method = 'BH', n = length(fbsoccor$P))
fbsoccor.r[fbsoccor.p>0.05|abs(fbsoccor.r)<0.6] = 0
##算网络
#f
figraph = graph_from_adjacency_matrix(foccor.r,mode="undirected",weighted=TRUE,diag=FALSE)
fbad.vs = V(figraph)[degree(figraph) == 0]
figraph = delete.vertices(figraph, fbad.vs)
figraph.weight = E(figraph)$weight
E(figraph)$weight = NA

fnum.edges = length(E(figraph)) # length(curve_multiple(igraph))
fnum.vertices = length(V(figraph))# length(diversity(igraph, weights = NULL, vids = V(igraph)))
fconnectance = edge_density(figraph,loops=FALSE)# 同 graph.density;loops如果为TRUE,允许自身环（self loops即A--A或B--B）的存在
faverage.degree = mean(igraph::degree(figraph))# 或者为2M/N,其中M 和N 分别表示网络的边数和节点数。
faverage.path.length = average.path.length(figraph) # 同mean_distance(igraph) # mean_distance calculates the average path length in a graph
fdiameter = diameter(figraph, directed = FALSE, unconnected = TRUE, weights = NULL)
fedge.connectivity = edge_connectivity(figraph)
fclustering.coefficient = transitivity(figraph)
fno.clusters = no.clusters(figraph)
fcentralization.betweenness = centralization.betweenness(figraph)$centralization
fcentralization.degree = centralization.degree(figraph)$centralization
fmodularity = cluster_fast_greedy(figraph,weights =NULL) %>% modularity(figraph,membership())
#fs
fsigraph = graph_from_adjacency_matrix(fsoccor.r,mode="undirected",weighted=TRUE,diag=FALSE)
fsbad.vs = V(fsigraph)[degree(fsigraph) == 0]
fsigraph = delete.vertices(fsigraph, fsbad.vs)
fsigraph.weight = E(fsigraph)$weight
E(fsigraph)$weight = NA

fsnum.edges = length(E(fsigraph))
fsnum.vertices = length(V(fsigraph))
fsconnectance = edge_density(fsigraph,loops=FALSE)
fsaverage.degree = mean(igraph::degree(fsigraph))
fsaverage.path.length = average.path.length(fsigraph) 
fsdiameter = diameter(fsigraph, directed = FALSE, unconnected = TRUE, weights = NULL)
fsedge.connectivity = edge_connectivity(fsigraph)
fsclustering.coefficient = transitivity(fsigraph)
fsno.clusters = no.clusters(fsigraph)
fscentralization.betweenness = centralization.betweenness(fsigraph)$centralization
fscentralization.degree = centralization.degree(fsigraph)$centralization
fsmodularity = cluster_fast_greedy(fsigraph,weights =NULL) %>% modularity(fsigraph,membership())
#b
bigraph = graph_from_adjacency_matrix(boccor.r,mode="undirected",weighted=TRUE,diag=FALSE)
bbad.vs = V(bigraph)[degree(bigraph) == 0]
bigraph = delete.vertices(bigraph, bbad.vs)
bigraph.weight = E(bigraph)$weight
E(bigraph)$weight = NA

bnum.edges = length(E(bigraph))
bnum.vertices = length(V(bigraph))
bconnectance = edge_density(bigraph,loops=FALSE)
baverage.degree = mean(igraph::degree(bigraph))
baverage.path.length = average.path.length(bigraph) 
bdiameter = diameter(bigraph, directed = FALSE, unconnected = TRUE, weights = NULL)
bedge.connectivity = edge_connectivity(bigraph)
bclustering.coefficient = transitivity(bigraph)
bno.clusters = no.clusters(bigraph)
bcentralization.betweenness = centralization.betweenness(bigraph)$centralization
bcentralization.degree = centralization.degree(bigraph)$centralization
bmodularity = cluster_fast_greedy(bigraph,weights =NULL) %>% modularity(bigraph,membership())
#bs
bsigraph = graph_from_adjacency_matrix(bsoccor.r,mode="undirected",weighted=TRUE,diag=FALSE)
bsbad.vs = V(bsigraph)[degree(bsigraph) == 0]
bsigraph = delete.vertices(bsigraph, bsbad.vs)
bsigraph.weight = E(bsigraph)$weight
E(bsigraph)$weight = NA

bsnum.edges = length(E(bsigraph))
bsnum.vertices = length(V(bsigraph))
bsconnectance = edge_density(bsigraph,loops=FALSE)
bsaverage.degree = mean(igraph::degree(bsigraph))
bsaverage.path.length = average.path.length(bsigraph) 
bsdiameter = diameter(bsigraph, directed = FALSE, unconnected = TRUE, weights = NULL)
bsedge.connectivity = edge_connectivity(bsigraph)
bsclustering.coefficient = transitivity(bsigraph)
bsno.clusters = no.clusters(bsigraph)
bscentralization.betweenness = centralization.betweenness(bsigraph)$centralization
bscentralization.degree = centralization.degree(bsigraph)$centralization
bsmodularity = cluster_fast_greedy(bsigraph,weights =NULL) %>% modularity(bsigraph,membership())
#fb
fbigraph = graph_from_adjacency_matrix(fboccor.r,mode="undirected",weighted=TRUE,diag=FALSE)
fbbad.vs = V(fbigraph)[degree(fbigraph) == 0]
fbigraph = delete.vertices(fbigraph, fbbad.vs)
fbigraph.weight = E(fbigraph)$weight
E(fbigraph)$weight = NA

fbnum.edges = length(E(fbigraph))
fbnum.vertices = length(V(fbigraph))
fbconnectance = edge_density(fbigraph,loops=FALSE)
fbaverage.degree = mean(igraph::degree(fbigraph))
fbaverage.path.length = average.path.length(fbigraph) 
fbdiameter = diameter(fbigraph, directed = FALSE, unconnected = TRUE, weights = NULL)
fbedge.connectivity = edge_connectivity(fbigraph)
fbclustering.coefficient = transitivity(fbigraph)
fbno.clusters = no.clusters(fbigraph)
fbcentralization.betweenness = centralization.betweenness(fbigraph)$centralization
fbcentralization.degree = centralization.degree(fbigraph)$centralization
fbmodularity = cluster_fast_greedy(fbigraph,weights =NULL) %>% modularity(fbigraph,membership())
#fbs
fbsigraph = graph_from_adjacency_matrix(fbsoccor.r,mode="undirected",weighted=TRUE,diag=FALSE)
fbsbad.vs = V(fbsigraph)[degree(fbsigraph) == 0]
fbsigraph = delete.vertices(fbsigraph, fbsbad.vs)
fbsigraph.weight = E(fbsigraph)$weight
E(fbsigraph)$weight = NA

fbsnum.edges = length(E(fbsigraph))
fbsnum.vertices = length(V(fbsigraph))
fbsconnectance = edge_density(fbsigraph,loops=FALSE)
fbsaverage.degree = mean(igraph::degree(fbsigraph))
fbsaverage.path.length = average.path.length(fbsigraph) 
fbsdiameter = diameter(fbsigraph, directed = FALSE, unconnected = TRUE, weights = NULL)
fbsedge.connectivity = edge_connectivity(fbsigraph)
fbsclustering.coefficient = transitivity(fbsigraph)
fbsno.clusters = no.clusters(fbsigraph)
fbscentralization.betweenness = centralization.betweenness(fbsigraph)$centralization
fbscentralization.degree = centralization.degree(fbsigraph)$centralization
fbsmodularity = cluster_fast_greedy(fbsigraph,weights =NULL) %>% modularity(fbsigraph,membership())
#写出参数
f.netproperty = cbind(fnum.edges, fnum.vertices, 
                      fconnectance, faverage.degree, 
                      faverage.path.length, fdiameter, 
                      fedge.connectivity, fclustering.coefficient,
                      fno.clusters, fcentralization.betweenness, 
                      fcentralization.degree, fmodularity)
fs.netproperty = cbind(fsnum.edges, fsnum.vertices, 
                       fsconnectance, fsaverage.degree, 
                       fsaverage.path.length, fsdiameter, 
                       fsedge.connectivity, fsclustering.coefficient, 
                       fsno.clusters, fscentralization.betweenness, 
                       fscentralization.degree, fsmodularity)
b.netproperty = cbind(bnum.edges, bnum.vertices, 
                       bconnectance, baverage.degree, 
                       baverage.path.length, bdiameter, 
                       bedge.connectivity, bclustering.coefficient, 
                       bno.clusters, bcentralization.betweenness, 
                       bcentralization.degree, bmodularity)
bs.netproperty = cbind(bsnum.edges, bsnum.vertices, 
                       bsconnectance, bsaverage.degree, 
                       bsaverage.path.length, bsdiameter, 
                       bsedge.connectivity, bsclustering.coefficient, 
                       bsno.clusters, bscentralization.betweenness, 
                       bscentralization.degree, bsmodularity)
fb.netproperty = cbind(fbnum.edges, fbnum.vertices, 
                       fbconnectance, fbaverage.degree, 
                       fbaverage.path.length, fbdiameter, 
                       fbedge.connectivity, fbclustering.coefficient, 
                       fbno.clusters, fbcentralization.betweenness, 
                       fbcentralization.degree, fbmodularity)
fbs.netproperty = cbind(fbsnum.edges, fbsnum.vertices, 
                       fbsconnectance, fbsaverage.degree, 
                       fbsaverage.path.length, fbsdiameter, 
                       fbsedge.connectivity, fbsclustering.coefficient, 
                       fbsno.clusters, fbscentralization.betweenness, 
                       fbscentralization.degree, fbsmodularity)
netprop = t(cbind(t(f.netproperty),t(fs.netproperty),
               t(b.netproperty),t(bs.netproperty),
               t(fb.netproperty),t(fbs.netproperty)))

row.names(netprop) <- c('f','fs','b','bs','fb','fbs')
colnames(netprop) <- c('num.edges','num.vertices',
                    'connectance','average.degree',
                    'average.path.length','diameter',
                    'edge.connectivity','clustering.coefficient',
                    'no.clusters','centralization.betweenness',
                    'centralization.degree','modularity')
write.csv(netprop, file = 'netprop.csv', sep = ',') 

#怎么算模块个数？
#写出多行多列、指定行列名的表格怎么弄？

#画网路
#set.seed(100)
#plot(igraph,main="Co-occurrence network",vertex.frame.color=NA,vertex.label=NA,edge.width=1,
#     vertex.size=5,edge.lty=1,edge.curved=TRUE,margin=c(0,0,0,0))#根本画不动
