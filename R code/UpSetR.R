#Joy Buongiorno
#jbuongior21@gmail.com

library(UpSetR)
library(dplyr)

Transcripts_All<-read.csv("Transcripts_All.csv")
head(Transcripts_All)
nrow(Transcripts_All)
View(Transcripts_All)
upset(Trans_at_all_mapped_Main,sets = c("AB", "AC","F", "P"), keep.order=TRUE, order.by = "freq")

#Add COG info after putting transcribed_at_all.faa into Eggnog
Genes_with_trans<-as.data.frame(Transcripts_All$Gene, drop=FALSE)
names(Genes_with_trans)<-c("Gene")
head(Genes_with_trans)
nrow(Genes_with_trans)
Genes_mapped<-read.csv("Trans_at_all_map.csv")
head(Genes_mapped)
nrow(Genes_mapped)
Trans_at_all_mapped<-left_join(Genes_with_trans,Genes_mapped, by ="Gene")
head(Trans_at_all_mapped)
nrow(Trans_at_all_mapped)
#write.csv(Trans_at_all_mapped, "Trans_at_all_mapped.csv") #have to manually add a few. Also, translating letters to main COG categories, b/c some features were annotated with multiple letters within a main COG.

#Translate letters to main COG in Excel, then map to Transcripts_All
Trans_main<-read.csv("Trans_main.csv")

Trans_at_all_mapped_Main<-left_join(Transcripts_All,Trans_main, by ="Gene")
nrow(Trans_at_all_mapped_Main)
View(Trans_at_all_mapped_Main)
library(dplyr)
Identifier<-unite(Trans_at_all_mapped_Main, "Identifier", c(Gene, Main), sep="_", remove = TRUE) #Make identifier with both gene name and COG class
head(Identifier)
Trans_at_all_mapped_Main$Identifier<-Identifier$Identifier #Add that column to existing df 
row.names(Trans_at_all_mapped_Main)<-Trans_at_all_mapped_Main$Identifier #Make new column be row name, that way when we pull intersecting stuff out, it will have COG info already there

get_intersect_members <- function (x, ...){
  require(dplyr)
  require(tibble)
  x <- x[,sapply(x, is.numeric)][,0<=colMeans(x[,sapply(x, is.numeric)],na.rm=T) & colMeans(x[,sapply(x, is.numeric)],na.rm=T)<=1]
  n <- names(x)
  x %>% rownames_to_column() -> x
  l <- c(...)
  a <- intersect(names(x), l)
  ar <- vector('list',length(n)+1)
  ar[[1]] <- x
  i=2
  for (item in n) {
    if (item %in% a){
      if (class(x[[item]])=='integer'){
        ar[[i]] <- paste(item, '>= 1')
        i <- i + 1
      }
    } else {
      if (class(x[[item]])=='integer'){
        ar[[i]] <- paste(item, '== 0')
        i <- i + 1
      }
    }
  }
  do.call(filter_, ar) %>% column_to_rownames() -> x
  return(x)
}

AB_only<-get_intersect_members(Trans_at_all_mapped_Main, "AB") #Export these values, separate COG categories from the gene name and make pie charts
AC_only<-get_intersect_members(Trans_at_all_mapped_Main, "AC")
F_only<-get_intersect_members(Trans_at_all_mapped_Main, "F")
P_only<-get_intersect_members(Trans_at_all_mapped_Main, "P")

write.csv(AB_only, "AB_only.csv")
write.csv(AC_only, "AC_only.csv")
write.csv(F_only, "F_only.csv")
write.csv(P_only, "P_only.csv")


Intersect_all<-get_intersect_members(Trans_at_all_mapped_Main, "AB", "AC", "F", "P")
View(Intersect_all)
write.csv(Intersect_all, "Intersect_all.csv")

Debris_sites<-get_intersect_members(Trans_at_all_mapped_Main,"AC","P","F")
write.csv(Debris_sites, "Debris_sites.csv")
