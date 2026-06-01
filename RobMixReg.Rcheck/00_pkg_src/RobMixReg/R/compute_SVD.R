
#BRCA compute SVD in real application example



#' Compute the row space using SVD.
#'
#' @param bulk_data The bulk data..
#' @param tg_R1_lists_selected A list of the marker genes for several cell types.
#' @return A matrix which each row span the row space using cell type specific marker genes.
Compute_Rbase_SVD <- function(bulk_data,tg_R1_lists_selected)
{
  tg_R1_lists_st_ccc<-tg_R1_lists_selected
  data_c<-bulk_data
  Base_all<-c()
  for(i in 1:length(tg_R1_lists_st_ccc))
  {
    tg_data_c<-data_c[tg_R1_lists_st_ccc[[i]],]
    cc<-svd(tg_data_c)$v[,1]
    ccc<-cor(cc,t(tg_data_c))
    if(mean(ccc)<0)
    {
      cc<--cc
    }
    Base_all<-rbind(Base_all,cc)
  }
  rownames(Base_all)<-1:nrow(Base_all)
  colnames(Base_all)<-colnames(bulk_data)
  if(length(names(tg_R1_lists_selected))>1)
  {
    rownames(Base_all)<-names(tg_R1_lists_selected)
  }
  return(Base_all)
}
