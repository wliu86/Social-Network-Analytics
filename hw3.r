library(igraph)
library(ggplot2)
library(progress)
df1<-read.csv('Funding_events_7.14.csv',stringsAsFactors = FALSE)
df2<-read.csv('Funding_events_7.14_page2.csv',stringsAsFactors = FALSE)
colnames(df2)=colnames(df1)

raw_data<-rbind(df1,df2)
july_data<-raw_data[which(substr(raw_data$Deal.Date,(nchar(raw_data$Deal.Date)-1),nchar(raw_data$Deal.Date))=='14' 
                          & substr(raw_data$Deal.Date,1,1)==7),]

  
  july_data$Investors<-paste0(july_data$Investors,',')
  text_list<-character()
  for(i in 1:length(july_data$Investors)) {
    temp_vector <- unlist(strsplit(july_data$Investors[i],split = ','))
    text_list<-append(text_list,temp_vector)
  }

july_data$Investors <- gsub(', ',',',july_data$Investors)
investor_list<-unique(text_list)
investor_list<-trimws(investor_list[!investor_list==''],'both')
investor_list<-sort(unique(investor_list))
company_list <- unique(july_data$Portfolio.Company.Name)
company_list<- company_list[!company_list=='']
aff_mat<-data.frame(matrix(nrow=(length(investor_list)+1),ncol=(length(company_list)+1),rep(0)))
aff_mat[1,2:ncol(aff_mat)]<-company_list
aff_mat[,1][2:nrow(aff_mat)] <- investor_list

for(i in 2:nrow(aff_mat)){
  investor_name<-paste0(aff_mat[i,1],',')
  for(j in 1:nrow(july_data)){
    investors_temp <- july_data$Investors[j]
    if(length(grep(investor_name,investors_temp))>0){
      col_num<-which(aff_mat[1,]==trimws(july_data$Portfolio.Company.Name[j],'both'))
      aff_mat[i,col_num]<-as.numeric(aff_mat[i,col_num])+1
    }
  }
}

aff_mat.num<-data.matrix(aff_mat[2:nrow(aff_mat),2:ncol(aff_mat)])
aff_mat.num.t<-t(aff_mat.num)

result<-aff_mat.num %*% aff_mat.num.t



#Check if there's multiple investment by the same investor to the same company
#for(i in 1:nrow(aff_mat.num)){
 # for(j in 1:ncol(aff_mat.num)){
  #  if(i==j){
   #   aff_mat.num=0
    #}
 # }
#}

adj_mat<-result
diag(adj_mat)=0
adj_mat[adj_mat>0]=1

g_adj<-graph.adjacency(adj_mat,'undirected')
plot.igraph(g_adj,vertex.label=NA,vertex.size=5)
dim<-length(V(g_adj))
dist_table<-matrix(nrow=dim,ncol=dim,rep(0))
dist_table[dist_table>0]<-0
for(i in 1:length(V(g_adj))){
  for(j in 1:length(V(g_adj))){
    dist_table[i,j] <- shortest.paths(g_adj,V(g_adj)[i],V(g_adj)[j])[1]
    if(dist_table[i,j]==Inf){
      dist_table[i,j]=0
    }
  }
}
mean(dist_table[dist_table>0])

#Q2
  edge_list[has_rep,4]<-current.month
                  edchange_date<-function(date){
  n<-nchar(date)
  two_dig<-as.numeric(substr(date,(n-1),n))
  
  if(two_dig<10){
    four_dig<-paste0('200',two_dig)
    
  } else if(two_dig>=10 && two_dig<=15){
    four_dig<-paste0('20',two_dig)
  } else {
    four_dig<-paste0('19',two_dig)
  }
  date_str<-paste0(substr(date,1,(n-2)),four_dig)
  return(date_str)
}
test.date<-character(nrow(raw_data))
for(i in 1:nrow(raw_data)){
  test.date[i]<-change_date(raw_data$Deal.Date[i])
}


raw_data.sorted<-raw_data
raw_data.sorted$Deal.Date<-as.Date(test.date,format = "%m/%d/%Y")
raw_data.sorted<-raw_data.sorted[order(raw_data.sorted$Deal.Date),]




unique.month<-unique(substr(raw_data.sorted$Deal.Date,1,7))
total.month<-length(unique.month)
keep.track<-0


create_month_df<-function(df,decay=FALSE){
    count_month<-0;
    month_cor_df<- data.frame('month'=0,'coreness'=0)
    raw_data.sorted<-df
    raw_data.sorted<-raw_data.sorted[order(raw_data.sorted$Deal.Date),]
    edge_list<-data.frame('x1'=character(),'x2'=character(),'x3'=numeric(),
                          'x4'=numeric(),'x5'=numeric())  
    #initial.date<-raw_data.sorted$Deal.Date[1]
    #last.date<-raw_data.sorted$Deal.Date[nrow(raw_data.sorted)]
    #initial.month = as.numeric(substr(initial.date,1,4))*12+as.numeric(substr(initial.date,6,7))
    #last.month = as.numeric(substr(last.date,1,4))*12+as.numeric(substr(last.date,6,7))
    investor_grand_list<-c()
    nraw<-nrow(raw_data)
    #pb = txtProgressBar(min = 0, max = nraw, initial = 0) 
    #pb1 <- progress_bar$new(total = nraw)
    
    while(nrow(raw_data.sorted)>0){
      keep.track<-nrow(raw_data.sorted)
      count_month<-count_month+1
      year_month<-substr(raw_data.sorted$Deal.Date[1],1,7)
      current.month<-12*as.numeric(substr(raw_data.sorted$Deal.Date[1],1,4))+as.numeric(substr(raw_data.sorted$Deal.Date[1],6,7))
      y_m_index<-which(substr(raw_data.sorted$Deal.Date,1,7)==year_month)
      year_month_df<-raw_data.sorted[y_m_index,]
      #edge_list<-data.frame('x1'=character(),'x2'=character())
      investor_month_list<-character()
      for(i in 1:nrow(year_month_df)){
        month_investor<-year_month_df$Investors[i]
        if(month_investor == ''){
          next()
        }
        month_investor.arr<-unlist(strsplit(month_investor,split = ','))
        month_investor.arr<-trimws(month_investor.arr,which = 'both')
        month_investor.arr<-unique(sort(month_investor.arr))
        investor_month_list<-c(investor_month_list,month_investor.arr)
        investor_month_list <- unique(sort(investor_month_list))
        for(j in 1:length(month_investor.arr)){
          
          for(k in 1: length(month_investor.arr)){
            if(j==k){
              next()
            }else {
              has_rep<-which(edge_list$x1==month_investor.arr[j] & edge_list$x2==month_investor.arr[k])
              has_rev<-which(edge_list$x1==month_investor.arr[k] & edge_list$x2==month_investor.arr[j])
              
              if(sum(has_rep)==0  && sum(has_rev)==0){
                temp_edge<-data.frame('x1'=month_investor.arr[j],'x2'=month_investor.arr[k],'x3'=count_month,
                                      'x4'=current.month,'x5'=0)
                edge_list<-rbind(edge_list,temp_edge)
              } else if(sum(has_rep)>0 && !edge_list[has_rep,4]==current.month){
                ge_list[has_rep,5]<-edge_list[has_rep,5]+1
                } else if (sum(has_rev>0) && !edge_list[has_rev,4]==current.month){
                    edge_list[has_rev,4]<-current.month
                    edge_list[has_rev,5]<-edge_list[has_rev,5]+1
                }
            }
          }
        }
        
      }
      investor_grand_list<-unique(sort(c(investor_grand_list,investor_month_list)))
      vertecies<-data.frame('v'=investor_grand_list)
      #if(decay){
      #keep<-which((current.month-edge_list$x4)<=120)
      #g_temp<-graph.data.frame(edge_list[keep,1:2],vertices = vertecies,directed = FALSE)
      #} else {
        g_temp<-graph.data.frame(edge_list[,1:2],vertices = vertecies,directed = FALSE)
      #}
      #plot.igraph(g_temp,vertex.lable=FALSE)
      #adj<-as.matrix(get.adjacency(g_temp,type = 'both'))
      mean_corness<-mean(coreness(g_temp,'all'))
      m_c.df<-data.frame('month'=current.month,'coreness'=mean_corness)
      month_cor_df<-rbind(month_cor_df,m_c.df)
      raw_data.sorted<-raw_data.sorted[-y_m_index,]
      #for(p in 1:pg){
       # pb1$tick()
        #Sys.sleep(1 / 1000)
      #}
      #setTxtProgressBar(pb,pg)
    }
    return(month_cor_df)
}

no.decay<-create_month_df(raw_data.sorted,FALSE)

jun1991.index<-which(substr(raw_data.sorted$Deal.Date,1,7)=='1991-06')
Jun1991<-raw_data.sorted[jun1991.index,]
jun_edge<-data.frame('x1'=character(),'x2'=character())
jun_investor_list<-character()

for(i in 1:nrow(Jun1991)){
  jun_investor<-Jun1991$Investors[i]
  if(jun_investor == ''){
    next()
  }
  jun_investor.arr<-unlist(strsplit(jun_investor,split = ','))
  jun_investor.arr<-trimws(jun_investor.arr,which = 'both')
  jun_investor.arr<-unique(sort(jun_investor.arr))
  jun_investor_list<-c(jun_investor_list,jun_investor.arr)
  jun_investor_list <- unique(sort(jun_investor_list))
  for(j in 1:length(jun_investor.arr)){
    
    for(k in 1: length(jun_investor.arr)){
      if(j==k){
        next()
      }else {
        has_rep<-which(jun_edge$x1==jun_investor.arr[j] & jun_edge$x2==jun_investor.arr[k])
        has_rev<-which(jun_edge$x1==jun_investor.arr[k] & jun_edge$x2==jun_investor.arr[j])
        
        if(sum(has_rep)==0  && sum(has_rev)==0){
          temp_edge<-data.frame('x1'=jun_investor.arr[j],'x2'=jun_investor.arr[k])
          jun_edge<-rbind(jun_edge,temp_edge)
        } 
      }
    }
  }
}
g_jun<-graph.data.frame(jun_edge,vertices = jun_investor_list, directed = FALSE)
jun_adj <- as.matrix(get.adjacency(g_jun))
jun_list<-list()
jun_list[[1]]<-jun_adj
for(i in 2:20){
  jun_list[[i]]=cor(jun_list[[i - 1]])
}
while(sum(which(abs(cor(jun_adj))!=1))>0){
  jun_adj<-cor(jun_adj)
} 
