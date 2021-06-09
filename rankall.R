rankall <- function(outcome, num = "best") {
      ## Read outcome data
      hospitalfile <- read.csv("outcome-of-care-measures.csv")
      
      ## Check that state and outcome are valid
      outcomelist<-c("heart attack", "heart failure", "pneumonia")
      test2<-1
      
      for(i in 1:length(outcomelist)){
            if(outcome==outcomelist[i]){
                  test2<-2
                  break
            }
      }
      
      if(test2==1){
            stop("invalid outcome", call.=TRUE)
      }
      
      ## For each state, find the hospital of the given rank
      
      statelist_1<-as.factor(hospitalfile$State)
      statelist<-levels(statelist_1)
      new_df<-data.frame(statelist)
      if(outcome=="heart attack"){
            colnumber<-11
      }
      else if(outcome=="heart failure"){
            colnumber<-17
      }
      else if(outcome=="pneumonia"){
            colnumber<-23
      }
      
      state<-character(0)
      hospital<-character(0)
      
      for(i in 1:length(statelist)){
            data2<-filter(hospitalfile, hospitalfile$State==statelist[i])
            data2[,colnumber]<-as.numeric(data2[,colnumber])
            data_3<-data2[!is.na(data2[,colnumber]), ]
            
            if (num=="best"){
                  rank<-1
            }
            else if(num=="worst"){
                  rank<-length(data_3[,colnumber])
                  #print(rank)
            }
            else{
                  rank<-as.numeric(num)
            }
            
            data3<-data_3[order( data_3[,colnumber], data_3$Hospital.Name ),]
            state[i]<-statelist[i]
            hospital[i]<-data3$Hospital.Name[rank]
      }

      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      ranking_list<-cbind(hospital, state, row.names=NULL)
      ranking_list
}