best <- function(state, outcome) {
      ## Read outcome data
      hospitalfile <- read.csv("outcome-of-care-measures.csv")
      
      ## Check that state and outcome are valid
      statelist_1<-as.factor(hospitalfile$State)
      statelist<-levels(statelist_1)
      outcomelist<-c("heart attack", "heart failure", "pneumonia")
      test<-1
      test2<-1
      
      for(i in 1:length(statelist)){
            if(state==statelist[i]){
                  test<-2
                  break
            }
      }
      
      if(test==1){
            stop("invalid state", call.=TRUE)
      }
      
      for(i in 1:length(outcomelist)){
            if(outcome==outcomelist[i]){
                  test2<-2
                  break
            }
      }
      
      if(test2==1){
            stop("invalid outcome", call.=TRUE)
      }
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      if(outcome=="heart attack"){
            colnumber<-11
      }
      else if(outcome=="heart failure"){
            colnumber<-17
      }
      else if(outcome=="pneumonia"){
            colnumber<-23
      }
      data2<-filter(hospitalfile, hospitalfile$State==state)
      data2[,colnumber]<-as.numeric(data2[,colnumber])
      data_3<-data2[!is.na(data2[,colnumber]), ]
      min_value<-min(as.numeric(data_3[,colnumber]))
      data_4<-filter(data_3, data_3[,colnumber]==min_value)
      data5<-data_4[order(data_4[,2]),]
      ret_value<-data5$Hospital.Name[1]
      ret_value
}