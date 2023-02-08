DEV_MODE = TRUE
library(crayon)

#function 0
Read_file<-function(){
  while(TRUE){
    tryCatch(
      {   
        print("Please select the database on which you wish to work")
        input_file<-read.csv(file.choose())
        break
      },
      error=function(cond) {
        if(DEV_MODE)
          cat(red(paste("E0: Invalid FILE OR ",cond,"\n")))
        else
          cat(red("E0: Invalid FILE\n"))
      }
    )
  }
  return(input_file)
}

# function 1
Message <- function(col_names){
  print("Which columns do you require to compute on ")
  for (i in 1:length(col_names)){
    print(paste("press   " , i ,"    For:     ",col_names[i]  ))
  }
  print("If you finished press 0")
}

# function 2
Message_opr <- function(){
  print("Enter the number of what you'd want to caclulate")
  cat("1.count\t\t2.Mean\t\t3.Std\n4.Min\t\t5.25%\t\t6.50%\n7.75%\t\t8.Max\t\t9.All\n")
  print("If you finished press 0")
}

# function 3
Found <- function(item_id,set){
  Flag <- FALSE 
  tryCatch(
    {   
      for(i in 1 : length(set) ){
        if (item_id == set[i]){
          Flag <- TRUE
          break
        }
      } 
    },error=function(cond) {
      if(DEV_MODE)
        cat(red(paste("E3: Invalid Input OR ",cond,"\n")))
      else
        cat(red("E3: Invalid Input\n"))
      return(NA)
    },warning=function(cond) {
      if(DEV_MODE)
        cat(red(paste("W3: Invalid Input OR ",cond,"\n")))
      else
        cat(red("W3: Invalid Input\n"))
      return(NA)
    }
  ) 
  return(Flag)
}

# function 4
col_chose <- function(col_names){
  col_set <- {}
  while(TRUE){
    tryCatch(
      {   
        Message(col_names)
        choice <- as.integer(readline())
        if(choice == 0){
          break
        }else{
          if(choice >= 1 && choice<= length(col_names)){
            if(length(col_set) == 0){
              col_set<-append(col_set,choice)
              print(paste("Input ",choice, " was added"))
            }else if(length(col_set) != 0 && !Found(choice,col_set)){
              col_set<-append(col_set,choice)
              print(paste("Input ",choice, " was added"))
            }else {
              print(paste("Item ",choice, " exist"))
            }
          }else
            cat(red("O4-1: Invalid Input (OUT OF RINGE)\n"))
        }
      },
      error=function(cond) {
        if(DEV_MODE)
          cat(red(paste("E4: Invalid Input OR ",cond,"\n")))
        else
          cat(red("E4: Invalid Input\n"))
      },
      warning=function(cond) {
        if(DEV_MODE)
          cat(red(paste("W4: Invalid Input OR ",cond,"\n")))
        else
          cat(red("W4: Invalid Input\n"))
      }
    ) 
  }
  return(col_set)
}

# function 5
opr_chose <- function(){
  opr_set <- {}
  while(TRUE){
    tryCatch(
      {   
        Message_opr()
        choice <- as.integer(readline())
        if(choice == 0){
          break
        }else if(choice == 9){
          opr_set <-{}
          opr_set <-append(opr_set,1:8)
          break
        }else{
          if(choice >= 1 && choice<= 8){
            if(length(opr_set) == 0){
              opr_set<-append(opr_set,choice)
              print(paste("Input ",choice, " was added"))
            }else if(length(opr_set)!= 0 && !Found(choice,opr_set)){
              opr_set<-append(opr_set,choice)
              print(paste("Input ",choice, " was added"))
            }
            else {
              print(paste("Item ",choice, " exist"))
            }
          }else
            cat(red("O5-1: Invalid Input (OUT OF RINGE)\n"))
        }
      },
      error=function(cond) {
        if(DEV_MODE)
          cat(red(paste("E5: Invalid Input OR ",cond,"\n")))
        else
          cat(red("E5: Invalid Input\n"))
      },
      warning=function(cond) {
        if(DEV_MODE)
          cat(red(paste("W5: Invalid Input OR ",cond,"\n")))
        else
          cat(red("W5: Invalid Input\n"))
      }
    ) 
  }
  return(opr_set)
}

# function 5
calculate<-function(amm,i){
  calc=NA
  is_char = FALSE
  if(is.character(amm))
    is_char = TRUE
  tryCatch(
    { 
      if(i==1){calc=length(amm)}
      else if(i==2 && !is_char){calc=mean(amm)}
      else if(i==3 && !is_char){calc=sd(amm)}
      else if(i==4 && !is_char){calc=min(amm)}
      else if(i==5 && !is_char){calc=quantile(amm,probs=c(0.25))}
      else if(i==6 && !is_char){calc=quantile(amm,probs=c(0.50))}
      else if(i==7 && !is_char){calc=quantile(amm,probs=c(0.75))}
      else if(i==8 && !is_char){calc=max(amm)}
    },
    error=function(cond) {
      print("E5: Invalid Input")
      return(NA)
    },
    warning=function(cond) {
      print("W5: Invalid Input")
      return(NA)
    }
  ) 
  return(calc)
}

# function 6
Get_Matrix<-function(File,col,set){
  # Create row  & col labels
  rw_set={}
  cn_set={}
  for(j in 1:length(set)){
    rw_set= append(rw_set,switch(set[j],"count","Mean","std","min","25%","50%","75%","max"))
  }
  for(i in 1:length(col)){
    cn_set<-append(cn_set,colnames(File)[col[i]])
  }
  
  # Collect Data
  data_set={}
  for(j in 1:length(set)){
    for(i in 1:length(col)){
      data_set=append(data_set,calculate(File[,col[i]],set[j]))
    }
  }
  return(matrix(data_set,nrow = length(set),ncol=length(col),byrow=TRUE,dimnames = list(rw_set,cn_set)))
}

# main function
main<-function(){
  File<-read.csv(file.choose())
  col_names = colnames(File)
  colset = col_chose(col_names)
  if(!is.null(colset)){
    oprset = opr_chose()
    if(!is.null(oprset)){
      mat<-Get_Matrix(File,colset,oprset)
      mat
    }
  }
}