
#############################################################################
#A function to check,  format and order NMA data according to sortvar
#############################################################################
#data: the inputed dataset in sequentialnma function
#args: the defined arguments
#!!!!!!!!!!! Define the arguments ???
formatdata=function (data,args)
{
  perarm=as.logical(as.character(args$perarm))
  #sto epomeno giati den exei apla type=args["type"] gia na mhn trexeis nea function?
  getpar=function(field){
    return(args[field])
  }
  type=getpar("type")
  
  
  ## !!!!!!!!! olo to epomeno mexri ta epomena thaumastika  tha mporouses na to kaneis me to 
  ####  data$r = eval(substitute(r), data)
  ####na min mperdeuomaste me ta onomata palia kai nea?
  
  #define correspondance between old and new names
  defaultargs = function (arg){
    switch(arg, "studlab" = "id"
           , "t" = "t"
           , "r"= "r"
           , "n"="n"
           , "y"="y"
           , "sd"="sd"
           , "TE"="TE"
           , "seTE"="seTE"
           , "t1"="t1"
           , "t2"="t2"
           , "sortvar"="year")}
  
  fieldsToArgs = function (field){
    switch(field, "id" = "studlab"
           , "t" = "t"
           , "r"= "r"
           , "n"="n"
           , "y"="y"
           , "sd"="sd"
           , "TE"="TE"
           , "seTE"="seTE"
           , "t1"="t1"
           , "t2"="t2"
           , "year"="sortvar")}
    
  oldnames = colnames(data)

  #define new names of variables in the dataset
 newnames = as.vector(
   mapply(function(name){
     i = match(name,unlist(args))
     out = ""
     if (is.na(i)){
       out = name
     }else{
       arg = colnames(as.data.frame(unlist(args)[i]))
       out =  defaultargs(arg)
     }
     return(out)
   },oldnames)
 )
 
 ###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
 #function to check whether mandotory arguments exist in the data and stop otherwise
 checkarguments = function(nnames,mands){
   mapply(function(field){
    if(any(is.na(match(field,nnames)))) {
      stop("field ",field," is missing, define ",fieldsToArgs(field)," in the arguments")
    }
   },mands)
 }
  
 #define mandatory data for each data type and check whether exist in the data
  if (perarm && type=="binary"){
    mandatories = c("id","t","r","n","year")
    checkarguments(newnames,mandatories)
  } 
 if (perarm && type=="continuous"){
   mandatories = c("id","t","y","sd","n","year")
   checkarguments(newnames,mandatories)
 }
 if (!perarm){
   mandatories = c("id","t1","t2","TE","seTE","year")
   checkarguments(newnames,mandatories)
 }

 #sort everything according to sorting value and define new names
 orddata=data[order(unlist(data[args$sortvar])),]
 colnames(orddata) = newnames
 studies = orddata
 
 return(studies)
}