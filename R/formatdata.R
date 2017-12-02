#checks and formats and orders data according to sortvar
formatdata=function (data,args)
{
  perarm=as.logical(as.character(args$perarm))
  getpar=function(field){
    return(args[field])
  }
  type=getpar("type")
  
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
  
 checkarguments = function(nnames,mands){
   mapply(function(field){
    if(any(is.na(match(field,nnames)))) {
      stop("field ",field," is missing, define ",fieldsToArgs(field)," in the arguments")
    }
   },mands)
 }
  #check data set columns
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
  

 #sort everything according to sorting value
 orddata=data[order(unlist(data[args$sortvar])),]
 colnames(orddata) = newnames
 studies = orddata
 
 return(studies)
}