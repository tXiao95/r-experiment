library(sloop)

f <- factor(c("a", "b", "c"))
typeof(f)

attributes(f)


# What class of object does the following cod ereturn? What base type is it built upon? 
#' What attributes does it use?
x <- ecdf(rpois(100,10))
typeof(x)
attributes(x)
#' ecdf objects are built on the 'closure' base type. It has the 'class' and 'call' 
#' attributes. Theclasses in order of most to least specific are 'ecdf', 'stepfun', and 'function'
#' 

#' What class of object does the following code return? What base type is it built on? What attributes
#' does it use?
x <- table(rpois(100,5))
typeof(x)
attributes(x)
#' table objects are built on the 'integer' base type. It has the 'class', 'dim', and 'dimnames' 
#' attributes. Since it has a 'class' attribute, it is a S3 object. 

#' S3 system has no formal definition of a class: to make an object an instance of a calss
#' you just set the 'class' attribute. Unlike in python, where you officially declare it. 
#' Therefore, since there is no fromal definition, there is no built-in way to ensure that 
#' all objects of a given class have the same structure

#' Low level constructor for data frame. Data frame is a list base type. 
new_data_frame <- function(x = list(), row.names = character(), names = character()){
  stopifnot(is.list(x))
  stopifnot(is.character(row.names) & is.integer(row.names))
  stopifnot(is.character(names))
  
  structure(x, 
            names = names, 
            row.names = row.names, 
            class = "data.frame")
}


ftype(t.test)

x <- structure(1:10, class = "test")
t(x)
