##Social TV paper Addyman et al 2017
##Some helper functions to display graphs.

#' Takes the wide (1 row per person) version of the data and makes a long 
#' subset with a single DV (1 col for grouping variable, 1 col for DV)
#' In output grouping variable is "groupVar", DV is "value"
#' 
#' @param allData original data
#' @param col1 1st DV column, etc
groupLongData <- function(allData,col1,col2,col3){
  #subset and rearrange laugh data for anova and plotting
  #convert to long format 
  data.long <- melt(allData,
                    id.vars = c("ID","SEX","ORDER"),
                    measure.vars = c(col1, col2,col3),
                    variable.name ="groupVar")
  #indicate the factor variable
  data.long$groupVar <- factor(data.long$groupVar)
  return(data.long)
}

#' Takes three columns from dataframe and compares them in one way Anova
#' while also plotting a box plot with overlayed individual data points.
#' 
#' @param data.Long Long format version of the data.
#' @param varName Name of the grouping Variable.
#' @examples
#' plotGroupData(longLaughs, "Smiles")
plotGroupData <- function (data.long, varName) {
  #plot groups as box plot and individual values
  ylabel<-paste("Number of ", varName,sep = "")
  p<-ggplot(data=data.long,aes(groupVar,value)) +
    geom_boxplot() +
    geom_point(aes(groupVar,value), size=3,position = position_jitter(width = .1) ) +
    geom_signif(comparisons = list(c(2,3),c(1,3),c(1,2)), step_increase = .12, map_signif_level = TRUE, test="t.test", test.args = list(paired = TRUE)) +
    ylab(ylabel) +
    xlab(NULL) +
    theme_bw(base_size =18)
  return(p)
}

#' plots a standard interaction plot for one-way anova but taking care to
#' use standard errors suitable for repeated errors (like SPSS)
#' 
#' @param data.Long Long format version of the data.
#' @param varName Name of the grouping Variable.
#' @examples
#' plotOneWayANOVA(groupLaughter, "Smiles") 
plotOneWayANOVA <- function (longData, varName) {
  #calculate repeated measures standard errors
  laughsummary<-summarySEwithin(longData, measurevar = "value",betweenvars = NULL,withinvars =  "groupVar")
  # Standard error of the mean
  ylabel<-paste("Number of ", varName,sep = "")
  p<-ggplot(laughsummary, aes(x=groupVar, y=value, group=1)) + 
    geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
    geom_signif(comparisons = list(c(2,3),c(1,3),c(1,2)), step_increase = .12, map_signif_level = TRUE, test="t.test", test.args = list(paired = TRUE)) +
    geom_line() +
    geom_point(size=3) +
    ylab(ylabel) +
    xlab(NULL) +
    theme_bw(base_size =18)
  return(p)
}
