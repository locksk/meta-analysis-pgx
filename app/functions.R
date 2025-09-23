# functions for PGx Shiny App

# transforms numbers to superscript
to_superscript <- function(x) {
  superscript_digits <- c(
    '0' = '\u2070', '1' = '\u00B9', '2' = '\u00B2', '3' = '\u00B3', 
    '4' = '\u2074', '5' = '\u2075', '6' = '\u2076', '7' = '\u2077', 
    '8' = '\u2078', '9' = '\u2079', '-' = '\u207B'
  )
  chars <- strsplit(x, NULL)[[1]]
  superscript <- sapply(chars, function(char) superscript_digits[char])
  return(paste0(superscript, collapse = ""))
}

# transforms numbers from 6e-5 to 6x10-5
format_p <- function(x) {
  significant_digits <- ifelse(x < 0.01, 1, ifelse(x < 0.1, 2, 3))  
  tmp <- as.character(signif(x, digits = significant_digits))
  
  tmp <- gsub("e-0", "x10^-", tmp)
  tmp <- gsub("e", "x10^", tmp)
  
  tmp <- gsubfn("x10\\^(-?\\d+)", function(y) {
    power <- gsub("x10\\^", "", y)
    paste0("x10", to_superscript(power))
  }, tmp)
  return(tmp)
}

# get %
round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}

# power calc function
get_power <- function(df){
  power_result <- pwr.t.test(n=df$sample_sizes, 
                             d=df$effect_sizes,
                             type='two.sample')
  df$power=power_result$power
  return(df)
}

# antonio plot function
antonio_plot <- function(model){
  df <- model$data
  df$class <- "ind"
  
  # calculate and organise the adjusted effects
  sums <- data.frame(lab = rownames_to_column(as.data.frame(model$beta))[1],
                     SMD = model$beta,
                     SE = model$se,
                     Var = model$se^2,
                     CI_low = model$ci.lb,
                     CI_up = model$ci.ub,
                     Weight = 1/(model$se^2),
                     class = "pool"
  )
  df <- rbind(df, sums, fill = TRUE)
  
  # sort the sorting
  df$class <- factor(test_map$class, ordered = TRUE, levels = c("ind", "pool"))
  df <- df %>% arrange(desc(class), SMD)
  df$study_id <- 1:(nrow(df))
  
  ggplot(df, aes(x=SMD, y= study_id, shape = class, size = class)) +
    geom_point() + 
    scale_size_manual(values=c(1,5)) +
    geom_errorbar(data = ~filter(.x, class == "pool"), aes(xmin = CI_low, xmax = CI_up), linewidth = .5, width = 7, position = position_dodge(.01)) + #  
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    scale_shape_manual(values = c(16,18)) + # different shapes for individual vs pooled 
    labs(x="Standardised Mean Difference for Individual and Pooled Effects", y = "Study") +
    geom_vline(xintercept = 0, linetype = "longdash", colour = '#A2AEB3') +
    theme_minimal() +
    #  xlim(c(-35, 20)) +
    guides(fill='none', shape='none', size= 'none') +
    theme(axis.title = element_text(colour = "#A2AEB3"),
          axis.text.x = element_text(colour = "#A2AEB3", size = 8),
          axis.title.x = element_text(colour = "#A2AEB3", size = 10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(colour = "#A2AEB3"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
}


# BibTeX generation func
generate_bibtex <- function(data) {
  bibtex_entries <- apply(data, 1, function(row) {
    entry <- paste0("@article{", tolower(gsub(" ", "_", str_replace_all(row["Study"], "[[:punct:]]", ""))), ",\n")
    fields <- c("author", "title", "journal", "year", "volume", "issue", "pages", "doi", "url")
    
    for (field in fields) {
      if (!is.na(row[field])) {
        entry <- paste0(entry, "  ", field, ' = "', row[field], '",\n')
      }
    }
    
    entry <- paste0(entry, "}\n\n")
    return(entry)
  })
  
  return(paste(bibtex_entries, collapse = ""))
}


# custom orchard plot function

custom_orchard_plot <- function(object, mod = "1", group, xlab, N = NULL,
                                alpha = 0.5, angle = 90, cb = TRUE, k = TRUE, g = TRUE,
                                tree.order = NULL, trunk.size = 1.2, branch.size = 1, twig.size = 0.25,
                                transfm = c("none", "tanh", "invlogit", "percent", "percentr"), condition.lab = "Condition",
                                legend.pos = c("bottom.right", "bottom.left",
                                               "top.right", "top.left",
                                               "top.out", "bottom.out",
                                               "none"), # "none" - no legends
                                k.pos = c("right", "left", "none"),
                                colour = FALSE,
                                fill = TRUE,
                                weights = "prop", by = NULL, at = NULL, upper = TRUE, flip = TRUE)
{
  ## evaluate choices, if not specified it takes the first choice
  transfm <- match.arg(NULL, choices = transfm)
  legend.pos <- match.arg(NULL, choices = legend.pos)
  k.pos <- match.arg(NULL, choices = k.pos)
  
  if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni"))){
    
    if(mod != "1"){
      results <-  orchaRd::mod_results(object, mod, group,  N,
                                       by = by, at = at, weights = weights, upper = upper)
    } else {
      results <-  orchaRd::mod_results(object, mod = "1", group,  N,
                                       by = by, at = at, weights = weights, upper = upper)
    }
  }
  
  if(any(class(object) %in% c("orchard"))) {
    results <- object
  }
  
  mod_table <- results$mod_table
  
  data_trim <- results$data
  # making sure factor names match
  data_trim$moderator <- factor(data_trim$moderator, levels = mod_table$name, labels = mod_table$name)
  
  data_trim$scale <- (1/sqrt(data_trim[,"vi"]))
  legend <- "Precision (1/SE)"
  
  #if tree.order isn't equal to NULL, and length of tree order does not match number of categories in categorical moderator, then stop function and throw an error
  if(!is.null(tree.order)&length(tree.order)!=nlevels(data_trim[,'moderator'])){
    stop("Length of 'tree.order' does not equal number of categories in moderator")
  }
  
  #if tree.order isn't equal to NULL but passes above check, then reorder mod table according to custom order if there is one
  if (!is.null(tree.order)){
    data_trim$moderator<-factor(data_trim$moderator, levels = tree.order, labels = tree.order)
    mod_table <- mod_table %>% dplyr::arrange(factor(name, levels = tree.order))
  }
  
  if(is.null(N) == FALSE){
    data_trim$scale <- data_trim$N
    legend <- paste0("Sample Size ($\\textit{N}$)") # we want to use italic
    #latex2exp::TeX()
  }
  
  if(transfm == "tanh"){
    cols <- sapply(mod_table, is.numeric)
    mod_table[,cols] <- Zr_to_r(mod_table[,cols])
    data_trim$yi <- Zr_to_r(data_trim$yi)
    label <- xlab
  }
  
  if(transfm == "invlogit"){
    
    cols <- sapply(mod_table, is.numeric)
    mod_table[,cols] <- lapply(mod_table[,cols], function(x) metafor::transf.ilogit(x))
    data_trim$yi <- metafor::transf.ilogit(data_trim$yi)
    label <- xlab
  }
  
  if(transfm == "percentr"){
    
    cols <- sapply(mod_table, is.numeric)
    mod_table[,cols] <- lapply(mod_table[,cols], function(x) (exp(x) - 1)*100)
    data_trim$yi <- (exp(data_trim$yi) - 1)*100
    label <- xlab
  } 
  
  
  
  if(transfm == "percent"){
    
    cols <- sapply(mod_table, is.numeric)
    mod_table[,cols] <- lapply(mod_table[,cols], function(x) exp(x)*100)
    data_trim$yi <- (exp(data_trim$yi)*100)
    label <- xlab
  } else{
    label <- xlab
  }
  
  # Add in total effect sizes for each level
  mod_table$K <- as.vector(by(data_trim, data_trim[,"moderator"], function(x) length(x[,"yi"])))
  
  # Add in total levels of a grouping variable (e.g., study ID) within each moderator level.
  mod_table$g <- as.vector(num_studies(data_trim, moderator, stdy)[,2])
  
  # the number of groups in a moderator & data points
  group_no <- length(unique(mod_table[, "name"]))
  
  #data_no <- nrow(data)
  
  # colour blind friendly colours with grey
  cbpl <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  
  # setting fruit colour
  if(colour == TRUE){
    color <- as.factor(data_trim$stdy)
    color2 <- NULL
  }else{
    color <- data_trim$mod
    color2 <- mod_table$name
  }
  
  # whether we fill fruit or not
  if(fill == TRUE){
    fill <- color
  }else{
    fill <- NULL
  }
  
  # whether marginal
  if(names(mod_table)[2] == "condition"){
    
    # the number of levels in the condition
    condition_no <- length(unique(mod_table[, "condition"]))
    
    plot <- ggplot2::ggplot() +
      # pieces of fruit (bee-swarm and bubbles)
      ggbeeswarm::geom_quasirandom(data = data_trim, ggplot2::aes(y = yi, x = moderator, size = scale, colour = color, fill = fill), alpha=alpha, shape = 21) +
      
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "black", alpha = alpha) +
      # creating CI
      ggplot2::geom_linerange(data = mod_table, ggplot2::aes(x = name, ymin = lowerCL, ymax = upperCL),
                              size = branch.size, position = ggplot2::position_dodge2(width = 0.3)) +
      # drowning point estimate and PI
      ggplot2::geom_pointrange(data = mod_table, ggplot2::aes(y = estimate, x = name, ymin = lowerPR, ymax = upperPR,  shape = as.factor(condition), fill = color2), size = trunk.size, position = ggplot2::position_dodge2(width = 0.3), linewidth = twig.size) +
      # this will only work for up to 5 different conditions
      # flipping things around (I guess we could do use the same geoms but the below is the original so we should not change)
      ggplot2::scale_shape_manual(values =  20 + (1:condition_no))  +
      ggplot2::theme_bw() +
      ggplot2::guides(fill = "none", colour = "none") +
      ggplot2::theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.direction="horizontal") +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::labs(y = label, x = "", size = latex2exp::TeX(legend)) +
      ggplot2::labs(shape = condition.lab) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, colour ="black",
                                                         hjust = 0.5,
                                                         angle = angle))
    if(flip){
      plot <- plot + ggplot2::coord_flip()
    }
    
  } else {
    
    plot <- ggplot2::ggplot() +
      # pieces of fruit (bee-swarm and bubbles)
      ggbeeswarm::geom_quasirandom(data = data_trim, ggplot2::aes(y = yi, x = moderator, size = scale, colour = color, fill = fill), alpha=alpha, shape = 21) +
      
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "black", alpha = alpha) +
      # creating CI
      ggplot2::geom_linerange(data = mod_table, ggplot2::aes(x = name, ymin = lowerCL, ymax = upperCL), size = branch.size) +
      # drowning point estimate and PI
      ggplot2::geom_pointrange(data = mod_table, ggplot2::aes(y = estimate, x = name,  ymin = lowerPR, ymax = upperPR, fill = color2), size = trunk.size, linewidth = twig.size, shape = 18) +
      ggplot2::theme_bw() +
      ggplot2::guides(fill = "none", colour = "none") +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 9)) +
      ggplot2::theme(legend.direction="horizontal") +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::labs(y = label, x = "", size = latex2exp::TeX(legend)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, colour ="black",
                                                         hjust = 0.5,
                                                         angle = angle)) #+
    #ggplot2::theme(legend.position= c(1, 0), legend.justification = c(1, 0))
    if(flip){
      plot <- plot + ggplot2::coord_flip()
    }
  }
  
  # adding legend
  if(legend.pos == "bottom.right"){
    plot <- plot + ggplot2::theme(legend.position= c(1, 0), legend.justification = c(1, 0))
  } else if ( legend.pos == "bottom.left") {
    plot <- plot + ggplot2::theme(legend.position= c(0, 0), legend.justification = c(0, 0))
  } else if ( legend.pos == "top.right") {
    plot <- plot + ggplot2::theme(legend.position= c(1, 1), legend.justification = c(1, 1))
  } else if (legend.pos == "top.left") {
    plot <- plot + ggplot2::theme(legend.position= c(0, 1), legend.justification = c(0, 1))
  } else if (legend.pos == "top.out") {
    plot <- plot + ggplot2::theme(legend.position="top")
  } else if (legend.pos == "bottom.out") {
    plot <- plot + ggplot2::theme(legend.position="bottom")
  } else if (legend.pos == "none") {
    plot <- plot + ggplot2::theme(legend.position="none")
  }
  
  # putting colors in
  if(cb == TRUE){
    plot <- plot +
      ggplot2::scale_fill_manual(values = cbpl) +
      ggplot2::scale_colour_manual(values = cbpl)
  }
  
  # putting k and g in
  if(k == TRUE && g == FALSE && k.pos == "right"){
    plot <- plot +
      ggplot2::annotate('text', y = (max(data_trim$yi) + (max(data_trim$yi)*0.10)), x = (seq(1, group_no, 1)+0.3),
                        label= paste("italic(k)==", mod_table$K[1:group_no]), parse = TRUE, hjust = "right", size = 3.5)
  } else if(k == TRUE && g == FALSE && k.pos == "left") {
    plot <- plot +  ggplot2::annotate('text', y = (min(data_trim$yi) + (min(data_trim$yi)*0.10)), x = (seq(1, group_no, 1)+0.3),
                                      label= paste("italic(k)==", mod_table$K[1:group_no]), parse = TRUE, hjust = "left", size = 3.5)
  } else if (k == TRUE && g == TRUE && k.pos == "right"){
    # get group numbers for moderator
    plot <- plot + ggplot2::annotate('text', y = (max(data_trim$yi) + (max(data_trim$yi)*0.10)), x = (seq(1, group_no, 1)+0.3),
                                     label= paste("italic(k)==", mod_table$K[1:group_no], "~","(", mod_table$g[1:group_no], ")"),
                                     parse = TRUE, hjust = "right", size = 3.5)
  } else if (k == TRUE && g == TRUE && k.pos == "left"){
    # get group numbers for moderator
    plot <- plot + ggplot2::annotate('text',  y = (min(data_trim$yi) + (min(data_trim$yi)*0.10)), x = (seq(1, group_no, 1)+0.3),
                                     label= paste("italic(k)==", mod_table$K[1:group_no], "~","(", mod_table$g[1:group_no], ")"),
                                     parse = TRUE, hjust = "left", size = 3.5)
  } else if (k == TRUE && g == FALSE && k.pos%in%c('right','left','none')==FALSE) {
    # get group numbers for moderator
    plot <- plot + ggplot2::annotate("text", y = k.pos, x = (seq(1, group_no,
                                                                 1) + 0.3), label = paste("italic(k)==", mod_table$K[1:group_no]),
                                     parse = TRUE, size = 3.5)
  } else if (k == TRUE && g == TRUE && k.pos%in%c('right','left','none')==FALSE) {
    # get group numbers for moderator
    plot <- plot + ggplot2::annotate("text", y = k.pos, x = (seq(1, group_no,
                                                                 1) + 0.3), label = paste("italic(k)==", mod_table$K[1:group_no],
                                                                                          "~", "(", mod_table$g[1:group_no], ")"),
                                     parse = TRUE, size = 3.5)
  }
  return(plot)
}

#' Calculate I-squared values and variance distribution for multilevel meta-analysis models
#'
#' This function calculates values of \eqn{I^2} and the variance distribution for multilevel meta-analysis
#' models fitted with \code{\link[metafor]{rma.mv}}.
#'
#'
#' @usage mlm.variance.distribution(x)
#'
#' @param x An object of class \code{rma.mv}. Must be a multilevel model with two random effects (three-level meta-analysis model).
#'
#' @details This function estimates the distribution of variance in a three-level meta-analysis
#' model (fitted with the \code{\link[metafor]{rma.mv}} function). The share of variance attributable to
#' sampling error, within and between-cluster heterogeneity is calculated,
#' and an estimate of \eqn{I^2} (total and for Level 2 and Level 3) is provided. The function uses the formula by
#' Cheung (2014) to estimate the variance proportions attributable to each model component and to derive the \eqn{I^2} estimates.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/mlma.html}{Chapter 12}.
#'
#'Cheung, M. W. L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural equation modeling approach. \emph{Psychological Methods, 19}(2), 211.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @aliases var.comp
#'
#' @import ggplot2
#' @importFrom stats model.matrix
#'
#' @return Returns a data frame containing the results. A plot summarizing the variance distribution and \eqn{I^2} values can be generated using \code{plot}.
#'
#' @export mlm.variance.distribution
#' @export var.comp
#'
#' @examples
#' # Use dat.konstantopoulos2011 from the "metafor" package
#' library(metafor)
#'
#' # Build Multilevel Model (Three Levels)
#' m = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)
#'
#' # Calculate Variance Distribution
#' mlm.variance.distribution(m)
#'
#' # Use alias 'var.comp' and 'Chernobyl' data set
#' data("Chernobyl")
#' m2 = rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
#' res = var.comp(m2)
#'
#' # Print results
#' res
#'
#' # Generate plot
#' plot(res)



mlm.variance.distribution = var.comp = function(x){
  
  m = x
  
  # Check class
  if (!(class(m)[1] %in% c("rma.mv", "rma"))){
    stop("x must be of class 'rma.mv'.")
  }
  
  # Check for three level model
  if (m$sigma2s != 2){
    stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
  }
  
  # Check for right specification (nested model)
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1){
    stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
  }
  
  # Get variance diagonal and calculate total variance
  n = m$k.eff
  vector.inv.var = 1/(diag(m$V))
  sum.inv.var = sum(vector.inv.var)
  sum.sq.inv.var = (sum.inv.var)^2
  vector.inv.var.sq = 1/(diag(m$V)^2)
  sum.inv.var.sq = sum(vector.inv.var.sq)
  num = (n-1)*sum.inv.var
  den = sum.sq.inv.var - sum.inv.var.sq
  est.samp.var = num/den
  
  # Calculate variance proportions
  level1=((est.samp.var)/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level2=((m$sigma2[2])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level3=((m$sigma2[1])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  
  # Prepare df for return
  Level=c("Level 1", "Level 2", "Level 3")
  Variance=c(level1, level2, level3)
  df.res=data.frame(Variance)
  colnames(df.res) = c("% of total variance")
  rownames(df.res) = Level
  I2 = c("---", round(Variance[2:3], 2))
  df.res = as.data.frame(cbind(df.res, I2))
  
  totalI2 = Variance[2] + Variance[3]
  
  
  # Generate plot
  df1 = data.frame("Level" = c("Sampling Error", "Total Heterogeneity"),
                   "Variance" = c(df.res[1,1], df.res[2,1]+df.res[3,1]),
                   "Type" = rep(1,2))
  
  df2 = data.frame("Level" = rownames(df.res),
                   "Variance" = df.res[,1],
                   "Type" = rep(2,3))
  
  df = as.data.frame(rbind(df1, df2))
  
  
  g = ggplot(df, aes(fill=Level, y=Variance, x=as.factor(Type))) +
    coord_cartesian(ylim = c(0,1), clip = "off") +
    geom_bar(stat="identity", position="fill", width = 1, color="black") +
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color="black"),
          axis.line.y = element_blank(),
          axis.title.y=element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(lineend = "round"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          axis.ticks.length=unit(.25, "cm"),
          plot.margin = unit(c(1,3,1,1), "lines")) +
    scale_fill_manual(values = c("darkseagreen3", "deepskyblue3", "darkseagreen2",
                                 "deepskyblue1", "deepskyblue2")) +
    
    # Add Annotation
    
    # Total Variance
    annotate("text", x = 1.5, y = 1.05,
             label = paste("Total Variance:",
                           round(m$sigma2[1]+m$sigma2[2]+est.samp.var, 3))) +
    
    # Sampling Error
    annotate("text", x = 1, y = (df[1,2]/2+df[2,2])/100,
             label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 3) +
    
    # Total I2
    annotate("text", x = 1, y = ((df[2,2])/100)/2-0.02,
             label = bquote("Total"~italic(I)^2*":"~.(round(df[2,2],2))*"%"), size = 3) +
    annotate("text", x = 1, y = ((df[2,2])/100)/2+0.05,
             label = paste("Variance not attributable \n to sampling error: \n", round(m$sigma2[1]+m$sigma2[2],3)), size = 3) +
    
    # Level 1
    annotate("text", x = 2, y = (df[1,2]/2+df[2,2])/100, label = paste("Level 1: \n",
                                                                       round(df$Variance[3],2), "%", sep=""), size = 3) +
    
    # Level 2
    annotate("text", x = 2, y = (df[5,2]+(df[4,2]/2))/100,
             label = bquote(italic(I)[Level2]^2*":"~.(round(df[4,2],2))*"%"), size = 3) +
    
    # Level 3
    annotate("text", x = 2, y = (df[5,2]/2)/100,
             label = bquote(italic(I)[Level3]^2*":"~.(round(df[5,2],2))*"%"), size = 3)
  
  returnlist = list(results = df.res,
                    totalI2 = totalI2,
                    plot = g)
  class(returnlist) = c("mlm.variance.distribution", "list")
  
  invisible(returnlist)
  
  returnlist
  
}


# bayesian 

# Custom functions
posterior = function(b,s,p,tau){
  z=b/s
  tau2=tau^2
  q=p*dnorm(z,0,sqrt(tau2+1))
  q=q/sum(q) # conditional mixing probs
  pm=b*tau2/(tau2+1) # conditional means
  pv=s^2*tau2/(tau2+1) # conditional variances
  ps=sqrt(pv) # conditional std devs
  data.frame(q,pm,pv,ps)
}
shrink = function(b,s,p,tau){
  post=posterior(b,s,p,tau)
  b/sum(post$q * post$pm)
}

dmix = function (x,p,m,s){ # density (vector x)
  p %*% sapply (x, function (x) dnorm (x,mean=m,sd=s)) }
pmix = function (x,p,m,s){ # cumulative distr (vector x)
  p %*% sapply (x, function (x) pnorm (x,mean=m,sd=s)) }
rmix = function (n,p,m,s){ d= rmultinom (n,1,p)
rnorm (n,m %*% d,s %*% d) }
qfun = function (q,p,m,s){ # quantile function scalar q
  uniroot ( function (x) pmix (x,p,m,s) - q, interval= c ( - 20,20)) $ root }
qmix = function (q,p,m,s){ # quantile function vector q
  sapply (q, function (q) qfun (q,p=p,m=m,s=s) ) } 