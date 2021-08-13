################################################
## MARGINAL MODEL PLOTS for BRM @Hok Chio (Mark) Lai
################################################

mmp_brm <- function(object, x = NULL, prob = 0.95, size = 0.8, 
                    plot_pi = FALSE, jitter = FALSE, 
                    smooth_method = "auto") {
  dat <- object$data
  post_mu <- fitted(object, scale = "response")
  colnames(post_mu) <- c("mu", "mu_se", "lwr_ci", "upr_ci")
  df_plot <- cbind(dat, post_mu)
  if (is.null(x)) {
    lin_pred <- fitted(object, scale = "linear")
    df_plot$lin_pred <- lin_pred[ , 1]
    x <- "lin_pred"
  }
  x_sd <- sd(df_plot[[x]])
  p <- ggplot(aes_string(x = paste0("`", x, "`"), 
                         y = paste0("`", names(dat)[1], "`")), data = df_plot) + 
    # Add a layer of predictive intervals
    geom_ribbon(aes(ymin = predict(loess(as.formula(paste("lwr_ci ~", x)), 
                                         data = df_plot)), 
                    ymax = predict(loess(as.formula(paste("upr_ci ~", x)), 
                                         data = df_plot))), 
                fill = "skyblue", alpha = 0.3) + 
    geom_smooth(aes(y = mu, col = "Model"), se = FALSE, 
                method = smooth_method) + 
    geom_smooth(aes(col = "Data"), se = FALSE, linetype = "dashed", 
                method = smooth_method) + 
    theme(legend.position = "bottom") + 
    scale_color_manual(values = c("red", "blue"), name = "")
  if (jitter) {
    p <- p + geom_jitter(size = size, width = x_sd * .1, height = .02)
  } else {
    p <- p + geom_point(size = size)
  }
  if (plot_pi) {
    pi <- predictive_interval(object, prob = prob)
    colnames(pi) <- c("lwr_pi", "upr_pi")  # change the names for convenienc
    # Combine the PIs with the original data
    df_plot <- cbind(df_plot, pi)
    p <- p + geom_smooth(aes(y = upr_pi), data = df_plot, linetype = "longdash", 
                         se = FALSE, size = 0.5, col = "green", 
                         method = smooth_method) + 
      geom_smooth(aes(y = lwr_pi), data = df_plot, linetype = "longdash", 
                  se = FALSE, size = 0.5, col = "green", 
                  method = smooth_method)
  }
  p
}


################################################
## HIERARCHICAL VARIABLE SCALING
################################################

library(plyr)
hscale <- function(v,h) {
  base <- aggregate(v~h,FUN=mean)
  v <- as.numeric(mapvalues(as.character(h),from=as.character(base$h),to=scale(base$v)))
  v
}


################################################
## TRANSFORMED LMER RESIDUALS
################################################

tdiagnostic <- function(merMod) {
  var.d <- crossprod(getME(merMod,"Lambdat"))
  Zt <- getME(merMod,"Zt")
  vr <- sigma(merMod)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(merMod@frame))
  var.y <- var.b + sI
  Li <- t(chol(var.y))
  tres <- as.vector(solve(Li) %*% residuals(merMod))
  tfit <- as.vector(solve(Li) %*% fitted(merMod))
  data.frame(tres,tfit)
}

##########################################################################
#                                                                        #
#               FUNCTION FOR COMPUTING PARTIAL ETA-SQUARED               #
#                  ALONG WITH THEIR CONFIDENCE INTERVAL                  #
#                   FOR ANALYSES OF VARIANCE (ANOVAs)                    #
#                                                                        #
##########################################################################


# FROM Yoann STUSSI , LAST UPDATED ON: 27.03.20 by David MUNOZ TORD

# Based on:
# Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: A practical primer for t-tests and ANOVAs. 
#                    Frontiers in Psychology, 4, 863. https://doi.org/10.3389/fpsyg.2013.00863
# http://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html


#------------------------------------------------------------------------------------------------------------------------#
# ARGUMENTS:
# formula    = formula specifying the model using the aov format (e.g., DV ~ IVb * IVw1 * IVw2 + Error(id/(IVw1 * IVw2)))
# data       = a data frame in which the variables specified in the formula will be found
# conf.level = level of the confidence interval between 0 to 1 [default -> .90]
# epsilon    = epsilon correction used to adjust the lack of sphericity for factors involving repeated measures among 
#              list("default", "none", "GG", "HF")
#              - default: use Greenhouse-Geisser (GG) correction when epsilon GG < .75, Huynh-Feldt (HF) correction 
#                         when epsilon GG = [.75, .90], and no correction when epsilon GG >= .90 [default]
#              - none: use no sphericity correction
#              - GG: use Greenhouse-Geisser (GG) correction for all factors (>2 levels) involving repeated measures
#              - HF: use Huynh-Feldt (HF) correction for all factors (>2 levels) involving repeated measures
# anova.type = type of sum of squares used in list("II", "III", 2, 3)
#              - "II" or 2: use type II sum of squares (hierarchical or partially sequential)
#              - "III" or 3: use type III sum of squares (marginal or orthogonal) [default] 
#   added observed and factorize arguments
#------------------------------------------------------------------------------------------------------------------------#

pes_ci <- function(formula, data, conf.level, epsilon, anova.type, observed=NULL, factorize=FALSE)
{
  
  #----------------------------------------------------------------------------------------------#
  #----ARGUMENTS
  # conf.level
  ifelse(missing(conf.level), conf.level <- .90, conf.level <- conf.level)
  
  # epsilon
  ifelse(missing(epsilon), epsilon <- "default", epsilon <- epsilon)
  
  iscorrectionok <- epsilon == list("default", "none", "GG", "HF")
  if(all(iscorrectionok == F)) stop('Unknown correction for sphericity used. Please use (i) the default option ("default"; i.e., no correction applied when epsilon GG >= .90, 
                                    GG correction applied when GG epsilon < .75, and HF correction applied when GG epsilon = [.75, .90]),
                                    (ii) no correction ("none"), (iii) Greenhous-Geisser ("GG") correction, or (iv) Huyhn-Feldt ("HF") correction.')
  
  # anova.type
  ifelse(missing(anova.type), anova.type <- "III", anova.type <- anova.type)
  
  
  #----------------------------------------------------------------------------------------------#
  #----COMPUTE ETA-SQUARED EFFECT SIZE ESTIMATES
  
  # afex package for computing eta-squared estimates
  require(afex)
  
  # compute anova with partial (pes) eta-squared estimates
  aov     <- aov_car(formula = formula, 
                     data = data, 
                     anova_table = list(es = "pes"),
                     type = anova.type,
                     observed = observed,
                     factorize = factorize)
  aov.sum <- summary(aov)
  
  
  #----CREATE MATRIX TO STORE RESULTS
  matrix.es <- matrix(nrow = length(rownames(aov$anova_table)), ncol = 3)
  
  
  #----------------------------------------------------------------------------------------------#
  #----COMPUTE CONFIDENCE INTERVAL FOR EACH FACTOR
  
  # MBESS package for computing confidence intervals
  require(MBESS)
  
  for (i in 1:length(rownames(aov$anova_table))) {
    
    # CHECK
    if (length(aov.sum$pval.adjustments) != 0) {
      
      #--------------------------------------------------------------------------------------------#
      # DEFAULT OPTION 
      # (no correction if GG epsilon >= .90, GG correction if GG epsilon < .75, or HF correction if GG epsilon = [.75, .90])
      if (epsilon == "default") {
        
        # Search for factors with more than 2 levels
        for (j in 1:length(rownames(aov.sum$pval.adjustments))) {
          
          if (rownames(aov.sum$pval.adjustments)[j] == rownames(aov$anova_table)[i]) {
            
            # Apply GG correction if GG epsilon < .75
            if (is.na(aov.sum$pval.adjustments[j, "GG eps"]) == F && aov.sum$pval.adjustments[j, "GG eps"] < .75) {
              
              aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                             conf.level = conf.level,
                                             df.1 <- aov.sum$univariate.tests[i + 1, "num Df"] * aov.sum$pval.adjustments[j, "GG eps"],
                                             df.2 <- aov.sum$univariate.tests[i + 1, "den Df"] * aov.sum$pval.adjustments[j, "GG eps"])
              break
            }
            
            # Apply HF correction if GG epsilon = [.75, .90]
            else if (is.na(aov.sum$pval.adjustments[j, "GG eps"]) == F && aov.sum$pval.adjustments[j, "GG eps"] >= .75 && aov.sum$pval.adjustments[j, "GG eps"] < .90) {
              
              aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                             conf.level = conf.level,
                                             df.1 <- aov.sum$univariate.tests[i + 1, "num Df"] * aov.sum$pval.adjustments[j, "HF eps"],
                                             df.2 <- aov.sum$univariate.tests[i + 1, "den Df"] * aov.sum$pval.adjustments[j, "HF eps"])
              break
            }
            
            # Apply no correction if GG epsilon >= .90
            else if (is.na(aov.sum$pval.adjustments[j, "GG eps"]) == F && aov.sum$pval.adjustments[j, "GG eps"] >= .90) {
              
              aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                             conf.level = conf.level,
                                             df.1 <- aov.sum$univariate.tests[i + 1, "num Df"],
                                             df.2 <- aov.sum$univariate.tests[i + 1, "den Df"])
              break
            }
          } else {
            
            aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                           conf.level = conf.level,
                                           df.1 <- aov.sum$univariate.tests[i + 1, "num Df"],
                                           df.2 <- aov.sum$univariate.tests[i + 1, "den Df"])
          }
        }
      }
      
      #--------------------------------------------------------------------------------------------#
      # SPHERICITY ASSUMED
      if (epsilon == "none") {
        
        aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                       conf.level = conf.level,
                                       df.1 <- aov.sum$univariate.tests[i + 1, "num Df"],
                                       df.2 <- aov.sum$univariate.tests[i + 1, "den Df"])
      }
      
      #--------------------------------------------------------------------------------------------#
      # GREENHOUSE-GEISSER (GG) CORRECTION
      else if (epsilon == "GG") {
        
        # Search for factors with more than 2 levels
        for (j in 1:length(rownames(aov.sum$pval.adjustments))) {
          
          if (is.na(aov.sum$pval.adjustments[j, "GG eps"]) == F && rownames(aov.sum$pval.adjustments)[j] == rownames(aov$anova_table)[i]) {
            
            # Apply GG correction
            aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                           conf.level = conf.level,
                                           df.1 <- aov.sum$univariate.tests[i + 1, "num Df"] * aov.sum$pval.adjustments[j, "GG eps"],
                                           df.2 <- aov.sum$univariate.tests[i + 1, "den Df"] * aov.sum$pval.adjustments[j, "GG eps"])
            break
          } else {
            
            aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                           conf.level = conf.level,
                                           df.1 <- aov.sum$univariate.tests[i + 1, "num Df"],
                                           df.2 <- aov.sum$univariate.tests[i + 1, "den Df"])
          }
        }
      }
      
      #--------------------------------------------------------------------------------------------#
      # HUYNH-FELDT (HF) CORRECTION
      else if (epsilon == "HF") {
        
        # Search for factors with more than 2 levels
        for (j in 1:length(rownames(aov.sum$pval.adjustments))) {
          
          if (is.na(aov.sum$pval.adjustments[j, "GG eps"]) == F && rownames(aov.sum$pval.adjustments)[j] == rownames(aov$anova_table)[i]) {
            
            # Apply HF correction
            aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                           conf.level = conf.level,
                                           df.1 <- aov.sum$univariate.tests[i + 1, "num Df"] * aov.sum$pval.adjustments[j, "HF eps"],
                                           df.2 <- aov.sum$univariate.tests[i + 1, "den Df"] * aov.sum$pval.adjustments[j, "HF eps"])
            break
          } else {
            
            aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$univariate.tests[i + 1, "F value"], 
                                           conf.level = conf.level,
                                           df.1 <- aov.sum$univariate.tests[i + 1, "num Df"],
                                           df.2 <- aov.sum$univariate.tests[i + 1, "den Df"])
          }
        }
      }
    } else {
      aov.sum.lim <- conf.limits.ncf(F.value = aov.sum$F[i], conf.level = .90, 
                                     df.1 <- aov.sum$"num Df"[i], 
                                     df.2 <- aov.sum$"den Df"[i])
    }
    
    #--------------------------------------------------------------------------------------------#
    # LOWER LIMIT
    aov.sum.lower_lim <- ifelse(is.na(aov.sum.lim$Lower.Limit/(aov.sum.lim$Lower.Limit + df.1 + df.2 + 1)),
                                0,
                                aov.sum.lim$Lower.Limit/(aov.sum.lim$Lower.Limit + df.1 + df.2 + 1))
    
    # UPPER LIMIT
    aov.sum.upper_lim <- aov.sum.lim$Upper.Limit/(aov.sum.lim$Upper.Limit + df.1 + df.2 + 1)
    
    
    #--------------------------------------------------------------------------------------------#
    #----STORE RESULTS IN THE MATRIX
    matrix.es[i,]         <- matrix(c(aov$anova_table$pes[i],
                                      aov.sum.lower_lim,
                                      aov.sum.upper_lim),
                                    ncol = 3)
    
  }
  
  
  #----------------------------------------------------------------------------------------------#
  #----OUTPUT MATRIX WITH PARTIAL ETA-SQUARED EFFECT SIZE ESTIMATES AND THEIR CONFIDENCE INTERVAL
  
  # Rename rows and columns
  rownames(matrix.es) <- rownames(aov$anova_table)
  colnames(matrix.es) <- c("Partial eta-squared", 
                           paste(conf.level * 100, "% CI lower limit"), 
                           paste(conf.level * 100, "% CI upper limit"))
  
  # Output
  return(matrix.es)
}

# -------------------------------------- Miscellaneous  ----------------------------------------------------------

imput = function(x) {
  x[is.na(x)] <- 0 
return(x)
}

interact = function(x) {
  x$idXsession <- ifelse(x$session == 'third', x$id + 2000, x$id)
  return(x)
}


scale_this <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm) 

internal = function(data, number){
  baseINTERN = subset(intern, phase == number)
  data = merge(x = get(data), y = baseINTERN[ , c("thirsty", 'hungry', 'id')], by = "id", all.x=TRUE)
  return(data)
}

diffX = function(x){
  x$diff_BMI = x$BMI_t1 - x$BMI_t2
  x$diff_BMIz = x$BMI_t1 - x$BMI_t2
  return(x)
}

# diffXglu = function(x){
#   x$glu = x$Fast_glu_third - x$Fast_glu_second
#   return(x)
# }

idXses = function(x){
  x$idXsession = as.numeric(x$id) * as.numeric(x$session)
  return(x)
}

facID = function(x){
  x$id = as.factor(x$id)
  return(x)
}

`%notin%` <- Negate(`%in%`)

# imput = function(x) {
#   x[is.na(x)] <- 0 
#   return(x)
# }




# STAN FUNCTIONS ----------------------------------------------------------

# Create a custom family that is logit if y = 0, normal/gaussian if not
hurdle_gaussian <- 
  custom_family("hurdle_gaussian", 
                dpars = c("mu", "sigma", "hu"),
                links = c("identity", "log", "logit"),
                lb = c(NA, NA, 0),
                type = "real")

stan_funs <- "
  real hurdle_gaussian_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) +
             normal_lpdf(y | mu, sigma);
    }
  }
"




stanvars <- stanvar(scode = stan_funs, block = "functions")


# Adapted from posterior_predict_hurdle_lognormal at
# https://github.com/paul-buerkner/brms/blob/master/R/posterior_predict.R#L736
posterior_predict_hurdle_gaussian <- function(i, prep, ...) {
  theta <- prep$dpars$hu[, i]
  mu <- prep$dpars$mu[, i]
  sigma <- prep$dpars$sigma
  ndraws <- prep$nsamples
  
  hu <- runif(ndraws, 0, 1)
  ifelse(hu < theta, 0, rnorm(ndraws, mu, sigma))
}

posterior_epred_hurdle_gaussian <- function(prep) {
  with(prep$dpars, mu * (1 - hu))
}



# bayes -------------------------------------------------------------------





plotHDI <- function(sample   = NULL,
                    credMass = 0.95,
                    Title    = NULL,
                    xLab     = "Value",
                    yLab     = "Density",
                    fontSize = NULL,
                    binSize  = 30,
                    ...) {
  
  # To pass R CMD Checks (serves no other purpose than to create binding)
  ..density.. <- NULL
  
  HDI <- hBayesDM::HDIofMCMC(as.vector(t(sample)), credMass = credMass)  # 'sample' w/ data.frame class is also fine..
  sample_df <- data.frame(sample)
  
  h1 <- ggplot(sample_df, aes(x = sample)) +
    ggplot2::theme_bw() +
    geom_histogram(aes(y = ..density..), colour = "grey", fill = "blue", alpha = 0.3, bins = binSize, ...) +
    ggtitle(Title) + xlab("Estimate") + ylab(yLab) +
    geom_segment(aes(x = HDI[1], y = 0, xend = HDI[2], yend = 0), size = 1.5, colour = "darkblue") +
    theme(axis.text.x = ggplot2::element_text(size = fontSize)) +
    theme(axis.text.y = ggplot2::element_text(size = fontSize)) +
    theme(axis.title.y = ggplot2::element_text(size = fontSize)) +
    theme(axis.title.x = ggplot2::element_text(size = fontSize)) +
    theme(plot.title = ggplot2::element_text(size = fontSize))
  
  #print(paste0(credMass*100, "% Highest Density Interval (HDI):"))
  #print(paste0("Lower bound = ", round(HDI[1], 4), ", Upper bound = ", round(HDI[2], 4)))
  return(h1)
}
