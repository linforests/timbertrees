## biomod2 scripts for Species Distribution Models £¨SDMs£©. 
## This script framework adopts the methodology outlined in Damien Georges's scripts and integrates standard procedures for constructing SDMs.
## 
##
## The original citation of this script:
## author: Damien Georges
## date: 2020-05-13
## licence: GPL2
##
## Credit:
## This example has been inspired by the book
## **Habitat Suitability and Distribution Models** with Applications in R
## **by A. Guisan (1), W. Thuiller (2), N.E. Zimmermann (3) **
## with contribution by V. Di Cola, D. Georges and A. Psomas
## _(1) University of Lausanne, Switzerland_
## _(2) CNRS, Universit¨¦ Grenoble Alpes, France_
## _(3) Swiss Federal Research Institute WSL, Switzerland_
## Cambridge University Press
## http://www.cambridge.org/gb/academic/subjects/life-sciences/quantitative-biology-biostatistics-and-mathematical-modellin/habitat-suitability-and-distribution-models-applications-r
## 

setwd("D:/data_biomod2") 

## load the required packages
library(biomod2)
library(raster)
library(rasterVis)
library(gridExtra)
library(reshape2)

## read data ----
## species occurences data
data <- read.csv('test_occ_all_addlast.csv', stringsAsFactors = FALSE)
head(data)
table(data$species)
spp_to_model <- unique(data$species)

## curent climatic variables
stk_current <- 
  raster::stack(
    c(
      bio_1 =  "Now/bio_1.asc",
      bio_2 =  "Now/bio_2.asc",
      bio_7 =  "Now/bio_7.asc",
      bio_12 =  "Now/bio_12.asc",
      bio_14 =  "Now/bio_14.asc",
      bio_15 =  "Now/bio_15.asc",
      DEM_ele= "Now/DEMnow/DEMasc/dem_ele.asc",
      DEM_aspect = "Now/DEMnow/DEMasc/dem_aspect.asc",
      DEM_slope = "Now/DEMnow/DEMasc/dem_slope.asc"
    ),
    RAT = FALSE
  )

## 2100_BC126 climatic variables
stk_ssp126 <- 
  raster::stack(
    c(
      bio_1 =  "Future/ssp126/ssp126asc/bio1.asc",
      bio_2 =  "Future/ssp126/ssp126asc/bio2.asc",
      bio_7 =  "Future/ssp126/ssp126asc/bio7.asc",
      bio_12 =  "Future/ssp126/ssp126asc/bio12.asc",
      bio_14 =  "Future/ssp126/ssp126asc/bio14.asc",
      bio_15 =  "Future/ssp126/ssp126asc/bio15.asc",
      DEM_ele= "Future/ssp126/DEMasc/dem_ele.asc",
      DEM_aspect = "Future/ssp126/DEMasc/dem_aspect.asc",
      DEM_slope = "Future/ssp126/DEMasc/dem_slope.asc"
    ),
    RAT = FALSE
  )

## 2100_BC_245 climatic variables
stk_ssp245 <- 
  raster::stack(
    c(
      bio_1 =  "Future/ssp245/ssp245ASC/bio1.asc",
      bio_2 =  "Future/ssp245/ssp245ASC/bio2.asc",
      bio_7 =  "Future/ssp245/ssp245ASC/bio7.asc",
      bio_12 =  "Future/ssp245/ssp245ASC/bio12.asc",
      bio_14 =  "Future/ssp245/ssp245ASC/bio14.asc",
      bio_15 =  "Future/ssp245/ssp245ASC/bio15.asc",
      DEM_ele = "Future/ssp245//DEMasc/dem_ele.asc",
      DEM_aspect = "Future/ssp245/DEMasc/dem_aspect.asc",
      DEM_slope = "Future/ssp245/DEMasc/dem_slope.asc"
    ),
    RAT = FALSE 
  )

## 2100_BC_370 climatic variables
stk_ssp370 <- 
  raster::stack(
    c(
      bio_1 =  "Future/ssp370/ssp370asc/bio1.asc",
      bio_2 =  "Future/ssp370/ssp370asc/bio2.asc",
      bio_7 =  "Future/ssp370/ssp370asc/bio7.asc",
      bio_12 =  "Future/ssp370/ssp370asc/bio12.asc",
      bio_14 =  "Future/ssp370/ssp370asc/bio14.asc",
      bio_15 =  "Future/ssp370/ssp370asc/bio15.asc",
      DEM_ele = "Future/ssp370/DEMasc/dem_ele.asc",
      DEM_aspect = "Future/ssp370/DEMasc/dem_aspect.asc",
      DEM_slope = "Future/ssp370/DEMasc/dem_slope.asc"
    ),
    RAT = FALSE
  )

## 2100_BC_585 climatic variables
stk_ssp585 <- 
  raster::stack(
    c(
      bio_1 =  "Future/ssp585/ssp585asc/bio1.asc",
      bio_2 =  "Future/ssp585/ssp585asc/bio2.asc",
      bio_7 =  "Future/ssp585/ssp585asc/bio7.asc",
      bio_12 =  "Future/ssp585/ssp585asc/bio12.asc",
      bio_14 =  "Future/ssp585/ssp585asc/bio14.asc",
      bio_15 =  "Future/ssp585/ssp585asc/bio15.asc",
      DEM_ele = "Future/ssp585/DEMasc/dem_ele.asc",
      DEM_aspect = "Future/ssp585/DEMasc/dem_aspect.asc",
      DEM_slope = "Future/ssp585/DEMasc/dem_slope.asc"
    ),
    RAT = FALSE
  )

## build species modelling wrapper ----
biomod2_wrapper <- function(sp){
  cat("\n> species : ", sp)
  
  ## get occurrences points
  sp_dat <- data[data$species == sp, ]
  
  ## formating the data
  set.seed(1234) ## Reproducible pseudo-absences
  
  sp_format <- 
    BIOMOD_FormatingData(
      resp.var = rep(1, nrow(sp_dat)), 
      expl.var = stk_current,
      resp.xy = sp_dat[, c("long", "lat")],
      resp.name = sp,
      PA.strategy = "random", 
      PA.nb.rep = 1, 
      PA.nb.absences = 10000
    )
  
  ## print formatting summary
  sp_format
  
  ## save image of input data summary
  if(!exists(sp)) dir.create(sp)
  pdf(paste(sp, "/", sp ,"_data_formated.pdf", sep="" ))
  try(plot(sp_format))
  dev.off()
  
  ## define models options
  sp_opt <- BIOMOD_ModelingOptions(
    GLM=list(type="quadratic", interaction.level=0, myFormula=NULL,
             family=binomial(link="logit"), test="AIC"),
    GAM=list(algo="GAM_mgcv", type="s_smoother", k=4, interaction.level=0, 
             myFormula=NULL, 
             family=binomial(link="logit")),
    RF=list(do.classif=TRUE, ntree=500),
    MAXENT.Phillips=list(path_to_maxent.jar= getwd(), 
                         visible=FALSE, maximumiterations=500,
                         memory_allocated=512,
                         # To avoid overparametrization (Merow  et al.  2013)
                         product=FALSE, threshold=FALSE, hinge=FALSE)
  )
  
  ## model species
  sp_model <- BIOMOD_Modeling( 
    sp_format, 
    models = c("GLM","GAM","RF","MAXENT.Phillips"), 
    models.options = sp_opt, 
    NbRunEval = 5, 
    DataSplit = 70, 
    Yweights = NULL, 
    VarImport = 3, 
    models.eval.meth = c('TSS', 'ROC'),
    SaveObj = TRUE,
    rescal.all.models = FALSE,
    do.full.models = FALSE,
    modeling.id = "demo2"
  )
  
  ## save some graphical outputs
  #### models scores
  pdf(paste0(sp, "/", sp , "_models_scores.pdf"))
  try(gg1 <- models_scores_graph(sp_model, metrics = c("TSS", "ROC"), by = 'models', plot = FALSE))
  try(gg2 <- models_scores_graph(sp_model, metrics = c("TSS", "ROC"), by = 'data_set', plot = FALSE))
  try(gg3 <- models_scores_graph(sp_model, metrics = c("TSS", "ROC"), by = 'cv_run', plot = FALSE))
  try(grid.arrange(gg1, gg2, gg3))
  dev.off()
  
  ## build ensemble models
  sp_ens_model <- 
    BIOMOD_EnsembleModeling(
      modeling.output = sp_model,
      em.by = 'all',
      eval.metric = 'TSS',
      eval.metric.quality.threshold = 0.6,
      models.eval.meth = c('TSS','ROC'),
      prob.mean = FALSE,
      prob.mean.weight = TRUE,
      VarImport = 0
    )
   
  ## Calculate variable importance
  VarImp <- as.data.frame(get_variables_importance(sp_model))
  Rank <- as.data.frame(apply(-VarImp,2,rank))
  VarImp$mean.rank <- apply(Rank,1,mean)
  VarImp$rank <- rank(VarImp$mean.rank,ties.method="max")
  write.table(VarImp,paste0(sp,"_varimp.txt"),sep="\t")
  
  ## do projections
  proj_scen <- c("current","ssp126","ssp245","ssp370","ssp585")
  
  for(scen in proj_scen){
    cat("\n> projections of ", scen)
    
    ## single model projections
    sp_proj <- 
      BIOMOD_Projection(
        modeling.output = sp_model,
        new.env = get(paste0("stk_", scen)),
        proj.name = scen,
        selected.models = 'all',
        binary.meth = "TSS",
        filtered.meth = NULL,
        compress = TRUE,
        build.clamping.mask = FALSE,
        do.stack = FALSE,
        output.format = ".img"
      )
    
    ## ensemble model projections
    sp_ens_proj <- 
      BIOMOD_EnsembleForecasting(
        EM.output = sp_ens_model,
        projection.output = sp_proj,
        binary.meth = "TSS",
        compress = TRUE,
        do.stack = FALSE,
        output.format = ".img"
      )
  }
  
  return(paste0(sp," modelling completed !"))
}
