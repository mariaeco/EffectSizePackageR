# ============================================================================================================
# CONFIDENCE INTERVAL FOR EFFECT SIZE FUNCTION
# Author: Maria Marcolina L. Cardoso & Daniel Alexandre S. Gomes
# Version: 1.0
#
#
# r = The number of simulations replicates to use.
#
# ============================================================================================================


ciEffect <- function(r, database){

  with(database,{
    #library(simpleboot)# IC calculation
    #library(boot)
    #library(ggplot2)
    
    
    n_variables = length(levels(as.factor((database$var))))
    n = length(levels(as.factor((result$effect))))
    effectName = c()
    fatorName = c()
    media = c()
    ICpos = c()
    ICneg = c()
    
    
    for (k in 1:n_variables){
      
      nameFactor = levels(as.factor(database$var))[k]
      mydata = subset(database, var==nameFactor)
      
      for (i in 1:n) {
        
        effect_n = levels(as.factor(mydata$effect))[i]
        x = mydata$value[mydata$effect==effect_n]  
        ef <- one.boot(x, mean, sd, R= r, sim="parametric", student = FALSE, na.rm=T)
        ef_ci <- boot.ci(ef, conf = 0.95, type ="norm", na.rm=T)
        
        media = c(media,mean(ef$t  , na.rm=T))
        ICpos = c(ICpos, ef_ci$normal[,3])
        ICneg = c(ICneg,ef_ci$normal[,2])
        effectName = c(effectName,levels(as.factor(mydata$effect))[i])
        fatorName = c(fatorName,nameFactor)
        
      }
    }
    
    resEffects = data.frame(var = fatorName, effect = effectName, meanEffect = media, ICpos, ICneg);
    
    lim_y = max(abs(resEffects$ICpos),abs(resEffects$ICneg))
    
    figure  = qplot(x    =  effect, y    =  meanEffect, fill=effect,data =  resEffects) + 
              facet_wrap(~var, scales="free")+
              scale_fill_brewer(palette = "Set1")+
              geom_errorbar(aes(ymin = ICneg, ymax = ICpos, width = 0.15))+
              geom_point(shape = 22, size = 4, show.legend = FALSE)+
              geom_hline(yintercept=0, linetype="dashed", color = "red")+
              ylim(-lim_y,lim_y)+
              ylab("Effect Size")+
              xlab("Effect")+
              theme_bw()+
              theme(legend.position = "none")

    return(list(figure,resEffects))
    
      })
}





