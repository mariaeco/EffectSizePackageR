# ============================================================================================================
# EFFECT SIZE FUNCTION
# Method: Log Ratio -> effectsize = log(value on treatment/ value on control)
# Author: Maria Marcolina L. Cardoso & Daniel Alexandre S. Gomes
# Version: 1.0.0
#
# DESCRIPTION:
#   Calculate the effect size of a treatment by the method Log Ratio.
#   (Incluir outros métodos) <=============
#   
#
# USAGE:
#   effectSize(x, index, treatments, controls,data=dataframe) # method="logratio" <- quando incluir outros métodos
#
# ARGUMENTS
#   x             A vector object containing the value for analysis
#   index         A list of one or more factors. 
#   treatments    Vector containing the names of the treatments. Ex. treat = c("TreatA","TreatB")
#   controls      Vector containing the names of the controls. Ex. contr = c("Control")
#   method        The method to be applied: logratio, difference, 
#   data          The complete dataframe to use
#
# DETAILS:
#   The effect size is calculated using a randomized process, taking into account each combination of
#   treatment and control. Example:If the treatment has three replicates, and the control 3 replicates,
#   each replicate of treatment will be combined with each replicate of the control. Therefore, a 3x3 
#   combination results in 6 calculated effects sizes. All treatments and controls must have the same number of
#   replicates, but controls do not need to have the same number of replicate from treatments. 
#   The function works well with NA, so if there is missing data in the database, include the value as NA.
#
#
# VALUE:
#   A vector containing the effect sizes by treatment and variable used.
#   Positive and negative values indicates a positive and negative effect of the treatment, respectively; 0 means no treatmente effect  
#
# REFERENCES:
# 
# EXEMPLES:
#
#
# ****TRABALHAR EM:
#                   - TRATAMENTOS COM NUMEROS DE REPLICAS DIFERENTES
#                   - CONECTAR COMO UM PACOTE (PARA SER PUXADO COMO UM LIBRARY)
#                   - MELHORAR OS GRAFICOS
#                   - ESCREVER BEM AS DESCRICOES DOS PACOTES, COMO EH FEITO NOS PACOTES DO R
#                   - INCLUIR OUTROS MÉTODOS DE EFFECT SIZE
#                   - E MÉTODOS DE BOOTSTRAP, MONTE CARLO
# ============================================================================================================


effectSize <- function(index,treatments,controls, database){
        
  with(database,{
        
        
        #treatments = treatments
        #controls = controls
        
        n_variables = length(levels(as.factor(variable)))  #number of variables to be analysed
        repControl = length(values[Treatment==controls[1]])/n_variables # N of replicates of controls
        repTreatment = length(values[Treatment==treatments[1]])/n_variables  #N of replicates of treatments
                #criar 4 matriz para o resultado final
        
        treatm = c() # para armazenar temporariamente os dados dos tratamentos
        control = c()   # para armazenar temporariamente os dados do controle
        mylist = c()# para armazenar o resultado final
        effectName = c()
        fatorName = c()
        
        for (k in 1:n_variables){
          
          nomeVar = levels(as.factor(variable))[k]
          mydata = subset(database, variable==nomeVar)
          value = mydata$values
          my_treat = mydata$Treatment
          
         # attach(mydata)
          
              for (z in 1:length(treatments)){
                treatm[z] = list(matrix(0,repTreatment))
                treatm[[z]] = value[my_treat==treatments[z]]
                efeito = levels(as.factor(treatments))[z]
                #(paste0("treatm", z), treatm[[z]])
                
                for (x in 1:length(controls)){
                  control[x] = list(matrix(0,1))
                  control[[x]] = value[my_treat==controls[x]]
                  
                  for (j in 1:repControl){    # REPLICAS DOS CONTROLES
                    
                    #assign(paste0("control", x), control[[x]])
                    res = log(treatm[[z]]/control[[x]][j])
                    mylist = c(mylist,res)
                  }
                }
              }
        
        #detach(mydata)
        combination = repControl*repTreatment
        n_controls = length(controls)
        n_treatments = length(treatments)
        treat = rep(rep(as.factor(treatments), each=combination*n_controls),n_variables);treat
        contr = rep(rep(as.factor(controls), each=combination),n_treatments*n_variables);contr
        var = rep(levels(as.factor(variable)), each=combination*n_treatments*n_controls); var
        
        response = data.frame( var,treat,contr,effect=paste0(treat,"/",contr) ,value = mylist)
 
        return(response)
        }
  })
}
















