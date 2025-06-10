library(tidyverse)
library(tidyverse)
ecole <- arrow::read_parquet("data/ecole.parquet") |> 
  dplyr::filter(CODE_TYPE_ANNEE != 19,!is.na(DRE)) 

ecole$Eff_G_tot = rowSums(ecole[,c("An2_G_CM2","An2_G_CM1","An2_G_CE2","An2_G_CE1","An2_G_CP2","An2_G_CP1")],na.rm = TRUE)
ecole$Eff_F_tot = rowSums(ecole[,c("An2_F_CM2","An2_F_CM1","An2_F_CE2","An2_F_CE1","An2_F_CP2","An2_F_CP1")],na.rm = TRUE)
ecole$Eff_tot = rowSums(ecole[,c("Eff_G_tot","Eff_F_tot")],na.rm = TRUE)
ecole$Eff_Ens = rowSums(ecole[,c("Ens_H","Ens_F")],na.rm = TRUE)


myTaux <- function(Num,denom){
  taux = ifelse((is.na(denom) | denom == 0),0,(Num/denom))
  return(taux)
}



df_indic <- function(base){
    base |>
    mutate(
      ## Total section
      an2_nb = An2_NB_CP1 + An2_NB_CP2 + An2_NB_CE1 + An2_NB_CE2 + An2_NB_CM1 + An2_NB_CM2,
      ## Effectif total
      eff_CP1 = An2_G_CP1 + An2_F_CP1,
      eff_CP2 = An2_G_CP2 + An2_F_CP2,
      eff_CE1 = An2_G_CE1 + An2_F_CE1,
      eff_CE2 = An2_G_CE2 + An2_F_CE2,
      eff_CM1 = An2_G_CM1 + An2_F_CM1,
      eff_CM2 = An2_G_CM2 + An2_F_CM2,
      ## Pourcentage de fille
      PrcF_CP1 = myTaux(An2_F_CP1,eff_CP1),
      PrcF_CP2 = myTaux(An2_F_CP2,eff_CP2),
      PrcF_CE1 = myTaux(An2_F_CE1,eff_CE1),
      PrcF_CE2 = myTaux(An2_F_CE2,eff_CE2),
      PrcF_CM1 = myTaux(An2_F_CM1,eff_CM1),
      PrcF_CM2 = myTaux(An2_F_CM2,eff_CM2), 
      PrcF_Tot = myTaux((An2_F_CP1+An2_F_CP2+An2_F_CE1+An2_F_CE2+An2_F_CM1+An2_F_CM2),(eff_CP1+eff_CP2+eff_CE1+eff_CE2+eff_CM1+eff_CM2)),
      
      ## Taux de promotion des garçons
      TP_G_CP2 = myTaux(An2_G_CP1 - An2_G_Red_CP1,An1_G_CP1),
      TP_G_CE1 = myTaux(An2_G_CP2 - An2_G_Red_CP2,An1_G_CP2),
      TP_G_CE2 = myTaux(An2_G_CE1 - An2_G_Red_CE1,An1_G_CE1),
      TP_G_CM1 = myTaux(An2_G_CE2 - An2_G_Red_CE2,An1_G_CE2),
      TP_G_CM2 = myTaux(An2_G_CM1 - An2_G_Red_CM1,An1_G_CM1),
      ## Taux de promotion des filles
      TP_F_CP2 = myTaux(An2_F_CP1 - An2_F_Red_CP1,An1_F_CP1),
      TP_F_CE1 = myTaux(An2_F_CP2 - An2_F_Red_CP2,An1_F_CP2),
      TP_F_CE2 = myTaux(An2_F_CE1 - An2_F_Red_CE1,An1_F_CE1),
      TP_F_CM1 = myTaux(An2_F_CE2 - An2_F_Red_CE2,An1_F_CE2),
      TP_F_CM2 = myTaux(An2_F_CM1 - An2_F_Red_CM1,An1_F_CM1),
      ## Enesemble
      TP_CP2 = myTaux((An2_G_CP1 + An2_F_CP1) - (An2_G_Red_CP1 + An2_F_Red_CP1), (An1_G_CP1 + An1_F_CP1 )),
      TP_CE1 = myTaux((An2_G_CP2 + An2_F_CP2) - (An2_G_Red_CP2 + An2_F_Red_CP2), (An1_G_CP2 + An1_F_CP2 )),
      TP_CE2 = myTaux((An2_G_CE1 + An2_F_CE1) - (An2_G_Red_CE1 + An2_F_Red_CE1), (An1_G_CE1 + An1_F_CE1 )),
      TP_CM1 = myTaux((An2_G_CE2 + An2_F_CE2) - (An2_G_Red_CE2 + An2_F_Red_CE2), (An1_G_CE2 + An1_F_CE2 )),
      TP_CM2 = myTaux((An2_G_CM1 + An2_F_CM1) - (An2_G_Red_CM1 + An2_F_Red_CM1), (An1_G_CM1 + An1_F_CM1 )),
      
      ## Taux de retention
      
      TR_G = (TP_G_CP2*TP_G_CE1*TP_G_CE2*TP_G_CM1*TP_G_CM2),
      TR_F = (TP_F_CP2*TP_F_CE1*TP_F_CE2*TP_F_CM1*TP_F_CM2),
      TR_T = (TP_CP2*TP_CE1*TP_CE2*TP_CM1*TP_CM2),
      
      ## Taux de redoublement
      TRed_CP1 = myTaux((An2_F_Red_CP1 + An2_G_Red_CP1),(An1_F_CP1 + An1_G_CP1)),
      TRed_CP2 = myTaux((An2_F_Red_CP2 + An2_G_Red_CP2),(An1_F_CP2 + An1_G_CP2)),
      TRed_CE1 = myTaux((An2_F_Red_CE1 + An2_G_Red_CE1),(An1_F_CE1 + An1_G_CE1)),
      TRed_CE2 = myTaux((An2_F_Red_CE2 + An2_G_Red_CE2),(An1_F_CE2 + An1_G_CE2)),
      TRed_CM1 = myTaux((An2_F_Red_CM1 + An2_G_Red_CM1),(An1_F_CM1 + An1_G_CM1)),
      TRed_CM2 = myTaux((An2_F_Red_CM2 + An2_G_Red_CM2),(An1_F_CM2 + An1_G_CM2)),
      Eff_red = An2_F_Red_CP1 + An2_G_Red_CP1+An2_F_Red_CP2 + An2_G_Red_CP2+An2_F_Red_CE1 + An2_G_Red_CE1+An2_F_Red_CE2 + An2_G_Red_CE2+An2_F_Red_CM1 + An2_G_Red_CM1+An2_F_Red_CM2 + An2_G_Red_CM2,
      Eff_T_An1 = An1_F_CP1 + An1_G_CP1+An1_F_CP2 + An1_G_CP2+An1_F_CE1 + An1_G_CE1+An1_F_CE2 + An1_G_CE2+An1_F_CM1 + An1_G_CM1+An1_F_CM2 + An1_G_CM2,
      TRed_T = myTaux(Eff_red,Eff_T_An1),

      ## Ratio Manuel Français Elèves
      RMFE_CP1 = round(myTaux(Lecture_CP1,eff_CP1),1),
      RMFE_CP2 = round(myTaux(Lecture_CP2,eff_CP2),1),
      RMFE_CE1 = round(myTaux(Lecture_CE1,eff_CE1),1),
      RMFE_CE2 = round(myTaux(Lecture_CE2,eff_CE2),1),
      RMFE_CM1 = round(myTaux(Lecture_CM1,eff_CM1),1),
      RMFE_CM2 = round(myTaux(Lecture_CM2,eff_CM2),1),
      ManF_T = Lecture_CP1+Lecture_CP2+Lecture_CE1+Lecture_CE2+Lecture_CM1+Lecture_CM2,
      RMFE_T = round(myTaux(ManF_T,Eff_tot),1),
      ## Ratio Manuel Math Elèves
      RMME_CP1 = round(myTaux(Calcul_CP1,eff_CP1),1),
      RMME_CP2 = round(myTaux(Calcul_CP2,eff_CP2),1),
      RMME_CE1 = round(myTaux(Calcul_CE1,eff_CE1),1),
      RMME_CE2 = round(myTaux(Calcul_CE2,eff_CE2),1),
      RMME_CM1 = round(myTaux(Calcul_CM1,eff_CM1),1),
      RMME_CM2 = round(myTaux(Calcul_CM2,eff_CM2),1),
      ManM_T = Calcul_CP1+Calcul_CP2+Calcul_CE1+Calcul_CE2+Calcul_CM1+Calcul_CM2,
      RMME_T = round(myTaux(ManM_T,Eff_tot),1),
    )
}
df_ecole <- df_indic(ecole)

arrow::write_parquet(df_ecole,"data/df_ecole.parquet") 
