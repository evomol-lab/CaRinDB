CaRinDB1 <- vroom::vroom("data/CaRinDB.csv",
  show_col_types = FALSE
)

CaRinDB2 <- vroom::vroom("data/PDB_Final.tsv",
  show_col_types = FALSE
)


# names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
names(CaRinDB2)[names(CaRinDB2) == "Tecido"] <- "Tissue"
names(CaRinDB2)[names(CaRinDB2) == "Risco_Mut_EFF"] <- "Risc_Mut_EFF"
names(CaRinDB1)[names(CaRinDB1) == "Deleterious"] <- "Deleteria"
names(CaRinDB1)[names(CaRinDB1) == "Deleterious5"] <- "Deleteria5"
names(CaRinDB1)[names(CaRinDB1) == "Deleterious10"] <- "Deleteria10"

setdiff(names(CaRinDB1), names(CaRinDB2))
# [1] "pdbx_seq_one_letter_code" "Inter_HBOND_Lig_tot"      "Inter_PIPISTACK_Lig_tot"  "Inter_IONIC_Lig_tot"
# [5] "Inter_SSBOND_Lig_tot"     "Inter_PICATION_Lig_tot"   "Inter_IAC_Res_tot"

setdiff(names(CaRinDB2), names(CaRinDB1))
# [1] "am_pathogenicity" "am_class"     "changeProt_am"    "Deleteria11"


## - Quais são as colunas únicas com identificadores? ----
dim(CaRinDB1)
# [1] 26893    99

dim(CaRinDB2)
# [1] 24792    96

dim(unique(CaRinDB1[, c("Tissue", "SNP_ID_COMMON", "POS", "Point_Mutation_EFF", "essencialChange")]))
# [1] 26891     5

dim(unique(CaRinDB1[, c("Tissue", "SNP_ID_COMMON", "Point_Mutation_EFF", "betweennessWeighted_node")]))
# [1] 26893     4


CaRinDB <- merge(CaRinDB1, CaRinDB2[, c("am_pathogenicity", "am_class", "changeProt_am", "Deleteria11", "Tissue", "SNP_ID_COMMON", "Point_Mutation_EFF", "betweennessWeighted_node")],
  by = c("Tissue", "SNP_ID_COMMON", "Point_Mutation_EFF", "betweennessWeighted_node"),
  all.x = TRUE, all.y = FALSE
)

save(CaRinDB, file = "data/CaRinDB.RData", compress = T)


#------------------------------------------------------------------

CaRinAF <- vroom::vroom("data/CaRinAF.tsv",
  # n_max = 50,
  delim = "\t",
  show_col_types = TRUE
)


save(CaRinAF, file = "data/CaRinAF.RData", compress = T)
#------------------------------------------------------------------

# install.packages("arsenal")
library(arsenal)

summary(comparedf(CaRinDB1, CaRinDB2))


# comparedf(CaRinDB1, CaRinDB2)
# Compare Object
#
# Function Call:
#   comparedf(x = CaRinDB1, y = CaRinDB2)
#
# Shared: 86 non-by variables and 24792 observations.
# Not shared: 23 variables and 2101 observations.
#
# Differences found in 82/84 variables compared.
# 0 variables compared have non-identical attributes.
