# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# ==== Loading library ===============================================================
# if(!require(uuid)){ install.packages("uuid") }
# if(!require(curl)){ install.packages("curl") }
# if(!require(remotes)){install.packages("remotes")}
# if(!require(GAlogger)){ remotes::install_github("bnosac/GAlogger") }

# ga_set_tracking_id("G-34TE3RG6BK")
# ga_set_approval(consent = TRUE)
# https://bioinfo.imd.ufrn.br/dbPepVar/#tab-9985-3
# ga_collect_pageview(page = "/dbPepVar")
# ga_collect_pageview(page = "#tab-9985-2", title = "Variants")
# ga_collect_pageview(page = "#tab-9985-3", title = "Evidence tables")
# ga_collect_pageview(page = "#tab-9985-4", title = "Proteogenomics Viewer")
# ga_collect_pageview(page = "#tab-9985-5", title = "Donwload dataset")
# ga_collect_pageview(page = "/dbPepVar", title = "Homepage", hostname = "bioinfo.imd.ufrn.br")

if (!require(memoise, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("memoise", quiet = TRUE)
}
if (!require(shiny, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("shiny", quiet = TRUE)
}
if (!require(shinyWidgets, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("shinyWidgets", quiet = TRUE)
}
if (!require(htmltools, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("htmltools", quiet = TRUE)
}
if (!require(shinycssloaders, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("shinycssloaders", quiet = TRUE)
}
if (!require(shinythemes, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("shinythemes", quiet = TRUE)
}
# if(!require(shinyjs, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('shinyjs', quiet=TRUE) }
if (!require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("ggplot2", quiet = FALSE)
}
if (!require(DT, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("DT", quiet = TRUE)
}
if (!require(dplyr, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("dplyr", quiet = TRUE)
}
if (!require(tidyr, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("tidyr", quiet = TRUE)
}
if (!require(vroom, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("vroom", quiet = TRUE)
}
if (!require(plotly, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("plotly", quiet = TRUE)
}
if (!require(data.table, quietly = TRUE, warn.conflicts = FALSE)) {
  install.packages("data.table", quiet = TRUE)
}
# if(!require(Rcpp, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('Rcpp', quiet=TRUE) }
# if(!require(magick, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('magick', quiet=TRUE) }
# if(!require(summarytools, quietly=TRUE, warn.conflicts=FALSE)){ install.packages('summarytools', dependencies = T, quiet=TRUE) }
# if(!require(skimr, quietly=TRUE, warn.conflicts=FALSE)){ devtools::install_github("ropensci/skimr") }


# https://daattali.com/shiny/shinycssloaders-demo/
# https://github.com/daattali/shinycssloaders#usage
# https://projects.lukehaas.me/css-loaders/
if(!require(cicerone, quietly=TRUE, warn.conflicts=FALSE)){ 
  remotes::install_github("JohnCoene/cicerone@v1.0.4")
  #install.packages("cicerone", quiet=TRUE) 
}

# if(!require(magrittr)){ install.packages('magrittr') }
# if(!require(generics)){ install.packages('generics') }
# if(!require(DT)){ install.packages('DT') }
# if(!require(httpuv)){ install.packages('httpuv')}
# if(!require(promises)){ install.packages('promises')}
# if(!require(vctrs)){ install.packages('vctrs') }
# if(!require(lifecycle)){ install.packages('lifecycle') }
# if(!require(ellipsis)){ install.packages('ellipsis') }
# if(!require(crayon)){ install.packages('crayon') }
# if(!require(glue)){ install.packages('glue') }
# if(!require(fansi)){ install.packages('fansi') }
# if(!require(utf8)){ install.packages('utf8') }
# if(!require(pillar)){ install.packages('pillar') }
# if(!require(gtable)){ install.packages('gtable') }
# if(!require(colorspace)){ install.packages('colorspace') }
# if(!require(munsell)){ install.packages('munsell') }
# if(!require(pkgconfig)){ install.packages('pkgconfig') }
# if(!require(tibble)){ install.packages('tibble') }
# if(!require(withr)){ install.packages('withr') }
# if(!require(scales)){ install.packages('scales') }
# if(!require(ggplot2)){ install.packages('ggplot2') }
# if(!require(purrr)){ install.packages('purrr') }
# if(!require(tidyselect)){ install.packages('tidyselect') }
# if(!require(dplyr)){ install.packages('dplyr') }
# if(!require(tidyr)){ install.packages('tidyr') }
# if(!require(tzdb)){ install.packages('tzdb') }
# if(!require(vroom)){ install.packages('vroom') }
# if(!require(data.table)){ install.packages('data.table') }
# if(!require(httr)){ install.packages('httr') }
# if(!require(jsonlite)){ install.packages('jsonlite') }
# if(!require(lazyeval)){ install.packages('lazyeval') }
# if(!require(viridisLite)){ install.packages('viridisLite') }
# if(!require(plotly)){ install.packages('plotly')}
# if(!require(later)){ install.packages('later')}
# if(!require(bitops)){ install.packages('bitops')}
# #if(!require(RCurl)){ install.packages('RCurl')}
# if(!require(farver)){ install.packages('farver')}
# if(!require(terra)){ install.packages('terra')}
# if(!require(raster)){ install.packages('raster')}
# if(!require(shiny)){ install.packages('shiny')}
# if(!require(leaflet)){ install.packages('leaflet')}
# if(!require(leafem)){ install.packages('leafem')}

# ==== Set up caching ===============================================================
source("R/memoize.R")
# Configure memoization using a shared disk cache. The lifetime of this cache
# directory is the life of the R process; when the R process exits, it will
# be removed.
# cache_dir <- file.path(tempdir(), "bind-cache")
cache_dir <- file.path("./bind-cache")
# cache <- cachem::cache_disk(cache_dir, max_size = 1024 * 1024 * 1024, logfile = "bind-cache/log")
# Expire items in cache after 15 minutes
cache <- cachem::cache_mem(max_size = 500e6, max_age = 15 * 60)

memoize2 <- function(fn) {
  memoize(fn, cache = cache)
}
# Tell Shiny to also use this cache for renderCachedPlot
shinyOptions(cache = cache, shiny.trace = T)

# ==== Global Functions ===============================================================
img_uri <- memoize2(function(x) {
  sprintf('<img src="%s"/>', knitr::image_uri(x))
})
img_uri_icon <- memoize2(function(x) {
  sprintf('<img src="%s" width="18" height="18"/>', knitr::image_uri(x))
})
img_uri_favicon <- memoize2(function(x) {
  sprintf("%s", knitr::image_uri(x))
})


# - Add static link to:
#   * Uniprot  (Uniprot_id)
# https://www.uniprot.org/uniprotkb/Q9NRD1
# * PDB (PDB_id)
# https://www.rcsb.org/structure/3LIT
# * AlphaFold
# https://alphafold.ebi.ac.uk/entry/Q9NRD1 [uniprot_id]

link_uniprot <- memoize2(function(val) {
  sprintf('<a href="https://www.uniprot.org/uniprotkb/%s" target="_blank"><img src="%s"  width="90" height="20"/></a>', val, knitr::image_uri("icons/logo_uniprot.png"))
})

link_pdb <- memoize2(function(val) {
  sprintf('<a href="https://www.rcsb.org/structure/%s" target="_blank"><img src="%s"  width="80" height="17"/></a>', val, knitr::image_uri("icons/logo_pdb.png"))
})

link_af <- memoize2(function(val) {
  sprintf('<a href="https://alphafold.ebi.ac.uk/entry/%s" target="_blank"><img src="%s"  width="90" height="20"/></a>', val, knitr::image_uri("icons/logo_af.png"))
})

link_genecards <- memoize2(function(val) {
  sprintf('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s#publications" target="_blank"><img src="%s"  width="90" height="20"/></a>', val, knitr::image_uri("icons/genecards.png"))
})

link_snps <- memoize2(function(val) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/snp/%s#publications" target="_blank"><img src="%s" height="18"/></a>', val, knitr::image_uri("icons/logo_dbSNP.png"))
})

link_proteins <- memoize2(function(val) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/protein/%s" target="_blank"><img src="%s"  height="18"/></a>', val, knitr::image_uri("icons/logo_ncbi.gif"))
})

# ==== Support Functions ===============================================================
# source("R/plotly.R")
# source("R/plots.R")

# ==== Global variables ===============================================================

# ==== Loading CaRinDB ================================================================
# CaRinDB <- vroom::vroom("data/CaRinDB.csv",
#                        show_col_types = FALSE)

load("data/CaRinDB.RData")

CaRinDB <- CaRinDB %>%
  dplyr::mutate(
    SNP_search = link_snps(SNP_ID_COMMON),
    Gene_search = link_genecards(Gene_EFF),
    RefSeq_search = link_proteins(RefSeq_EFF),
    Uniprot_search = link_uniprot(Uniprot_id),
    PDB_search = link_pdb(PDB_id),
    AlphaFold_search = link_af(Uniprot_id)
  )

CaRinDB_cols <- names(CaRinDB)

CaRinDB <- CaRinDB %>%
  dplyr::select(c("Tissue", "Gene_EFF", "Gene_search", "SNP_ID_COMMON", "SNP_search", "RefSeq_EFF", "RefSeq_search", "Uniprot_search", "PDB_search", "AlphaFold_search", dplyr::all_of(CaRinDB_cols)))

CaRinDB <- CaRinDB %>%
  dplyr::mutate(am_class = ifelse(is.na(am_class), "unclassified", am_class))

tissue_dictionary <- read.csv("data/tissue_dictionary.csv")
tissues <- unique(tissue_dictionary$tissue)
names(tissues) <- paste0(unique(tissue_dictionary$full_name), " (", tissues, ")")

# AM Class color pallette
am_class_palette <- c(
  "benign" = "#ff7f0e",
  "pathogenic" = "#2ca02c",
  "ambiguous" = "#1f77b4",
  "unclassified" = "#d62728"
)

# CaRinAF <- vroom::vroom("data/CaRinAF.tsv",
#                         #n_max = 50,
#                         delim = '\t',
#                         show_col_types = TRUE)
load("data/CaRinAF.RData")

CaRinAF_cols <- names(CaRinAF)
CaRinAF <- CaRinAF %>%
  dplyr::mutate(
    SNP_search = link_snps(SNP_ID_COMMON),
    Gene_search = link_genecards(Gene_EFF),
    RefSeq_search = link_proteins(RefSeq_EFF),
    Uniprot_search = link_uniprot(Uniprot_id),
    AlphaFold_search = link_af(Uniprot_id)
  )

CaRinAF <- CaRinAF %>%
  dplyr::select(c("Tissue", "Gene_EFF", "Gene_search", "SNP_ID_COMMON", "SNP_search", "RefSeq_EFF", "RefSeq_search", "Uniprot_search", "AlphaFold_search", dplyr::all_of(CaRinAF_cols)))

CaRinAF <- CaRinAF %>%
  dplyr::mutate(am_class = ifelse(is.na(am_class), "unclassified", am_class))

tissues_AF <- unique(tissue_dictionary$tissue)
names(tissues_AF) <- paste0(unique(tissue_dictionary$full_name), " (", tissues_AF, ")")

callback <- JS(
  "var a = document.createElement('a');",
  "$(a).addClass('dt-button');",
  "a.href = document.getElementById('downloadDB').href;",
  "a.download = '';",
  "$(a).attr('target', '_blank');",
  "$(a).text('all data');",
  "$('div.dwnld').append(a);",
  "$('#downloadDB').hide();"
)

# summarytools::dfSummary(summarytools::dfSummary(CaRinDB, style="grid", method = "render"),
#                    #varnumbers   = FALSE,
#                    #valid.col    = FALSE,
#                    omit.headings = TRUE,
#                    bootstrap.css = FALSE,
#                    escape.pipe = TRUE,
#                    file = "./www/CaRinDB.html"
# )


guide <- Cicerone$
  new()$
  step(
    #"home",
    el = "[data-value='Home']",
    title = "Summary of CaRinDB",
    description = "<p>The first menu (Home) contains a general summary  of both DB and DB::AlphaFold datasets accessible through the portal.</p>
    <p>The main graph shows the number of mutations separated by type of cancer, and the pie chart shows the percentage of mutations classified according to the AlphaMissense class and the NDamage parameter, which represents the number of computational predictors that consider a particular mutation as deleterious.</p>
    <p>Users can also search the database using specific Gene Symbols.</p>",
    is_id = FALSE,
    #tab = "home",
    #tab_id = "home",
    position = "bottom"
  )$
  step(
    #el = "CaRinDB",
    el = "[data-value='CaRinDB']",
    title = "CaRinDB and CaRinDB::AlphaFold",
    description = "<p>The second and third menus provide specific and interactive access to the <strong>CaRinDB</strong> and <strong>CaRinDB::AlphaFold</strong> datasets, respectively. Here, users can perform data mining and generate insights for their research. This entire analysis can be performed for all 33 tissues, or for a specific tissue.</p>
    <p>The <strong>Summary</strong> tab provides a summary description of all attributes (columns) of the dataset.</p>
    <p>Upon clicking the <strong>Plots</strong> tab, the user can visualize a set of graphs regarding some key database features. They are: the number of mutations versus N DamageCalc (Number of predictors that classified this mutation as damaging), the number of mutations versus Inter_Res_Tot (Total residue-residue interactions), the number of mutations versus Betweenness Weighted (RIN parameter), the number of mutations versus the B-factor (RIN parameter) or pLDDT (AlphaFold parameter) of the present residue, and the number of mutations versus the classes of deleterious mutations.</p>
    <p>The next tab (<strong>Dataset</strong>) is a web visualization of the table with the selected mutation data. Users can choose the fields/columns and types of cancer from the menu on the right. A small description of each item is shown next to each name. The table presents direct links with other information sources such as GeneCards, dbSNP, NCBI, Uniprot, and PDB/AlphaFoldDB. The panel on the left side allows users to filter on any of the provided columns using plain text and regular expressions.  Recovered results can be downloaded as CSV or PDF formatted files (all pages or current page only), through specific buttons.</p>
    <p>In the last tab (<strong>Custom plot</strong>), the user can explore the data in customized plots for specific or all types of cancer. The user can determine the plot's structure according to their parameters selection included in the x and y axes and the factor variable.</p>",
    is_id = FALSE,
    #is_id = TRUE,
    position = 'bottom'
  )#$
# step(
#   el = "carindb_summary",
#   title = "Summary",
#   description = "<p>The Summary tab provides a summary description of all attributes (columns) of the dataset.</p>",
#   tab = "Summary",
#   tab_id = "tab_carindb",
#   is_id = TRUE,
#   position = "bottom"
# )$step(
#   el = "carindb_plots",
#   title = "CaRinDB and CaRinDB::AlphaFold Plots",
#   description = "<p>Upon clicking the **Plots** tab, the user can visualize a set of graphs regarding some key database features. They are: the number of mutations versus N DamageCalc (Number of predictors that classified this mutation as damaging), the number of mutations versus Inter_Res_Tot (Total residue-residue interactions), the number of mutations versus Betweenness Weighted (RIN parameter), the number of mutations versus the B-factor (RIN parameter) or pLDDT (AlphaFold parameter) of the present residue, and the number of mutations versus the classes of deleterious mutations.</p>",
#   tab = "Plots",
#   tab_id = "tab_carindb",
#   is_id = TRUE,
#   position = "bottom"
# )


