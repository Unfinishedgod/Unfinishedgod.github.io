Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

library(rmarkdown)

html_link <- "porto_safedriver_pred.html"

render("/home/owen/Unfinishedgod.github.io/docs/kaggle/Porto_Seguro_Safe_Driver_Prediction/porto_safedriver_pred.Rmd",
       output_file = html_link,
       output_dir = "/home/owen/Unfinishedgod.github.io/docs/kaggle/Porto_Seguro_Safe_Driver_Prediction")


