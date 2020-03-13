library(readr)

library(tidyverse)

library(httr)
library(jsonlite)

corona_haspital <- read_csv(paste0(getwd(),"/docs/flexdashboard_shiny/corona_haspital.csv"),locale=locale('ko',encoding='euc-kr'))

corona_clinic_center <- read_csv(paste0(getwd(),"/docs/flexdashboard_shiny/corona_clinic_center.csv"),locale=locale('ko',encoding='euc-kr'))



# 지리정보 API를 사용한 위경도 추출
Lat_lon_fun <- function(addr) {
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", "2ecacbafd523802f293b245103346b06"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  
  lon_lat_df <- tibble(location = addr, long = data_list$documents$x, lat = data_list$documents$y)
  
  return(lon_lat_df)
}




#######################################################################################################################
# # hospital
#######################################################################################################################

location_list <- corona_haspital$주소

# for(i in 1:length(location_list)) {
#   if(i %in% c(2,30,39,102,168,227,280)) {
#     print(i)
#     # print(location_list[i])
#   } else {
#     print(i)
#     print(location_list[i])
#     Lat_lon_fun(location_list[i])
#   }
# }


Lat_lon_fun(location_list[32])


location_long_lat <- map_dfr(location_list[-c(2,30,39,102,168,227,280)], function(x) {
  Lat_lon_fun(x)
})

location_long_lat %>% tail()



#######################################################################################################################
# # clinic
#######################################################################################################################

location_list <- corona_clinic_center$주소

location_long_lat <- map_dfr(location_list[-c(128,131,158, 321,419,484, 538,540, 612)], function(x) {
  Lat_lon_fun(x)
}) 


location_long_lat



# for(i in 611:length(location_list)) {
#   if(i %in% c(128,131,158, 321,419,484, 538,540, 612)) {
#     print(i)
#     # print(location_list[i])
#   } else {
#     print(i)
#     print(location_list[i])
#     Lat_lon_fun(location_list[i])
#   }
# }


