library(flexdashboard)
library(readr)
library(tidyverse)
library(leaflet)
library(DT)
library(httr)
library(jsonlite)
library(glue)

# 데이터 추출 ------------------------

corona_haspital <- read_csv(paste0(getwd(),"/docs/flexdashboard_shiny/corona_haspital.csv"),locale=locale('ko',encoding='euc-kr'))

corona_clinic <- read_csv(paste0(getwd(),"/docs/flexdashboard_shiny/corona_clinic.csv"),locale=locale('ko',encoding='euc-kr'))


# 지리정보 API를 사용한 위경도 추출
Lat_lon_fun <- function(addr) {
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", "2ecacbafd523802f293b245103346b06"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  
  lon_lat_df <- tibble(주소 = addr, 
                       long = as.numeric(data_list$documents$x), 
                       lat = as.numeric(data_list$documents$y))
  
  return(lon_lat_df)
}




# 병원 ------------------------

hos_location_list <- corona_haspital$주소


not_hos_list <- c(2,30,39,102,168,227,280)

hos_location_long_lat <- map_dfr(hos_location_list[-not_hos_list], function(x) {
  Lat_lon_fun(x)
})

hos_info <- corona_haspital %>% 
  left_join(hos_location_long_lat)  %>% 
  # select(시도, 시군구, 기관명, 주소, 전화번호, long, lat) %>% 
  rename(유형 = 신청유형)

# 진료소 ------------------------
cli_location_list <- corona_clinic$주소

not_cli_list <- c(128,131,158, 321,419,484, 538,540, 612)

cli_location_long_lat <- map_dfr(cli_location_list[-not_cli_list], function(x) {
  Lat_lon_fun(x)
}) 


cli_info <- corona_clinic %>% 
  left_join(cli_location_long_lat) %>% 
  # select(시도, 시군구, 의료기관명, 주소, `대표 전화번호`, long, lat) %>% 
  rename(전화번호 = `대표 전화번호`, 기관명 = 의료기관명, 유형 = `검체채취 가능여부`)

cli_info$유형 <- case_when(
  cli_info$유형 == "Y" ~ "검체체취 가능",
  TRUE ~ "검체체취 불가"
)


hos_cli_info <- hos_info %>% 
  mutate(구분 = "국민안심병원") %>% 
  bind_rows((cli_info %>% 
               mutate(구분 = "선별진료소"))) %>% 
  drop_na(long) %>% 
  replace_na(list(운영가능일자 = "운영가능")) %>% 
  select(구분, 시도, 시군구, 기관명, 주소, 유형, 전화번호, 운영가능일자, long, lat)





theme_set(theme_bw())


# 병원,진료소 leaflet ------------------------

hos_cli_leaflet <- hos_cli_info %>% 
  mutate(ratingcol = case_when(
    구분 == "국민안심병원" ~ "green",
    TRUE ~ "blue"
  ))

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(hos_cli_leaflet$long, 
                   hos_cli_leaflet$lat, 
                   color = hos_cli_leaflet$ratingcol, 
                   radius = 8, 
                   fill = T,
                   fillOpacity = 0.6,
                   opacity = 0.1,
                   popup = paste0(hos_cli_leaflet$기관명, " \n(", hos_cli_leaflet$전화번호,")")) %>%
  addLegend("topright", 
            colors = c("green","blue"),
            labels = c("보건복지부 국민안심병원",
                       "보건복지부 선별진료소"), 
            opacity = 1)




# 병원,진료소 leaflet 숫자 표시 ------------------------

leaflet(total_location_long_lat) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())


# 병원,진료소 ggplot ------------------------

hos_cli_info$구분 <- as.factor(hos_cli_info$구분)

p <- hos_cli_info %>% 
  group_by(구분,시도) %>% 
  summarise(개수 = n()) %>% 
  arrange(구분, 시도) %>% 
  ggplot(aes(x = 시도, y = 개수, fill = 구분)) +
  geom_col(position="dodge") +
  scale_fill_manual(values = c("green3", "blue3")) +
  labs(title = "지역별 병원, 진료소 파악", x = "", y = "") +
  theme(legend.position = "bottom")


ggplotly(p) %>% 
  layout(legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5))   

# 병원,진료소 plotly -----

df_plotly <- hos_cli_info %>% 
  group_by(구분,시도) %>% 
  summarise(개수 = n()) %>% 
  arrange(구분, 시도) %>% 
  spread(key = "구분", value = "개수") %>% 
  replace_na(list(국민안심병원 = 0)) 



df_plotly %>% 
  plot_ly(
    x = ~시도,
    y = ~국민안심병원,
    type = "bar",
    name = "국민안심병원",
    textposition = 'auto',
    marker = list(color = 'green')
  ) %>% 
  add_trace(y = ~선별진료소,
            name = "선별진료소",
            textposition = 'auto',
            marker = list(color = 'blue')) %>% 
  layout(legend = list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5,
                       y = 0.9)) 


# 마스크 정보 API ------------------------

# 1. 주소/좌표 기준 판매처별 공적 마스크 판매정보 제공 서비스
url_1 <- "https://8oi9s0nnth.apigw.ntruss.com/corona19-masks/v1/stores/json?page=1&perPage=5000"
# 2. 판매처별 공적 마스크 판매처 정보 제공 서비스
url_2 <- "https://8oi9s0nnth.apigw.ntruss.com/corona19-masks/v1/sales/json?page=1&perPage=5000"


file_1 <- fromJSON(url_1)
file_2 <- fromJSON(url_2)

# 1번
df_1 <- file_1$storeInfos %>% 
  as_tibble()

# 2번
df_2 <- file_2$sales %>% 
  as_tibble()


asdf <- df_1 %>% 
  left_join(df_2) %>% 
  # replace_na(list(created_at = "정보 없음", remain_stat = "정보없음", stock_at = "정보 없음")) %>%
  drop_na() %>%
  mutate(remain_stat_kr = case_when(
    remain_stat == "plenty" ~ "100개 이상",
    remain_stat == "some" ~ "30개 이상 100개 미만",
    remain_stat == "few" ~ "2개 이상 30개 미만",
    remain_stat == "empty" ~ "1개 이하",
    remain_stat == "break" ~ "2개 이상 30개 미만",
    TRUE ~ "정보 없음"
  ), 구분 = case_when(
    type == "01" ~ "약국",
    type == "02" ~ "우체국",
    TRUE ~ "농협"
  ))



lng_lat_list <- map_chr(1:nrow(asdf), function(x) {
  glue("x={asdf[x,4]}&y={asdf[x,3]}")
})


# 위경도를 사용한 지리정보 추출
addr_fun <- function(lng_lat) {
  data_list <- GET(url = glue("https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?{lng_lat}"),
                   # query = list(query = addr),
                   add_headers(Authorization = paste0("KakaoAK ", "2ecacbafd523802f293b245103346b06"))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  lng_processing <- lng_lat %>% 
    str_split("&") 
  
  raw_lng <- lng_processing[[1]][1] %>% 
    str_sub(3,max(nchar(aaaa[[1]][1]))) %>% 
    as.numeric()
  
  addr_df <- tibble(
    # lng = data_list$documents$x[1],
    lng = raw_lng,
    시도 = data_list$documents$region_1depth_name[1],
    시군구 = data_list$documents$region_2depth_name[1],
    동 = data_list$documents$region_3depth_name[1])
  
  return(addr_df)
}


addr_info <- map_dfr(lng_lat_list , function(x) {
  addr_fun(x)
})

asdf_2 <- asdf %>% 
  left_join(addr_info)


asdf_seoul <- asdf_2 %>% 
  filter(시도 == "서울특별시")

asdf_daegu <- asdf_2 %>% 
  filter(시도 == "대구광역시")

# 마스크 판매처 leaflet ------------------------

asdf_leaflet <- asdf_2 %>% 
  mutate(ratingcol = case_when(
    type == "01" & remain_stat != "정보없음" ~ "green",
    type == "02" & remain_stat != "정보없음"~ "orange",
    type == "03" & remain_stat != "정보없음"~ "blue",
    TRUE ~ "black"
  ))


leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(asdf_leaflet$lng, 
                   asdf_leaflet$lat, 
                   color = asdf_leaflet$ratingcol, 
                   radius = 8, 
                   fill = T,
                   fillOpacity = 0.6,
                   opacity = 0.1,
                   popup = paste0(asdf_leaflet$name," (",asdf_leaflet$addr, "(",asdf_leaflet$remain_stat_kr,"))")) %>%
  addLegend("topright", 
            colors = c("green","orange","blue"),
            labels = c("약국",
                       "우체국",
                       "농협"), 
            opacity = 1)



# 마스크 판매처 ggplot ------------------------

# 마스크 판매처 plotly ------------------------
df_plotly <- asdf_2 %>% 
  group_by(구분,시도) %>% 
  summarise(개수 = n()) %>% 
  arrange(구분, 시도) %>% 
  spread(key = "구분", value = "개수") %>% 
  replace_na(list(농협 = 0, 약국 = 0, 우체국=0)) 



df_plotly %>% 
  plot_ly(
    x = ~시도,
    y = ~농협,
    type = "bar",
    name = "농협",
    textposition = 'auto',
    marker = list(color = 'green')
  ) %>% 
  add_trace(y = ~약국,
            name = "우체국",
            textposition = 'auto',
            marker = list(color = 'orange')) %>% 
  add_trace(y = ~농협,
            name = "농협",
            textposition = 'auto',
            marker = list(color = 'blue')) %>% 
  layout(legend = list(orientation = "v",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.9,
                       y = 1.09)) 









df_plotly <- asdf_daegu %>% 
  group_by(구분,시군구) %>% 
  summarise(개수 = n()) %>% 
  arrange(구분, 시군구) %>% 
  spread(key = "구분", value = "개수") %>% 
  replace_na(list(농협 = 0, 약국 = 0, 우체국=0))

df_plotly <- df_plotly %>% 
  mutate("농협" = 0) 

df_plotly %>% 
  plot_ly(
    x = ~시군구,
    y = ~농협,
    type = "bar",
    name = "농협",
    textposition = 'auto',
    marker = list(color = 'green')
  ) %>% 
  add_trace(y = ~약국,
            name = "우체국",
            textposition = 'auto',
            marker = list(color = 'orange')) %>% 
  add_trace(y = ~농협,
            name = "농협",
            textposition = 'auto',
            marker = list(color = 'blue')) %>% 
  layout(legend = list(orientation = "v",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.9,
                       y = 1.09)) 
