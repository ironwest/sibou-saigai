library(tidyverse)
library(rvest)

#html-----------------------------------------------------
#職場の安全サイトから対象となるファイルのURLを取得する
html <- "https://anzeninfo.mhlw.go.jp/anzen_pg/SIB_FND.html"

d <- rvest::read_html(html)

elema <- d |>  rvest::html_elements(css="a")


dotdot <- "https://anzeninfo.mhlw.go.jp"

dat <- tibble(href = html_attr(elema,name="href")) |> 
  filter(str_detect(href,"\\.\\..+xls(|x)$")) |> 
  mutate(gengou = str_extract(href,"(?<=db_).+?(?=\\d)")) |> 
  mutate(month  = str_extract(href,"\\d+(?=\\.xls(|x))")) |> 
  mutate(href = str_replace(href,"\\.\\.",dotdot))
  
dir.create("dldata_sibou")

#dldata-------------------------------
#職場の安全サイトからファイルをダウンロードする
pwalk(list(dat$href,dat$gengou,dat$month), ~{
  if(str_detect(..1,"xlsx")){
    ext <- "xlsx"  
  }else{
    ext <- "xls"
  }
  
  download.file(url = ..1, mode="wb",str_c("dldata_sibou/",..2,"_",..3,".",ext))
})

#cleaning--------------------------------
#ダウンロードしたファイルを結合して表記ゆれなどをクリーニングする
fl <- list.files("dldata_sibou", full.names = TRUE)

dd <- map(fl, ~{
  print(.)
  afl <- .
  d <- readxl::read_excel(afl, col_names = FALSE)
  
  newnames <- tibble(l1 = unlist(d[1,]),l2 = unlist(d[2,])) |> 
    mutate(across(c(l1,l2), ~str_replace(.,"\r\n",""))) |> 
    mutate(across(c(l1,l2), ~str_replace(.,"\n",""))) |> 
    mutate(across(c(l1,l2), ~str_replace(.,"（","("))) |> 
    mutate(across(c(l1,l2), ~str_replace(.,"）",")"))) |> 
    fill(l1,l2) |> 
    mutate(name = case_when(
      is.na(l2) ~ l1,
      TRUE ~ str_c(l1,"_",l2)
    )) |> 
    pull(name)
  
  d2 <- d |> 
    slice(-c(1:2)) |> 
    setNames(newnames) |> 
    mutate(afile = afl)
  
  return(d2)
})


#列名の表記ゆれの確認
# cns <- map_dfr(dd2,~{
#   acn <- colnames(.)
#   afl <- .$afile[1]
#   tibble(id = 1:length(acn),
#          cn = acn,
#          afl = afl
#          )
# })
# 
# cns |> 
#   pivot_wider(id_cols = afl, names_from = id, values_from = cn) |> View()

#列名の表記ゆれの修正
replace_colname <- function(add,from,to){
 add <- map(add, ~{
    adat <- .
    if(from %in% colnames(adat)){
      adat <- adat |> 
        rename(!!rlang::sym(to) := !!rlang::sym(from))
    }else{
      #do nothing  
    }
    return(adat)
  })
  
  return(add)
}

dd <- dd |> 
  replace_colname("ID_ID","ID") |> 
  replace_colname("月_月","月") |> 
  replace_colname("発生時間_発生時間","発生時間") |> 
  replace_colname("発生時間_発生時間","発生時間") |>
  replace_colname("災害状況_災害状況","災害状況") |>
  replace_colname("事業場規模_10～29","事業場規模_分類名") |>
  replace_colname("事業場規模_人数","事業場規模_分類名") |>
  replace_colname("起因物(中分類)_起因物中","起因物(中分類)_分類名")
  
ddraw <- bind_rows(dd)

dd <- ddraw
#この時点でR4までのデータで列名の相違はなし。
#ここからは各列の問題がないかを確認する

#ID,月－特に対応なし:数字に変化可能かだけみておく--------------
is.na(as.numeric(dd$ID)) |> sum()
as.numeric(dd$月) |> unique()

dd <- dd |> 
  mutate(`月` = as.numeric(`月`))

#発生時間----------------------------------------
dd |> count(`発生時間`) #|> View()
#微妙に8～7など、他のと違うのがありそう。
dd |> 
  select(発生時間,afile) |> 
  filter(発生時間 == "8～7") |> 
  count(afile)
#平成23年データがおかしい。

h23 <- dd |> 
  filter(afile == "dldata_sibou/h_23.xls")
r04 <- dd |> 
  filter(str_detect(afile,"r_04"))

h23 |> count(`発生時間`)　#25種類
r04 |> count(`発生時間`)　#12種類

#h23とr04の発生時間列の値を比較してみる。
ch23 <- h23 |> count(`発生時間`) |> rename(h23 = n)
cr04 <- r04 |> count(`発生時間`) |> rename(r04 = n)


full_join(ch23,cr04,by="発生時間") |> 
  arrange(`発生時間`)
#結果としてr04などの他のデータが指定している範囲より
#h23の方がより細かい範囲指定をしている
#処理に差支えが出るので、h23を吸収する形でr04他にデータを
#融合（h23の情報量は若干へるが他とマージできない方が問題）

#因子型にして吸収させる
replace_tibble <- {tribble(
  ~levels, ~labels,
  "0～1","0～2",
  "1～2","0～2",
  "0～2","0～2",
  
  "2～3","2～4",
  "3～4","2～4",
  "2～4","2～4",
  
  "4～5","4～6",
  "5～6","4～6",
  "4～6","4～6",
  
  "6～7","6～8",
  "7～8","6～8",
  "6～8","6～8",
  
  "8～9","8～10",
  "9～10","8～10",
  "8～10","8～10",
  
  "10～11","10～12",
  "11～12","10～12",
  "10～12","10～12",
  
  "12～13","12～14",
  "13～14","12～14",
  "12～14","12～14",
  
  "14～15","14～16",
  "15～16","14～16",
  "14～16","14～16",
  
  "16～17","16～18",
  "17～18","16～18",
  "16～18","16～18",
  
  "18～19","18～20",
  "19～20","18～20",
  "18～20","18～20",
  
  "20～21","20～22",
  "21～22","20～22",
  "20～22","20～22",
  
  "22～23","22～24",
  "23～0","22～24",
  "23～24","22～24",
  "22～24","22～24",
  "不明","不明"
)}
hjlevels <- replace_tibble$levels
hjlabels <- replace_tibble$labels

dd <- dd |> 
  mutate(`発生時間` = factor(`発生時間`,levels = hjlevels,labels = hjlabels))

#災害状況----------------------
dd$災害状況

#以下の業種のマスターデータを読み込む
#（マスタは、https://anzeninfo.mhlw.go.jp/anzen_pg/rousai_db_siyou1109.doc
#内の表を手作業で加工したもの）

master_gyousyu <- readxl::read_excel("sibou_codes.xlsx", sheet = "業種")
master_dai <- master_gyousyu |> select(dai_code, dai_name) |> distinct()
master_tyu <- master_gyousyu |> select(tyu_code, tyu_name) |> distinct()
master_syo <- master_gyousyu |> select(syo_code, syo_name) |> distinct()

#業種大分類コード-----------------
#dd |> count(`業種(大分類)_コード`) |> View()
#01と1など表記ゆれがあるので修正する

dd <- dd |> 
  mutate(`業種(大分類)_コード` = case_when(
    `業種(大分類)_コード` == "01" ~ "1",
    `業種(大分類)_コード` == "02" ~ "2",
    `業種(大分類)_コード` == "03" ~ "3",
    `業種(大分類)_コード` == "04" ~ "4",
    `業種(大分類)_コード` == "05" ~ "5",
    `業種(大分類)_コード` == "06" ~ "6",
    `業種(大分類)_コード` == "07" ~ "7",
    `業種(大分類)_コード` == "08" ~ "8",
    `業種(大分類)_コード` == "09" ~ "9",
    TRUE ~ `業種(大分類)_コード`
  ))

dd <-dd |> mutate(`業種(大分類)_コード` = as.factor(`業種(大分類)_コード`))

#業種大分類分類名---------------------------------
#業種大分類分類名にも表記ゆれがあるため、マスタを使ってラベルをつける分類コードを元とした因子型
#の列を作成する。
#dd |> count(`業種(大分類)_分類名`)
dd <- dd |> mutate(`業種(大分類)_分類名` = factor(`業種(大分類)_コード`, master_dai$dai_code, master_dai$dai_name))


#業種中分類コード---------------------
dd |> count(`業種(中分類)_コード`) #|> View()
dd |> filter(`業種(中分類)_コード` %in% c("101","0101")) |> count(`業種(中分類)_分類名`)
#0101と101のように、4桁になっているものは0が余分なので補正。
dd <- dd |>
  mutate(`業種(中分類)_コード` = str_remove(`業種(中分類)_コード`, "^0")) 

#業種中分類分類名---------------------
#表記ゆれを補正する
dd <- dd |> mutate(`業種(中分類)_分類名` = factor(`業種(中分類)_コード`, master_tyu$tyu_code, master_tyu$tyu_name))


#業種小分類コード-----------------------
#業種の小分類コードの誤りと考えられるものは次のように修正した
#1081 -> 10801
#140202 -> 140201 #内容からは調理作業があるので01で妥当？
#14039 -> 140309
#17029 -> 170209
#30303 -> 30309 #内容をよんでも01や02のあるコードに当てはめられない
#80108 -> 80109 #
#8041 -> 80401

dd <- dd |> 
  mutate(`業種(小分類)_コード` = case_when(
    `業種(小分類)_コード` == "1081"   ~ "10801",
    `業種(小分類)_コード` == "140202" ~ "140201",
    `業種(小分類)_コード` == "14039"  ~ "140309",
    `業種(小分類)_コード` == "17029"  ~ "170209",
    `業種(小分類)_コード` == "30303"  ~ "30309",
    `業種(小分類)_コード` == "80108"  ~ "80109",
    `業種(小分類)_コード` == "8041"  ~ "80401",
    TRUE ~ `業種(小分類)_コード`
  ))


#業種小分類分類名---------------------
#11609というコードがデータにはあるが、マスタにはない。そのため、
#新たにその他の電気・ガス・水道業というマスタを追加した。
dd <- dd |> 
  mutate(`業種(小分類)_分類名` = factor(`業種(小分類)_コード`, master_syo$syo_code, master_syo$syo_name))


#dd |> count(`起因物(小分類)_分類名`) |> View()

#View(dd)

#事業場規模---------------------
#一部の年度で0-9、300-というカテゴリーがあり、他の年度との
#統合ができない.0人のデータが72件だったので、0-9というカテゴリー、
#最近のものでは0と1-9と分けられているが、0-9というカテゴリーとしておく
kibomaster <- tribble(
  ~lev, ~lab,
  "0"       ,"0-9",
  "0～9"    ,"0-9",
  "1～9"    ,"0-9",
  "10～19"  ,"10-29",
  "10～29"  ,"10-29",
  "20～29"  ,"10-29",
  "30～39"  ,"30-49",
  "30～49"  ,"30-49",
  "40～49"  ,"50-99",
  "50～99"  ,"50-99",
  "100～299","100-299",
  "300～499","300-499",
  "500～999","500-999",
  "1,000～9,999","1000-9999",
  "1000～9999"  ,"1000-9999",
  "10,000～","10000-",
  "10000～","10000-",
  "300～"   ,"定義エラー",
  "不明","不明"
)

dd <-dd |> 
  mutate(`事業場規模_分類名` = factor(`事業場規模_分類名`,kibomaster$lev, kibomaster$lab))

dd |> count(事業場規模_分類名)


#起因物　マスター-----------------------------
kiinmaster <- readxl::read_excel("sibou_codes.xlsx", sheet = "起因物")
kiindai <- kiinmaster |> select(dai_code, dai_name) |> distinct()
kiintyu <- kiinmaster |> select(tyu_code, tyu_name) |> distinct()
kiinsyo <- kiinmaster |> select(syo_code, syo_name) |> distinct()

#起因物のコードと分類名の統合
dd |> count(`起因物(大分類)_コード`)
dd |> count(`起因物(中分類)_コード`)
dd |> count(`起因物(小分類)_コード`)

dd <- dd |> 
  mutate(`起因物(大分類)_分類名` = factor(`起因物(大分類)_コード`, kiindai$dai_code, kiindai$dai_name)) |> 
  mutate(`起因物(中分類)_分類名` = factor(`起因物(中分類)_コード`, kiintyu$tyu_code, kiintyu$tyu_name)) |> 
  mutate(`起因物(小分類)_分類名` = factor(`起因物(小分類)_コード`, kiinsyo$syo_code, kiinsyo$syo_name))

#事故の型----------------------------
jikomaster <- readxl::read_excel("sibou_codes.xlsx", sheet="事故の型")

dd <-dd |> 
  mutate(`事故の型_分類名` = factor(`事故の型_コード`, jikomaster$jiko_code, jikomaster$jiko_bunrui))

#ファイル名から年度列を作成する
dd <- dd |> 
  mutate(gengou = str_extract(afile,"(?<=dldata_sibou/).+(?=_)")) |> 
  mutate(nendo  = str_extract(afile,"\\d+") |> as.numeric()) |> 
  mutate(fy = case_when(
    gengou == "h" ~ 1988 + nendo,
    gengou == "r" ~ 2018 + nendo
  )) |> 
  select(!c(gengou, nendo, afile)) |> 
  relocate(fy)



#完成：以上で列は可能な範囲でクレンジングを実施完了。
#CSVファイルとして保存
write_excel_csv(dd, "死亡データ_統合.csv")
#RDSデータとして保存

dd <- dd |> 
  rename(
    month = "月",
    time = "発生時間",
    text = "災害状況",
    gyou_dai_code = "業種(大分類)_コード",
    gyou_dai_name = "業種(大分類)_分類名",
    gyou_tyu_code = "業種(中分類)_コード",
    gyou_tyu_name = "業種(中分類)_分類名",
    gyou_syo_code = "業種(小分類)_コード",
    gyou_syo_name = "業種(小分類)_分類名",
    kibo = "事業場規模_分類名",
    kiin_dai_code = "起因物(大分類)_コード",
    kiin_dai_name = "起因物(大分類)_分類名",
    kiin_tyu_code = "起因物(中分類)_コード",
    kiin_tyu_name = "起因物(中分類)_分類名",
    kiin_syo_code = "起因物(小分類)_コード",
    kiin_syo_name = "起因物(小分類)_分類名",
    jiko_code = "事故の型_コード",
    jiko_name = "事故の型_分類名"
  )

 dd <- dd |> 
   mutate(month = factor(month, levels = c(4:12,1:3)))
 
 dd <- dd |> 
   mutate(across(where(is.factor), ~{
     forcats::fct_na_value_to_level(., "欠損")
   })) |> 
   mutate(across(where(is.character), ~{
     replace_na(., "欠損")
   }))
 
 
 

write_rds(dd,"death_by_accident_data.rds", compress = "gz")
