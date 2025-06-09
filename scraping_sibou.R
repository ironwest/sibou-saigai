
## 方法：2)調査対象となるデータの取得--------------------
library(tidyverse)
library(rvest)

# 職場の安全サイトから対象となるファイルのURLを取得する
d <- rvest::read_html("https://anzeninfo.mhlw.go.jp/anzen_pg/SIB_FND.html")
elema <- d |>  rvest::html_elements(css="a")
dotdot <- "https://anzeninfo.mhlw.go.jp"
dat <- tibble(href = html_attr(elema,name="href")) |> 
  filter(str_detect(href,"\\.\\..+xls(|x)$")) |> 
  mutate(gengou = str_extract(href,"(?<=db_).+?(?=\\d)")) |> 
  mutate(month  = str_extract(href,"\\d+(?=\\.xls(|x))")) |> 
  mutate(href = str_replace(href,"\\.\\.",dotdot))
dir.create("dldata_sibou")

# 職場の安全サイトからファイルをダウンロードする
pwalk(list(dat$href,dat$gengou,dat$month), ~{
  if(str_detect(..1,"xlsx")){ext <- "xlsx"  }else{ext <- "xls"}
  download.file(url = ..1, mode="wb",str_c("dldata_sibou/",..2,"_",..3,".",ext))
})

## 方法：3)列名の処理--------------------
# Excelファイルのパスを取得
fl <- list.files("dldata_sibou", full.names = TRUE)

#すべてのExcelファイルを読み込んで列名の処理を行う
ddraw <- map(fl, ~{
  afl <- .
  d <- readxl::read_excel(afl, col_names = FALSE)
  
  #1行目と2行目を取得しカッコなどの表記ゆれの影響をなくし、
  #1行目‗2行目あるいは2行目がなければ1行目‗1行目という列名を作成
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
  
  #1行目と2行目を破棄して作成した列名でデータを置き換える
  d2 <- d |> slice(-c(1:2)) |> 
    setNames(newnames) |> 
    mutate(afile = afl)
  
  return(d2)
})


# 表記ゆれの年度を確認
walk(ddraw, ~{
  add <- .
  print(add$afile[1])
  print(colnames(add))
})


#列名の表記ゆれの修正する関数
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

ddraw <- ddraw |> 
  replace_colname("ID_ID","ID") |> 
  replace_colname("月_月","月") |> 
  replace_colname("発生時間_発生時間","発生時間") |> 
  replace_colname("発生時間_発生時間","発生時間") |>
  replace_colname("災害状況_災害状況","災害状況") |>
  replace_colname("事業場規模_10～29","事業場規模_分類名") |>
  replace_colname("事業場規模_人数","事業場規模_分類名") |>
  replace_colname("起因物(中分類)_起因物中","起因物(中分類)_分類名")
  
dd <- bind_rows(ddraw)

#この時点でR4までのデータで列名の相違はなし。
#ここからは各列の問題がないかを確認する

#ID,月－特に対応なし:数字に変化可能かだけみておく--------------
#欠損している数の確認
is.na(as.numeric(dd$ID)) |> sum()　#0件

#含まれているデータ
dd |> count(月) #|> clipr::write_clip()

dd <- dd |> 
  mutate(`月` = as.numeric(`月`))

#発生時間----------------------------------------
#欠損数
is.na(dd$発生時間) |> sum()　#0件

before <- dd |> count(`発生時間`)
#微妙に8～7など、他のと違うのがありそう。
dd |> 
  select(発生時間,afile) |> 
  filter(発生時間 == "8～7") |> 
  count(afile)
#平成23年データがおかしい。

h23 <- dd |> filter(afile == "dldata_sibou/h_23.xls")
r04 <- dd |> filter(str_detect(afile,"r_04"))


h23 |> count(`発生時間`) #|> View()　#25種類.他の値から、8~7は7~8の間違い
r04 |> count(`発生時間`)　#12種類

#h23とr04の発生時間列の値を比較してみる。
ch23 <- h23 |> count(`発生時間`) |> rename(h23 = n)
cr04 <- r04 |> count(`発生時間`) |> rename(r04 = n)


full_join(ch23,cr04,by="発生時間") |> 
  arrange(`発生時間`)
#結果としてr04などの他のデータが指定している範囲より
#h23の方がより細かい範囲指定をしている

#この2時間区切り表記と1時間区切りの表記の変化は、
dd |> 
  group_nest(afile) |> 
  mutate(data2 = map_dbl(data, ~nrow(count(.,`発生時間`)))) |> 
  select(!data) #|> View()

#平成28年まで1時間区切り、平成29年以降は2時間区切りと
#なっている。1時間区切りの方がより詳細ではあるが、
#新しく公開されているデータが2時間区切りになっているため、
#1時間毎のデータを2時間毎のデータに吸収させる形で
#置き換えを行う。

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
  "8～7","6～8",
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

#調査報告用の集計
replace_tibble |> 
  left_join(before, by=c("levels"="発生時間")) |> 
  group_nest(labels) |> 
  mutate(str = map_chr(data, ~{
    adata <- .
    
    adata |> 
      mutate(str = str_glue("{levels}({n})")) |> 
      pull(str) |> 
      str_c(collapse=", ")
  })) |> 
  select(!data) #|> clipr::write_clip()


dd <- dd |> 
  mutate(`発生時間` = factor(`発生時間`,levels = hjlevels,labels = hjlabels)) |> 
  mutate(`発生時間` = forcats::fct_na_value_to_level(`発生時間`, "不明"))

dd |> count(`発生時間`) |> clipr::write_clip()
#災害状況----------------------
dd$災害状況

#業種-------------------------------------
#以下の業種のマスターデータを読み込む
#（マスタは、https://anzeninfo.mhlw.go.jp/anzen_pg/rousai_db_siyou1109.doc
#内の表を手作業で加工したもの）

master_gyousyu <- readxl::read_excel("sibou_codes.xlsx", sheet = "業種", col_types = "text")
master_dai <- master_gyousyu |> select(dai_code, dai_name) |> distinct()
master_tyu <- master_gyousyu |> select(tyu_code, tyu_name) |> distinct()
master_syo <- master_gyousyu |> select(syo_code, syo_name) |> distinct()

#業種大分類コード-----------------
#dd |> count(`業種(大分類)_コード`) |> View()
#01と1など表記ゆれがあるので修正する

#マスタと業種大分類コードの不一致件数の集計(表1)
checkdai <- dd |> 
  select(afile, 
         gdaicode = `業種(大分類)_コード`,
         gdaibunrui = `業種(大分類)_分類名`) |>
  left_join(master_dai, by=c("gdaicode"="dai_code"))
  
checkdai2 <- checkdai |>
  count(afile,gdaicode, gdaibunrui, dai_name) 

checkdai2 |> filter(is.na(dai_name))

checkdai2 |> 
  mutate(isna = is.na(dai_name)) |> 
  group_by(isna) |> 
  summarise(n = sum(n))

#業種大分類のコードの修正対応
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
#マスタと業種大分類 分類名の不一致件数の集計(表1)
checkdai <- dd |> 
  select(afile, 
         gdaicode = `業種(大分類)_コード`,
         gdaibunrui = `業種(大分類)_分類名`) |>
  left_join(master_dai, by=c("gdaicode"="dai_code"))

gdaifuitti <- checkdai |>
  mutate(isbunruicorrect = gdaibunrui == dai_name) |> 
  filter(!isbunruicorrect) 

gdaifuitti |> count(isbunruicorrect)
gdaifuitti |>
  count(gdaibunrui, dai_name)
  

#業種大分類分類名にも表記ゆれがあるため、マスタを使ってラベルをつける分類コードを元とした因子型
#の列を作成する。
#dd |> count(`業種(大分類)_分類名`)
dd <- dd |> mutate(`業種(大分類)_分類名` = factor(`業種(大分類)_コード`, master_dai$dai_code, master_dai$dai_name))


#業種中分類コード---------------------

#欠損数
is.na(dd$`業種(中分類)_コード`) |> sum()
#マスタコードとの一致数
vec <- dd$`業種(中分類)_コード`
vec2 <- vec[!is.na(vec)]
sum(!c(vec2  %in% master_tyu$tyu_code))


dd |> filter(`業種(中分類)_コード` %in% c("101","0101")) |> count(`業種(中分類)_分類名`)
#0101と101のように、4桁になっているものは0が余分なので補正。
dd <- dd |>
  mutate(`業種(中分類)_コード` = str_remove(`業種(中分類)_コード`, "^0")) 

#業種中分類分類名---------------------
checktyu <- dd |> 
  select(afile, 
         gtyucode = `業種(中分類)_コード`,
         gtyubunrui = `業種(中分類)_分類名`) |>
  left_join(master_tyu, by=c("gtyucode"="tyu_code")) |> 
  mutate(itti = gtyubunrui == tyu_name)

checktyu |> 
  count(itti)

#表記ゆれを補正する
dd <- dd |> mutate(`業種(中分類)_分類名` = factor(`業種(中分類)_コード`, master_tyu$tyu_code, master_tyu$tyu_name))


#業種小分類コード-----------------------
#欠損数の確認
sum(is.na(dd$`業種(小分類)_コード`)) #18090
#マスタにないコード
vec <- dd$`業種(小分類)_コード`
vec2 <- vec[!is.na(vec)]
huitti <- !c(vec2 %in% master_syo$syo_code)
#誤記数
sum(huitti) #675
vec2[huitti]

master_syo$syo_code
#コードで0はじまりのものはマスタに存在しない。
sum(str_detect(vec2[huitti], "^0"))
#662件が0はじまり。これを修正してみる。また1件だけ/nの改行
#コードが含まれるレコードがあるのでこちらの改行コードも
#除去しておく

dd <- dd |> 
  mutate(`業種(小分類)_コード` = str_remove(`業種(小分類)_コード`,"^0")) |> 
  mutate(`業種(小分類)_コード` = str_remove(`業種(小分類)_コード`,"\n"))
#マスタにないコード
vec <- dd$`業種(小分類)_コード`
vec2 <- vec[!is.na(vec)]
huitti <- !c(vec2 %in% master_syo$syo_code)
#誤記数
sum(huitti) #13
vec2[huitti]

dd |> filter(`業種(小分類)_コード` == "11609") #|> View()

#業種の小分類コードの誤りと考えられるものは次のように修正した
#11609 ->   そのまま。マスタにその他分類を追加した(6件)
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
    `業種(小分類)_コード` == "30303"  ~ "30302",
    `業種(小分類)_コード` == "80108"  ~ "80109",
    `業種(小分類)_コード` == "8041"  ~ "80401",
    TRUE ~ `業種(小分類)_コード`
  ))


#業種小分類分類名---------------------
#11609というコードがデータにはあるが、マスタにはない。そのため、
#新たにその他の電気・ガス・水道業というマスタを追加した。
checksyo <- dd |> 
  select(afile, 
         gsyocode = `業種(小分類)_コード`,
         gsyobunrui = `業種(小分類)_分類名`) |>
  left_join(master_syo, by=c("gsyocode"="syo_code")) |> 
  mutate(itti = gsyobunrui == syo_name)

checksyo |> 
  count(itti)

dd <- dd |> 
  mutate(`業種(小分類)_分類名` = factor(`業種(小分類)_コード`, master_syo$syo_code, master_syo$syo_name))


#dd |> count(`起因物(小分類)_分類名`) #|> View()

#View(dd)

#事業場規模---------------------

#欠損の数
sum(is.na(dd$事業場規模_分類名)) #24

#カテゴリー
catskibo <- dd |> count(`事業場規模_分類名`)

#一部の年度で0-9、300-というカテゴリーがあり、他の年度との
#統合ができない.0人のデータが72件だったので、0-9というカテゴリーに統合しておく。
#最近のものでは0と1-9と分けられているが、0-9というカテゴリーとしておく
#300~といカテゴリーは、H23とH24の2年度のみ
o300cat <-dd |> filter(`事業場規模_分類名` == "300～") |> pull(afile) |> unique()

#この二年度の事業場規模分類名は次で、300以上でひとくくりにされる
#これを他の年度と齟齬なく結合することは難しいので、定義エラー300以上という
#カテゴリーに入れる処理を行う

dd |> 
  filter(afile %in% o300cat) |> 
  count(`事業場規模_分類名`)


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
  "1,000～9,999","1000-",
  "1000～9999"  ,"1000-",
  "10,000～","1000-",
  "10000～","1000-",
  "300～"   ,"定義エラー300以上",
  "不明","不明",
  NA_character_,"不明"
)

left_join(
  kibomaster,
  catskibo,
  by = c("lev"="事業場規模_分類名")
) |> 
  group_nest(lab) |>
  mutate(n = map_dbl(data, ~{sum(.$n)})) |> 
  mutate(str = map_chr(data, ~{
    adata <- .
    adata |> 
      mutate(res = str_glue("{lev}({n})")) |> 
      pull(res) |> 
      str_c(collapse=", ")
  })) |> 
  select(lab,n, str) #|> clipr::write_clip()

kibomaster <- kibomaster |> filter(!is.na(lev))

dd <-dd |> 
  mutate(`事業場規模_分類名` = factor(`事業場規模_分類名`,kibomaster$lev, kibomaster$lab)) |> 
  mutate(`事業場規模_分類名` = forcats::fct_na_value_to_level(`事業場規模_分類名`,"不明"))

dd |> count(事業場規模_分類名)


#起因物　マスター-----------------------------
kiinmaster <- readxl::read_excel("sibou_codes.xlsx", sheet = "起因物", col_types = "text")
kiindai <- kiinmaster |> select(dai_code, dai_name) |> distinct()
kiintyu <- kiinmaster |> select(tyu_code, tyu_name) |> distinct()
kiinsyo <- kiinmaster |> select(syo_code, syo_name) |> distinct()

#起因物大分類のコードの確認
#欠損
is.na(dd$`起因物(大分類)_コード`) |> sum() #0
#マスタとの不一致
sum(!c(dd$`起因物(大分類)_コード` %in% kiindai$dai_code))



#起因物大分類の分類名の確認
#欠損
is.na(dd$`起因物(大分類)_分類名`) |> sum() #0
#マスタとの不一致
sum(!c(dd$`起因物(大分類)_分類名` %in% kiindai$dai_name))
kdaidat <- dd |> 
  select(
    afile,
    kiindaicode = `起因物(大分類)_コード`,
    kiindainame = `起因物(大分類)_分類名`
  )

kdaidat <- kdaidat |> 
  left_join(kiindai, by=c("kiindaicode"="dai_code")) |> 
  mutate(itti = kiindainame==dai_name) 

kdaidat|> 
  count(itti)

kdaidat |> 
  filter(!itti) |> 
  distinct()

#起因物中分類のコードの確認
#欠損
is.na(dd$`起因物(中分類)_コード`) |> sum() #0
#マスタとの不一致
sum(!c(dd$`起因物(中分類)_コード` %in% kiintyu$tyu_code))

v <- dd$`起因物(中分類)_コード`
v[!c(v %in% kiintyu$tyu_code)] #17というコードがマスタにはないのに存在

# 17xという中分類、小分類は42レコード存在するものの、
# マスタには存在しない。マスタに次の内容を追加する
dd |> 
  filter(`起因物(中分類)_コード` == "17") |> 
  select(`起因物(中分類)_コード`,`起因物(中分類)_分類名`,`起因物(小分類)_コード`,`起因物(小分類)_分類名`) |> 
  distinct()  

#起因物中分類の分類名の確認
#欠損
is.na(dd$`起因物(中分類)_分類名`) |> sum() #0
ktyudat <- dd |> 
  select(
    afile,
    kiintyucode = `起因物(中分類)_コード`,
    kiintyuname = `起因物(中分類)_分類名`
  )

ktyudat <- ktyudat |> 
  left_join(kiintyu, by=c("kiintyucode"="tyu_code")) |> 
  mutate(itti = kiintyuname==tyu_name) 

ktyudat|> 
  count(itti)

ktyudat |> 
  filter(!itti | is.na(itti)) |> 
  count(kiintyucode, kiintyuname, tyu_name) #|> View()

#起因物小分類のコードの確認
#欠損
is.na(dd$`起因物(小分類)_コード`) |> sum() #0
#マスタとの不一致
sum(!c(dd$`起因物(小分類)_コード` %in% kiinsyo$syo_code))

ksyodat <- dd |> 
  select(
    afile,
    kiinsyocode = `起因物(小分類)_コード`,
    kiinsyoname = `起因物(小分類)_分類名`
  )

ksyoexist <- ksyodat |> filter(!is.na(kiinsyocode)) |> 
  mutate(iscodeexist = kiinsyocode %in% kiinsyo$syo_code)
  
ksyoexist |> count(iscodeexist)
ksyoexist |> filter(!iscodeexist) |> 
  count(kiinsyocode, kiinsyoname)
 #171,172,173は中分類17を追加したときに追加した。

dd |> 
  filter(`起因物(小分類)_コード`　 == "392") #|> View()
#392の分類名は、その他の装置、設備に2件ともなっており、
#391がその他の装置、設備になっているため、391の誤入力
#であると判断

dd <- dd |> 
  mutate(`起因物(小分類)_コード` = case_when(
    `起因物(小分類)_コード`=="392" ~ "391",
    TRUE ~ `起因物(小分類)_コード`
  ))

#起因物中分類の分類名の確認
#欠損
is.na(dd$`起因物(小分類)_分類名`) |> sum() #0
ksyodat <- dd |> 
  select(
    afile,
    kiinsyocode = `起因物(小分類)_コード`,
    kiinsyoname = `起因物(小分類)_分類名`
  )

ksyodat |> filter(is.na(kiinsyoname))

ksyodat <- ksyodat |> 
  left_join(kiinsyo, by=c("kiinsyocode"="syo_code")) |> 
  mutate(itti = kiinsyoname==syo_name) 

ksyodat |> filter(is.na(kiinsyoname))

ksyodat|> 
  filter(!is.na(kiinsyoname)) |> 
  count(itti)

ksyodat |> 
  filter(!itti | is.na(itti)) |> 
  count(kiinsyocode, kiinsyoname, syo_name) #|> View()

#コードに従ってすべのレコードの起因物X分類の分類名を置き換える
dd <- dd |> 
  mutate(`起因物(大分類)_分類名` = factor(`起因物(大分類)_コード`, kiindai$dai_code, kiindai$dai_name)) |> 
  mutate(`起因物(中分類)_分類名` = factor(`起因物(中分類)_コード`, kiintyu$tyu_code, kiintyu$tyu_name)) |> 
  mutate(`起因物(小分類)_分類名` = factor(`起因物(小分類)_コード`, kiinsyo$syo_code, kiinsyo$syo_name))

#事故の型----------------------------
jikomaster <- readxl::read_excel("sibou_codes.xlsx", sheet="事故の型", col_types = "text")

#事故の型コードの欠損値の数
sum(is.na(dd$`事故の型_コード`))

jikodat <- dd |> 
  select(afile, jiko_code = 事故の型_コード,
         jiko_name = 事故の型_分類名) |> 
  mutate(exist_in_master = jiko_code %in% jikomaster$jiko_code)

jikodat |> 
  count(exist_in_master) #0件の誤分類

#事故の型の分類の欠損値の数
sum(is.na(dd$事故の型_分類名))#0

#事故の型の分類の誤入力と思われる件数
jikodat |> 
  left_join(jikomaster, by=c("jiko_code" = "jiko_code")) |> 
  mutate(itti = jiko_name == jiko_bunrui) |> 
  filter(!itti) |> 
  count(jiko_code, jiko_name, jiko_bunrui) |> 
  pull(n) |> sum()

#事故の型の分類のコードと分類名を対応させる。
dd <-dd |> 
  mutate(`事故の型_分類名` = factor(`事故の型_コード`, jikomaster$jiko_code, jikomaster$jiko_bunrui))

#ファイル名から年度列を作成する-------------------------
#各年度のデータ読み込み時に、ファイル名をafile列に保存している
#ファイル名から、和暦を西暦で年度に変更して、fyという列
#に収納した。
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


# #スクリプトの保存先：https://github.com/ironwest/sibou-saigai/tree/main
# #qrcodeの作成
# code <- qrcode::qr_code("https://github.com/ironwest/sibou-saigai/tree/main")
# plot(code)
# 
# # app https://factory-health.shinyapps.io/sibou-saigai/
# code <- qrcode::qr_code("https://factory-health.shinyapps.io/sibou-saigai/")
# plot(code)


