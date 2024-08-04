# loading libraries 
library(text2map)
library(text2vec)
library(gutenbergr)
library(tidyverse)
library(textclean)
library(stringi)
library(openxlsx)
library(dplyr)
library(wordVectors)
library(quanteda)
library(factoextra)
library(FactoMineR)
library(writexl)
library("reshape2") 
library(radiant)
library(word2vec)

# setwd("/Users/tatiana/Documents/ГЗ/статья_1")
# loading word2vec model using different methods
 
WE <- read.wordvectors("/Users/tatiana/Documents/Модели_векторы/НКРЯ+Russian Wikipedia Dump of November 2021/model.bin", type = "bin", normalize = F) 
str(WE1)
WE1= as.data.frame(ds)
ddd = dimnames(WE) 
str(ds)
ds=ddd[[1]]
dfff = pblapply(ds, function(x) apply(TEXT, MARGIN = c(1,2), function(y) x %in% y))
data_new5 <- as.data.frame(as.numeric(data_new$Reaction.5_POS%in% ds))
write.xlsx(data_new5, "r5.xlsx")
str(data_new)
# data preparation 
info=read.xlsx("/Users/tatiana/Documents/ГЗ/статья_1/data/этап_1.xlsx")
data=read.xlsx("/Users/tatiana/Documents/ГЗ/статья_1/data/Word_Association_dataset.xlsx")
dim(data)
data1=data[complete.cases(data), ]
dim(data1)
str(data1)
data1$Stimulus.word_POS = as.factor(data1$Stimulus.word_POS)
summary(data1$Stimulus.word_POS)
summary(data1$ID)
data1$Stage = as.factor(data1$Stage)
data1 <- 
droplevels(data1 [data1$Stage %in% "1", ])
data1$ID = as.factor(data1$ID)
View(data1)

# choose only with n=80 and more
data11 <- 
  droplevels(data1 [data1$Stimulus.word_POS %in% c("добро_NOUN", "дом_NOUN", "друг_NOUN", "жизнь_NOUN", "мир_NOUN", "настоящий_ADJ", "семья_NOUN", "счастье_NOUN", "хотеть_VERB"), ])
View(data11)
str(data11)
summary(data11$ID)
summary(data11$Stimulus.word_POS)
write.xlsx(data11, "data10words.xlsx", rownames=T)
dim(data11)
summary(data111$PAUSE.4)
data111=data11[-which(data11$PAUSE.1>20000),]
View(data2)
data111=data111[-which(data111$PAUSE.5>20000),]

# phrase association search
library(stringr)
data1111 = data11
data2 = ifelse(str_count(data1111$Reaction.5_POS, "_") > 1, "MULTIPLE", data1111$Reaction.5_POS)
library(dplyr)
sum(grepl("\\bMULTIPLE\\b", as.matrix(data2)))

# feature construction 
data_neww=read.xlsx("/Users/tatiana/Documents/ГЗ/статья_1/data10words1.xlsx") 
str(feat)
data_neww$TEXT = paste(data_neww$Reaction.1_POS, data_neww$Reaction.2_POS, data_neww$Reaction.3_POS, data_neww$Reaction_4_POS, data_neww$Reaction_4_POS)
head(data_neww)
corp_inaug <- corpus(data_neww, text_field = "TEXT")
corp_inaug
toks= tokens(corp_inaug)
toks
tokenmy1 =  tokens_select(toks, selection="remove", pattern = c("лунтик_NOUN", "*DET", "*ADP", "*PRON", "*PROPN",  "самородок_NOUN", "карамазин_NOUN", "*PART", "семицветик_NOUN", "несгибаемость_NOUN", "майнкрафт_NOUN"), padding = F)
tokenmy1
dfmnew=dfm(tokenmy1, tolower=F) 
dtm_assoc = dfmnew 

# build the semantic direction 1 (and so on till 46):
additions  <- c("смотреть_VERB", "видеть_VERB", "свет_NOUN",  "бордовый_ADJ",  "глядеть_VERB",  "бежевый_ADJ",  "бирюзовый_ADJ", "голубой_NOUN", "просмотр_NOUN",  "рыжий_ADJ", "сияние_NOUN", "светлый_ADJ", "лампа_NOUN", "солнечный_ADJ", "окно_NOUN", "сцена_NOUN", "лазурный_ADJ", "квадратный_ADJ", "васильковый_ADJ", "кудрявый_ADJ", "жест_NOUN", "серебряный_ADJ", "прочитать_VERB",  "огонь_NOUN", "безглазый_ADJ",  "расплывчатый_ADJ", "размытость_NOUN", "ослепительный_ADJ", "зажмурить_VERB", "вид_NOUN", "полумрак_NOUN", "ночник_NOUN", "длинноногий_ADJ",  "образ_NOUN", "краска_NOUN", "худой_ADJ", "красить_VERB", "рогатый_ADJ", "танец_NOUN", "жемчужный_ADJ", "облачный_ADJ", "курчавый_ADJ", "стеклянный_ADJ", "деревянный_ADJ", "зелень_NOUN", "сирень_NOUN", "безоблачный_ADJ", "рельефный_ADJ")
substracts <- c("мелодичный_ADJ", "унюхать_VERB", "дослушать_VERB",  "разговорный_ADJ", "хриплый_ADJ", "напутственный_ADJ", "внятный_ADJ", "исконный_ADJ", "обидный_ADJ", "горький_ADJ", "храпеть_VERB", "замолчать_VERB", "национализировать_VERB",  "приторный_ADJ", "щекотливый_ADJ", "будний_ADJ", "хрипеть_VERB",  "перетерпеть_VERB",  "интуитивный_ADJ", "исходный_ADJ", "лирический_ADJ", "шум_NOUN", "уклончивый_ADJ", "духовный_ADJ", "врожденный_ADJ", "сегодняшний_ADJ", "актуальный_ADJ", "завизжать_VERB", "мотив_NOUN", "топот_NOUN", "недоумевать_VERB", "поспорить_VERB", "утвердить_VERB",  "силиться_VERB", "тишина_NOUN", "добиваться_VERB", "поэтичный_ADJ", "упоительный_ADJ", "тактичный_ADJ", "дыхательный_ADJ", "удачный_ADJ", "безмолвный_ADJ",  "вкус_NOUN")
pairs1 <- cbind(additions, substracts)
head(pairs1) 
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc1_closeness <- CMDist(dtm = dfmnew, cv = sd_eval1, wv = WE)
doc1_closeness[1:10,]

# build the semantic direction 2
additions = c("белый_ADJ", "бледный_ADJ", "вид_NOUN", "видеть_VERB", "выглядеть_VERB", "глядеть_NOUN", "голубой_ADJ", "гореть_VERB", "заметить_VERB", "заметный_ADJ", "замечать_VERB", "золотой_ADJ", "картина_NOUN", "красный_ADJ", "кровавый_ADJ", "мельком_ADV", "наблюдать_VERB", "небесный_ADJ", "обнаружить_VERB", "показаться_VERB", "появиться_VERB", "рыжий_ADJ", "седа_NOUN", "серый_ADJ", "синий_ADJ", "слепой_ADJ", "смотреть_VERB", "увидеть_VERB", "цвет_NOUN", "цветный_ADJ", "яркий_ADJ") 
sc_death <- get_centroid(additions, WE)
doc2_closeness <- CMDist(dtm = dfmnew, cv = sc_death, wv = WE)
head(doc2_closeness)
doc2_closeness

# build the semantic direction 3
additions = c("светлый_ADJ", "яркий_ADJ")
substracts = c("темный_ADJ", "тусклый_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc3_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)
head(doc3_closeness)

# build the semantic direction 4
additions = c("цветный_ADJ", "голубой_ADJ", "красный_ADJ", "золотой_ADJ", "синий_ADJ", "желтый_ADJ", "зеленый_ADJ", "яркий_ADJ")
sc_death <- get_centroid(additions, WE)
doc4_closeness <- CMDist(dtm = dtm_assoc, cv = sc_death, wv = WE)

# build the semantic direction 5
additions = c("большой_ADJ", "огромный_ADJ")
substracts = c("малый_ADJ", "маленький_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc5_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 6
additions = c("быстрый_ADJ")
substracts = c("медленный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove")
doc6_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 7
additions = c("красивый_ADJ", "миловидный_ADJ")
substracts = c("некрасивый_ADJ", "страшный_ADJ", "уродливый_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc7_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 8
additions = c("голова_NOUN", "грудь_NOUN", "живот_NOUN", "ладонь_NOUN", "лицо_NOUN", "лоб_NOUN", "нога_NOUN", "нос_NOUN", "палец_NOUN", "рот_NOUN", "рука_NOUN", "тело_NOUN", "шея_NOUN", "глаз_NOUN", "веко_NOUN", "бровь_NOUN", "ухо_NOUN", "макушка_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc8_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 9
additions = c("лимон_NOUN", "клубничный_ADJ", "соль_NOUN", "цитрусовый_ADJ", "вкус_NOUN", "яблочный_ADJ", "груша_NOUN", "ягодный_ADJ", "огурец_NOUN", "обед_NOUN", "горький_ADJ", "картофельный_ADJ", "приправа_NOUN", "облепиха_NOUN", "приторный_ADJ", "рассол_NOUN", "лук_NOUN", "быть_VERB", "откушать_VERB", "жевать_VERB", "пить_VERB", "аппетит_NOUN", "неспелый_ADJ", "столовая_NOUN", "зелень_NOUN", "застолье_NOUN", "кофеин_NOUN", "тошнотворный_ADJ", "жарить_VERB", "повар_NOUN", "варить_VERB", "холодильник_NOUN", "витаминный_ADJ", "хлебать_VERB", "глотать_VERB", "кусать_VERB", "грызть_VERB", "травянистый_ADJ", "приятный_ADJ", "духовка_NOUN", "кислота_NOUN", "бар_NOUN", "сосать_VERB", "празднество_NOUN", "цветочный_ADJ", "выкормить_VERB", "мясистый_ADJ", "сытый_ADJ", "жир_NOUN", "настой_NOUN")
substracts = c("выдвиженец_NOUN", "призыв_NOUN", "глушитель_NOUN", "бастовать_VERB", "суд_NOUN", "национализировать_VERB", "скандал_NOUN", "завал_NOUN", "конкурировать_VERB", "топот_NOUN", "ополченец_NOUN", "вызов_NOUN",  "щелчок_NOUN", "порыв_NOUN", "отчуждение_NOUN", "замедление_NOUN", "беглец_NOUN", "бег_NOUN", "движение_NOUN", "надгробие_NOUN", "полумрак_NOUN", "терроризировать_VERB", "шпионить_VERB", "светить_VERB",  "выслуживаться_VERB", "натяжение_NOUN", "дуэль_NOUN", "отыскать_VERB", "лужа_NOUN", "рост_NOUN", "ночник_NOUN", "мотив_NOUN", "утвердить_VERB", "фронтовик_NOUN", "пододеяльник_NOUN", "остановка_NOUN", "паутина_NOUN", "поворот_NOUN", "уязвимый_VERB", "неразлучный_ADJ", "лежачий_NOUN",  "стоять_VERB", "грозить_VERB", "подгузник_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc9_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 10
additions = c("горький_ADJ", "соленый_ADJ")
substracts = c("сладкий_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc10_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 11
additions = c("шум_NOUN", "будильник_NOUN", "кричать_VERB", "музыкант_NOUN", "мелодичный_ADJ", "проигрыватель_NOUN", "тишина_NOUN", "завизжать_VERB", "свистеть_VERB", "храпеть_VERB", "топот_NOUN", "скандал_NOUN", "говорить_VERB", "щелчок_NOUN", "стучать_VERB", "звать_VERB", "хриплый_ADJ", "хлопать_VERB", "монотонный_ADJ", "фен_NOUN", "звонить_VERB", "поезд_NOUN", "вечеринка_NOUN", "шептать_VERB", "трещать_VERB", "хлопать_VERB", "безмолвный_ADJ", "слушать_VERB", "хрипеть_VERB", "кашлять_VERB", "разговорный_ADJ", "комар_NOUN",  "пистолет_NOUN", "щелкать_VERB", "застолье_NOUN", "глушитель_NOUN", "шаркать_VERB", "война_NOUN", "веселиться_VERB", "сцена_NOUN", "вздох_NOUN", "чайник_NOUN", "поэтичный_ADJ", "морской_ADJ", "танец_NOUN", "рынок_NOUN", "муха_NOUN", "внятный_ADJ", "вихрь_NOUN")
substracts = c("загорелый_ADJ", "жир_NOUN", "вид_NOUN", "размытость_NOUN", "хранить_VERB", "гниль_NOUN", "йод_NOUN", "квадратный_ADJ", "сберечь_VERB", "рассол_NOUN", "груша_NOUN", "сорняк_NOUN", "кофеин_NOUN", "глядеть_VERB", "клей_NOUN", "приправа_NOUN", "клубничный_ADJ", "копить_VERB", "форма_NOUN", "вкус_NOUN", "светить_VERB", "искривленный_VERB", "сидеть_VERB", "ржавчина_NOUN",  "волокнистый_ADJ", "ночник_NOUN", "медуница_NOUN", "зажмурить_VERB", "безглазый_ADJ", "высохнуть_VERB", "чернильный_ADJ", "паутина_NOUN", "длинноногий_ADJ", "бордовый_ADJ", "вес_NOUN", "мазь_NOUN", "гнойный_ADJ", "настой_NOUN", "зелень_NOUN", "пододеяльник_NOUN", "полнота_NOUN", "национализировать_VERB", "рост_NOUN", "сияние_NOUN", "спрятать_VERB", "курчавый_ADJ", "украсть_VERB", "облепиха_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc11_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 12
additions = c("бить_VERB", "глухой_ADJ", "громкий_ADJ", "звонить_VERB", "звучать_VERB", "крик_NOUN",  "кричать_VERB", "музыкальный_ADJ", "ответить_VERB", "петь_VERB", "раздаться_VERB", "слушать_VERB", "слышать_VERB", "тихий_ADJ", "тишина_NOUN", "шум_NOUN", "стук_NOUN", "звон_NOUN", "писк_NOUN", "хрип_NOUN", "звук_NOUN", "отзвук_NOUN", "гудок_NOUN", "слово_NOUN", "речь_NOUN", "кашель_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc12_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 13
additions = c("громкий_ADJ")
substracts = c("тихий_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc13_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


#build the semantic direction 14

additions = c("трогать_VERB", "пушистый_ADJ", "шершавый_ADJ", "мягкость_NOUN", "холодный_ADJ", "теплый_NOUN", "уколоть_VERB", "целовать_VERB", "жидкий_ADJ", "рельефный_ADJ", "скользкий_ADJ", "пожать_VERB", "ожог_NOUN", "деревянный_ADJ", "держать_VERB", "укол_NOUN", "схватить_VERB", "жесткий_ADJ", "поцарапать_VERB", "липучка_NOUN", "бумажный_ADJ", "плотный_ADJ", "песочный_ADJ", "мыть_VERB",   "боль_NOUN", "хлопать_VERB", "еж_NOUN", "стеклянный_ADJ", "брить_VERB", "брать_VERB", "нагрев_NOUN", "нож_NOUN", "каменистый_ADJ", "мыльный_ADJ", "вязать_VERB", "месить_VERB", "танец_NOUN", "охлаждение_NOUN", "волокнистый_ADJ", "пластмассовый_ADJ", "железный_ADJ", "огонь_NOUN", "замерзнуть_VERB", "резать_VERB", "упасть_VERB", "мазь_NOUN", "глинистый_ADJ", "плыть_VERB")
substracts = c("национализировать_VERB", "тон_NOUN",  "суждение_NOUN", "надменный_ADJ", "честный_ADJ", "будний_ADJ", "мотив_NOUN",  "пропагандировать_VERB", "выдвиженец_NOUN", "призывать_VERB", "разговорный_ADJ", "верить_VERB", "бойкотировать_VERB", "знать_VERB", "просмотр_NOUN", "исконный_ADJ", "размытость_NOUN", "смотреть_VERB", "выпрашивать_VERB", "завтрашний_ADJ",  "думать_VERB", "радиоактивный_ADJ", "недоумевать_VERB", "безмолвный_ADJ", "удачливый_ADJ", "актуальный_ADJ", "глумливый_ADJ", "безрассудный_ADJ", "шум_NOUN", "преуспевать_VERB", "выслуживаться_VERB", "гордый_ADJ", "праведный_ADJ", "напутственный_ADJ", "вызов_NOUN", "замолчать_VERB", "полярный_ADJ", "конкурировать_VERB", "гордиться_VERB", "реактивный_ADJ", "дослушать_VERB", "исходный_ADJ", "лукавый_ADJ", "обидчивый_ADJ", "обаятельный_ADJ", "внятный_ADJ", "ведать_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc14_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 15
additions = c("влажный_ADJ", "гладкий_ADJ", "горячий_ADJ", "клейкий_ADJ", "колкий_ADJ", "колючий_ADJ", "леденить_VERB", "ледяной_ADJ", "липкий_ADJ", "мокрый_ADJ", "мягкий_ADJ", "сыр_NOUN", "холодный_ADJ", "шероховатый_ADJ", "шершавый_ADJ", "щетинистый_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc15_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 16
additions = c("теплый_ADJ", "горячий_ADJ") 
substracts = c("холодный_ADJ", "ледяной_ADJ", "прохладный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc16_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 17
additions = "легкий_ADJ"
substracts = "тяжелый_ADJ"
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc17_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 18
additions = "гладкий_ADJ"
substracts = "жесткий_ADJ"
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="paired")
doc18_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 19
additions = c("болеть_VERB", "больной_NOUN", "боль_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc19_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 20

additions = c("нюхать_VERB", "сирень_NOUN", "хвойный_ADJ", "цветочный_ADJ", "цитрусовый_ADJ", "хлорка_NOUN", "яблочный_ADJ", "приправа_NOUN", "клубничный_ADJ", "курить_VERB", "скунс_NOUN", "ягодный_ADJ", "морской_ADJ", "столовая_NOUN", "обед_NOUN", "лук_NOUN", "тошнотворный_ADJ",   "жарить_VERB", "краска_NOUN", "груша_NOUN", "духовка_NOUN", "лимон_NOUN", "гниль_NOUN", "кофеин_NOUN", "туалет_NOUN", "огурец_NOUN", "мазь_NOUN", "варить_VERB", "облепиха_NOUN", "приторный_ADJ", "больница_NOUN", "красить_VERB", "зелень_NOUN",   "рынок_NOUN", "клей_NOUN", "травянистый_ADJ", "повар_NOUN", "рассол_NOUN", "застолье_NOUN", "холодильник_NOUN", "подгузник_NOUN", "приятный_ADJ", "токсичный_ADJ", "картофельный_ADJ", "уборная_NOUN", "земляной_ADJ", "дождливый_ADJ", "медуница_NOUN") 
substracts = c("бойкотировать_VERB", "давление_NOUN", "слушать_VERB", "выдвиженец_NOUN", "призыв_NOUN", "молить_VERB", "мотив_NOUN", "выслуживаться_VERB", "агитировать_VERB", "ведать_VERB", "ополченец_NOUN", "национализировать_VERB", "вызов_NOUN", "бежевый_ADJ", "помирить_VERB", "отчуждение_NOUN", "голосовать_VERB", "робеть_VERB", "рост_NOUN", "градусник_NOUN", "герой_NOUN", "размер_NOUN", "комар_NOUN", "огорчаться_VERB", "хлопать_VERB", "разговорный_ADJ", "соперничать_VERB", "поворот_NOUN", "звонить_VERB", "щелчок_NOUN", "худой_ADJ", "скользить_VERB", "стучать_VERB", "замедление_NOUN", "ночник_NOUN",   "жест_NOUN", "осмыслять_VERB", "звать_VERB", "бастовать_VERB", "визжать_VERB", "воровать_VERB", "будильник_NOUN", "ухабистый_ADJ", "паутина_NOUN", "решать_VERB", "шпионить_VERB", "добиваться_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc20_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


# build the semantic direction 21
additions = c("улыбаться_VERB", "говорить_VERB", "щуриться_VERB", "моргать_VERB", "двигаться_VERB", "писать_VERB", "стучать_VERB", "бежать_VERB", "идти_VERB", "семенить_VERB", "бить_VERB", "кричать_VERB", "крик_NOUN", "спускаться_VERB", "поход_NOUN", "ходить_VERB", "ползать_VERB", "лететь_VERB", "шаг_NOUN", "бег_NOUN", "прыжок_NOUN", "прыгать_VERB", "бросок_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc21_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 22

additions = c("ложка_NOUN", "дрель_NOUN", "молоток_NOUN", "ручка_NOUN", "автомобиль_NOUN", "грабли_NOUN", "ластик_NOUN", "вилка_NOUN", "пинцет_NOUN", "нож_NOUN", "игла_NOUN", "велосипед_NOUN", "карандаш_NOUN", "машина_NOUN", "телефон_NOUN", "сковорода_NOUN", "фломастер_NOUN", "застежка_NOUN", "циркуль_NOUN", "лопата_NOUN", "клавиша_NOUN", "отвертка_NOUN", "тарелка_NOUN", "веник_NOUN", "стакан_NOUN", "наперсток_NOUN", "топор_NOUN", "напильник_NOUN",  "маркер_NOUN", "расческа_NOUN", "скальпель_NOUN", "копье_NOUN", "ножницы_NOUN", "дротик_NOUN", "пуговица_NOUN", "бумеранг_NOUN", "ключ_NOUN", "бисер_NOUN", "шнурок_NOUN", "мотоцикл_NOUN", "гайка_NOUN",  "кисть_NOUN", "мопед_NOUN", "пить_VERB", "гвоздь_NOUN", "штопор_NOUN", "самокат_NOUN", "запонка_NOUN")
substracts = c("ностальгия_NOUN", "гора_NOUN", "галлюцинация_NOUN", "стопа_NOUN", "презрение_NOUN", "яркость_NOUN", "зависть_NOUN", "скука_NOUN", "тошнота_NOUN", "акцент_NOUN", "мысль_NOUN", "отчаяние_NOUN", "лев_NOUN", "крокодил_NOUN", "инстинкт_NOUN", "фактор_NOUN", "стыд_NOUN", "понятие_NOUN", "допущение_NOUN", "небосвод_NOUN", "депрессия_NOUN", "цвет_NOUN", "тон_NOUN", "писк_NOUN", "тоска_NOUN", "воображение_NOUN", "луна_NOUN", "звезда_NOUN", "произношение_NOUN", "отговорка_NOUN", "сочность_NOUN", "вдох_NOUN", "молчание_NOUN", "факт_NOUN", "смысл_NOUN", "динозавр_NOUN", "равнодушие_NOUN", "рассвет_NOUN", "даль_NOUN", "обоняние_NOUN", "совет_NOUN", "солнце_NOUN", "сон_NOUN", "воспоминание_NOUN", "досада_NOUN", "облако_NOUN", "небо_NOUN", "отвращение_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc22_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 23

additions = c("близкий_ADJ", "близко_ADV", "приближаться_VERB", "приходить_VERB")
substracts = c("далеко_ADV", "дальний_ADJ", "отдаляться_VERB", "уходить_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc23_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 24
additions = c("даль_ADV", "звезда_NOUN", "луна_NOUN", "метеорит_NOUN", "молния_NOUN", "небо_NOUN", "небосвод_NOUN", "небосклон_NOUN", "облако_NOUN", "потолок_NOUN", "пространство_NOUN", "радуга_NOUN", "рассвет_NOUN", "солнце_NOUN", "спутник_NOUN", "туча_NOUN", "фейерверк_NOUN", "чердак_NOUN", "штиль_NOUN")
substracts = c("асфальт_NOUN", "впадина_NOUN", "газон_NOUN", "гора_NOUN", "грязь_NOUN", "дно_NOUN", "колодец_NOUN", "люк_NOUN", "могила_NOUN", "паркет_NOUN", "плинтус_NOUN", "пол_NOUN", "порог_NOUN", "почва_NOUN", "прорубь_NOUN",  "пруд_NOUN", "сор_NOUN", "трава_NOUN", "тротуар_NOUN", "углубление_NOUN", "шоссе_NOUN", "шпала_NOUN", "яма_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc24_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 25
additions = c("один_NUM", "два_NUM", "три_NUM", "четыре_NUM", "пять_NUM", "шесть_NUM", "семь_NUM", "восемь_NUM", "девять_NUM", "десять_NUM", "сто_NUM","тысяча_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc25_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 26

additions = c("бесконечный_ADJ", "беспрерывно_ADV", "ближайший_ADJ", "будущий_ADJ", "бывший_ADJ", "век_NOUN", "временный_ADJ", "вчера_ADV", "далее_ADV", "долго_ADV", "древний_ADJ", "жизнь_NOUN", "завтра_NOUN", "закончить_VERB", "заново_ADV", "заранее_ADV", "история_NOUN", "кончить_VERB", "мгновение_NOUN", "миг_NOUN", "минута_NOUN", "момент_NOUN", "моментальный_ADJ", "навеки_ADV", "начало_NOUN", "начать_VERB", "неделя_NOUN", "немедленно_ADV", "ныне_ADV", "нынешний_ADJ", "нынче_ADV", "окончание_NOUN", "опять_VERB", "позднее_ADV", "поздний_ADJ", "позже_ADV", "постепенно_ADV", "предварительно_ADV", "прежде_ADV", "ранее_ADV", "ранний_ADJ", "рано_ADV", "сегодня_ADV", "секунда_NOUN", "современный_ADJ", "сразу_ADV", "срок_NOUN", "старый_ADJ", "успеть_VERB", "час_NOUN", "этап_NOUN", "эпоха_NOUN", "юный_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc26_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 27

additions = c("длительный_ADJ", "долгий_ADJ")
substracts = c("быстрый_ADJ", "короткий_ADJ", "мгновенный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc27_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 28

additions = c("новый_ADJ")
substracts = c("старый_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove")
doc28_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


#build the semantic direction 29

additions = c("последующий_ADJ", "причина_NOUN", "потому_ADV", "поэтому_ADV", "следовательно_ADV")
sd_eval1 <- get_centroid(additions, WE)
doc29_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 30

additions = c("благородный_ADJ", "добрый_ADJ", "искренний_ADJ", "ласковый_ADJ", "нежный_ADJ", "независимый_ADJ", "порядочный_ADJ", "разумный_ADJ", "решительный_ADJ", "талантливый_ADJ", "творческий_ADJ", "уверенный_ADJ", "умный_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc30_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 31
additions = c("беседа_NOUN", "говорить_VERB", "договор_NOUN", "знакомый_ADJ", "коллега_NOUN", "конфликт_NOUN", "лидер_NOUN", "личность_NOUN", "любовь_NOUN", "бабушка_NOUN", "мальчик_NOUN", "мама_NOUN", "мать_NOUN", "муж_NOUN", "начальник_NOUN", "обсуждать_VERB", "общество_NOUN", "отец_NOUN", "папа_NOUN", "письмо_NOUN", "поддержка_NOUN", "подруга_NOUN", "помощь_NOUN", "понимание_NOUN", "понимать_VERB", "приятель_NOUN", "проговорить_VERB", "работа_NOUN", "работать_VERB", "разговор_NOUN", "разговаривать_VERB", "рассказывать_VERB", "речь_NOUN", "родитель_NOUN", "семейный_ADJ", "семья_NOUN", "сообщить_VERB", "сообщение_NOUN", "спор_NOUN", "товарищ_NOUN", "улыбаться_VERB", "улыбка_NOUN", "чуждый_ADJ", "шутка_NOUN", "смех_NOUN")
sd_eval1 <- get_centroid(additions, WE)
doc31_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 32
additions = c("мужчина_NOUN", "мужской_ADJ")
substracts = c("женщина_NOUN", "женский_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc32_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 33

additions = c("дух_NOUN", "душа_NOUN", "судьба_NOUN", "свобода_NOUN", "искусство_NOUN", "время_NOUN", "любовь_NOUN", "надежда_NOUN", "принятие_NOUN", "красота_NOUN", "счастье_NOUN", "мечта_NOUN", "ощущение_NOUN", "основной_ADJ", "сознание_NOUN", "изучение_NOUN", "мысль_NOUN", "потребность_NOUN", "творчество_NOUN", "воспоминание_NOUN", "открытие_NOUN", "эффективность_NOUN", "энергия_NOUN", "образ_NOUN", "необходимость_NOUN", "воздействие_NOUN", "будущий_ADJ", "чувство_NOUN", "развитие_NOUN", "сомнение_NOUN", "чудо_NOUN", "эффект_NOUN", "сожаление_NOUN", "реальность_NOUN", "стиль_NOUN", "добро_NOUN", "главный_ADJ", "взаимодействие_NOUN", "пространство_NOUN", "соединение_NOUN", "вера_NOUN", "увеличение_NOUN", "культура_NOUN", "разница_NOUN", "разрешение_NOUN", "выполнение_NOUN", "назначение_NOUN", "описание_NOUN", "перспектива_NOUN") 
substracts = c("стол_NOUN", "самолет_NOUN", "мальчик_NOUN", "квартира_NOUN", "больница_NOUN", "врач_NOUN", "водка_NOUN", "платье_NOUN", "кровать_NOUN", "девочка_NOUN", "телефон_NOUN", "пальто_NOUN", "фотография_NOUN", "рубль_NOUN", "поезд_NOUN", "птица_NOUN", "стакан_NOUN", "остров_NOUN", "сосед_NOUN", "мясо_NOUN", "бабушка_NOUN", "стекло_NOUN", "кресло_NOUN", "жена_NOUN", "стул_NOUN", "ухо_NOUN", "ладонь_NOUN", "этаж_NOUN", "автомобиль_NOUN", "книжка_NOUN", "лошадь_NOUN", "спина_NOUN", "телевизор_NOUN", "слеза_NOUN", "автобус_NOUN", "газета_NOUN", "бутылка_NOUN", "вино_NOUN", "шея_NOUN", "диван_NOUN", "пиво_NOUN", "танк_NOUN", "рука_NOUN", "нож_NOUN", "щека_NOUN", "мужчина_NOUN", "студент_NOUN", "хлеб_NOUN", "пистолет_NOUN", "нога_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc33_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 34

additions = c("малина_NOUN", "морковь_NOUN", "собака_NOUN", "персик_NOUN", "лампочка_NOUN", "вилка_NOUN", "топор_NOUN", "кружок_NOUN", "яблоко_NOUN", "мяч_NOUN", "арбуз_NOUN", "сапог_NOUN", "фейерверк_NOUN", "глаз_NOUN", "таракан_NOUN", "медведь_NOUN", "апельсин_NOUN", "нитка_NOUN", "лимон_NOUN", "потолок_NOUN", "мухомор_NOUN", "черепаха_NOUN", "пить_VERB", "самолет_NOUN", "укроп_NOUN", "заяц_NOUN", "тигр_NOUN", "тыква_NOUN", "автомобиль_NOUN", "велосипед_NOUN", "тарелка_NOUN", "мопед_NOUN", "мандарин_NOUN", "грейпфрут_NOUN", "баклажан_NOUN", "ботинок_NOUN", "лягушка_NOUN", "акула_NOUN", "санки_NOUN", "утка_NOUN", "игла_NOUN", "расческа_NOUN", "трамвай_NOUN", "кошка_NOUN", "перец_NOUN", "ананас_NOUN", "бабочка_NOUN", "солнце_NOUN", "облако_NOUN", "отвертка_NOUN")
substracts = c("ударение_NOUN", "идея_NOUN", "понимание_NOUN", "уныние_NOUN", "досада_NOUN", "уход_NOUN", "расширение_NOUN", "стратегия_NOUN", "тяга_NOUN", "галлюцинация_NOUN", "отвращение_NOUN", "мысль_NOUN", "отчаяние_NOUN", "факт_NOUN", "замысел_NOUN", "любовь_NOUN", "вина_NOUN", "сочувствие_NOUN", "переживание_NOUN", "набор_NOUN", "произношение_NOUN", "отговорка_NOUN", "волнение_NOUN", "образ_NOUN", "хрип_NOUN", "стыд_NOUN", "тревога_NOUN", "воображение_NOUN", "выбор_NOUN", "стресс_NOUN", "свет_NOUN", "осязание_NOUN", "сужение_NOUN", "запах_NOUN", "зависть_NOUN", "стимул_NOUN", "восприятие_NOUN", "теория_NOUN", "навык_NOUN", "попытка_NOUN", "инстинкт_NOUN", "отбор_NOUN", "тон_NOUN", "фактор_NOUN", "сознание_NOUN", "значение_NOUN", "смысл_NOUN", "гипотеза_NOUN", "допущение_NOUN")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc34_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 35

additions = c("абсолютно_ADV", "безусловно_ADV", "более_ADV", "возможный_ADJ", "доказательство_NOUN", "думать_VERB", "задача_NOUN", "знать_VERB", "итог_NOUN", "конкретный_ADJ", "лишний_ADJ", "максимальный_ADJ", "мнение_NOUN", "наблюдение_NOUN", "наверняка_ADV", "намеренный_ADJ", "наоборот_ADV", "напоминать_VERB", "невероятный_ADJ", "недостаточно_ADV", "неизвестный_ADJ", "необходимый_ADJ", "непонятный_ADJ", "несомненно_ADV", "несмотря_ADV", "объяснение_NOUN", "обычно_ADV", "обычный_ADJ", "одинаково_ADV", "одинаковый_ADJ", "основной_ADJ", "отличие_NOUN", "очевидный_ADJ", "понятие_NOUN", "понять_VERB", "потребность_NOUN", "правило_VERB", "признак_NOUN", "причина_NOUN", "проблема_NOUN", "процесс_NOUN", "разница_NOUN", "реальность_NOUN", "результат_NOUN", "ситуация_NOUN", "смысл_NOUN", "событие_NOUN", "сознание_NOUN", "сравнение_NOUN", "теоретически_ADV", "удивительно_ADV", "ум_NOUN", "явление_NOUN", "явно_ADV", "явный_ADJ", "ясно_ADJ")
sd_eval1 <- get_centroid(additions, WE)
doc35_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)


# build the semantic direction 36
additions = c("война_NOUN", "смерть_NOUN", "убить_VERB", "мертвый_ADJ", "погибнуть_NOUN", "умереть_VERB", "несчастный_ADJ", "потерять_VERB", "уголовный_ADJ", "опасный_ADJ", "злой_NOUN", "пьяный_ADJ", "плохой_ADJ", "страшный_ADJ", "больной_ADJ", "удар_NOUN", "бояться_VERB", "очередь_NOUN", "грязный_ADJ", "глупый_ADJ", "слабый_ADJ", "вооруженный_ADJ", "бедный_ADJ", "судебный_ADJ", "бить_VERB", "бросить_VERB", "заставить_VERB", "судить_VERB", "плакать_VERB", "падать_VERB", "налоговый_ADJ", "оружие_NOUN", "упасть_VERB", "кровь_NOUN", "зависеть_VERB", "невозможный_ADJ", "лишний_ADJ", "мешать_VERB", "чужой_ADJ", "пустой_ADJ", "исчезнуть_VERB", "партийный_ADJ", "кончиться_VERB", "толстый_ADJ", "конец_NOUN", "дикий_ADJ", "кричать_VERB")
substracts = c("мама_NOUN", "счастье_NOUN", "улыбнуться_VERB", "здоровый_ADJ", "родной_ADJ", "успех_NOUN", "любимый_ADJ", "лучший_ADJ", "счастливый_ADJ", "любовь_NOUN", "любить_VERB", "солнце_NOUN", "довольный_ADJ", "хороший_ADJ", "море_NOUN", "красивый_ADJ", "солнечный_ADJ", "замечательный_ADJ", "друг_NOUN", "положительный_ADJ", "смеяться_VERB", "прекрасный_ADJ", "природа_NOUN", "милый_ADJ", "жить_VERB", "свобода_NOUN", "образование_NOUN", "небо_NOUN", "добрый_ADJ", "жизнь_NOUN", "семья_NOUN", "герой_NOUN", "светлый_ADJ", "культура_NOUN", "веселый_ADJ", "девушка_NOUN", "искусство_NOUN", "умный_ADJ", "живой_ADJ", "активный_ADJ", "летний_ADJ", "развитие_NOUN", "сын_NOUN", "мать_NOUN", "создавать_VERB", "отец_NOUN", "интересный_ADJ", "миллион_NOUN", "смешной_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc36_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 37
additions = c("полезный_ADJ")
substracts = c("вредный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove")
doc37_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 38
additions = c("приятный_ADJ")
substracts = c("неприятный_ADJ", "противный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc38_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 39
additions = c("счастливый_ADJ", "радостный_ADJ")
substracts = c("грустный_ADJ", "несчастный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc39_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#build the semantic direction 40

additions = c("раздраженный_VERB", "разъяренный_VERB")
substracts = c("спокойный_ADJ", "умиротворенный_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc40_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 41  
additions = c("отвращение_NOUN", "неприязнь_NOUN")
substracts = c("влечение_NOUN", "любовь_NOUN", "любимый_ADJ", "любимый_VERB")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc41_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 42  
additions = c("опасный_ADJ", "страх_NOUN", "пугать_VERB")
substracts = c("безопасный_ADJ", "безобидный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc42_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 43

additions = c("удивлять_VERB", "поражать_VERB")
substracts = c("скучать_VERB", "скучный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc43_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 44
additions = c("вялый_ADJ", "апатичный_ADJ")
substracts = c("живой_ADJ", "буйный_ADJ", "бодрый_ADJ", "энергичный_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc44_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 45  
additions = c("необходимый_ADJ", "нужный_ADJ")
substracts = c("ненужный_ADJ", "лишний_ADJ")
pairs1 <- cbind(additions, substracts)
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc45_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

# build the semantic direction 46  
additions = c("взволнованный_VERB", "волновать_VERB", "волнительный_ADJ")
substracts = c("спокойный_ADJ", "безразличный_ADJ", "умиротворенный_VERB")
pairs1 <- cbind(additions, substracts) 
sd_eval1 <- get_direction(pairs1, WE, missing="remove", method="pooled")
doc46_closeness <- CMDist(dtm = dtm_assoc, cv = sd_eval1, wv = WE)

#final dataset construction
library(tidyverse)
#put all data frames into list
df_list <- list(doc1_closeness, doc2_closeness, doc3_closeness, doc4_closeness, doc5_closeness, doc6_closeness, doc7_closeness, doc8_closeness, doc9_closeness, doc10_closeness, doc11_closeness, doc12_closeness, doc13_closeness, doc14_closeness, doc15_closeness, doc16_closeness, doc17_closeness, doc18_closeness, doc19_closeness, doc20_closeness, doc21_closeness, doc22_closeness, doc23_closeness, doc24_closeness, doc25_closeness, doc26_closeness, doc27_closeness, doc28_closeness, doc29_closeness, doc30_closeness, doc31_closeness, doc32_closeness, doc33_closeness, doc34_closeness, doc35_closeness, doc36_closeness, doc37_closeness, doc38_closeness, doc39_closeness, doc40_closeness, doc41_closeness, doc42_closeness, doc43_closeness, doc44_closeness, doc45_closeness, doc46_closeness)      

# merge all data frames together
df_list %>% reduce(full_join, by='doc_id')
df_list = as.data.frame(df_list)
df_list = df_list[!duplicated(as.list(df_list))]
View(df_list)
names = c("doc_id", "VisNorms", 	"VisLIWC", 	"VisIntens",	"VisColor",	"VisSize",	"VisMotion", 	"VisFace", "VisBody",	"GustNorms", 	"GustTaste",	"AudNorms",	"AudLIWC",	"AudIntens",	"SomatNorms",	"SomatLIWC",	"SomatSurface", "SomatProprioception",	 "SomatTexture",	"SomatNociception",	"OlfacNorms",	 "MotorBinder",	 "MotorPractice",	"SpatialProx", 	"SpatialUpDown",	"SpatialNumber",	"TemporalLIWC",	"TempDuration", "TempAge",	"Causal",	 "SocialSelf",	"SocialLIWC",	 "SocialGender",	 "CognitionAbstract",  "CognitionImage",	"CognitionLIWC",	"EmoSentiment",	"EmoBenefit",	"EmoPleasant",	"EmoHappy",	"EmoAngry",	"EmoDisgust",	"EmoFear",	"EmoSurprised",	"Drive",	"DriveNeeds",	"AttentionArousal")
colnames(df_list) = names

# examining pause 
pause = read.xlsx("/Users/tatiana/Documents/ГЗ/статья_1/data10words1.xlsx", sheet=2)
str(data_long)
pause$AU=factor(pause$AU)
data_long <- gather(pause, place, duration, PAUSE.1:PAUSE.5, factor_key=TRUE)
View(data_long)
hist(data_long$duration)
boxplot.stats(data_long$duration)$out
out <- boxplot.stats(data_long$duration)$out
summary(out)
boxplot(data_long$duration,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
ggplot(filtered_mtcars, aes(x = place, y = duration, fill = place))  + 
  geom_violin(trim = FALSE) + 
  xlab("группы") + 
  ylab("баллы") + 
  ggtitle("Баллы за контрольную работу", subtitle = "(из 25)") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
filtered_mtcars <- data_long[data_long$duration < 8753 , ]
view(filtered_mtcars)
fit = aov(duration ~ place, data = filtered_mtcars) 
model  <- lm(duration ~ place, data = filtered_mtcars)
boxplot(filtered_mtcars$duration)
ggqqplot(residuals(model))
shapiro_test(residuals(model))
library(tidyverse)
library(ggpubr)
library(rstatix)
aa=kruskal.test(duration ~ place, data = filtered_mtcars)
filtered_mtcars %>% kruskal_effsize(duration ~ place)
pwc <- filtered_mtcars %>% 
  wilcox_test(duration ~ place, p.adjust.method = "bonferroni") 
pwc
p <- ggplot(filtered_mtcars, aes(x=place, y=duration)) + 
  geom_violin()
p + coord_flip()
p + geom_boxplot(width=0.1)
p <- ggplot(filtered_mtcars, aes(x=place, y=duration, fill=place)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Violin plot of pause duration distribution", x="Pause number", y = "Pause duration (ms)")
p + theme_classic()+ theme(legend.position = 'none') + coord_flip()

pause11= pause %>% 
  mutate(numtwos = rowSums(select(., c(5:8, )) > 2000))  %>% mutate(numtwos = rowSums(select(., c(5:8, )) < 8753))      
pause111= pause %>% 
  mutate(numtwos = rowSums(select(., c(5:8, )) > 2000))

summary(pause111$numtwos)

pause2= pause %>% 
  mutate(numtwos = rowSums(select(., c(5:8)) > 8753))
head(featpause)
dim(featpause)
featpause = cbind(pause111$numtwos, pause2$numtwos)
feat =cbind(df_list, featpause)
str(data_neww)
data = data_neww[, c(1:3)]
View(data)
head(new_feat)
new_feat=cbind(data, feat)
view(new_feat)
colnames(new_feat)[51] <- "Cognitive_pauses"
colnames(new_feat)[52] <- "Long_pauses"
info1=read.xlsx("/Users/tatiana/Documents/ГЗ/статья_1/info1.xlsx")
dim(joined_df)
joined_df <- merge(new_feat, info1, by = "AU", 
                     all.x = TRUE, all.y = FALSE)
joined_df$Gender=as.factor(joined_df$Gender)
summary(joined_df$Gender)
joined_df$Cognitive_pauses=pause111$numtwos
View(joined_df)
write.xlsx(joined_df, "allfeat.xlsx")

# analysis
#subset rows for specific stimulus
library("factoextra")
df_дом <- joined_df[joined_df$Stimulus.word_POS == 'дом_NOUN', ]
View(df_дом)
df_дом$Gender=as.factor(df_дом$Gender)
summary(df_дом$Gender)
df_дом = df_дом[, -c(1:4)]
pca_house=PCA(df_дом, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=FALSE)
pca_house$eig
fviz_pca_var(pca_house)
fviz_pca_var(pca_house, col.var="contrib")+
  
  theme_minimal()
plot(pca_house, choix="ind")
dd=dimdesc(pca_house, axes = 1:3, proba = 0.05)
dd$Dim.1$quali
res = catdes(df_дом, num.var=49, proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 

res.hcpc_house = HCPC(pca_house, min=2)
plot(res.hcpc_house, choice = "3D.map")
fviz_dend(res.hcpc_house, show_labels = T)
fviz_cluster(res.hcpc_house, geom = "point", main = "Factor map")
fviz_cluster(res.hcpc_house,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
res.hcpc_house$desc.var$test.chi2
res.hcpc_house$desc.var$quanti
res.hcpc_house$desc.var$category

# family
df_family <- joined_df[joined_df$Stimulus.word_POS == 'семья_NOUN', ]
View(df_family)
df_family$Gender=as.factor(df_family$Gender)
summary(df_family$Gender)
df_family = df_family[, -c(1:4)]
pca_family=PCA(df_family, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=FALSE)
head(pca_family$eig)
fviz_pca_var(pca_family)
plot(pca_family, choix="ind")
dd=dimdesc(pca_family, axes = 1:3, proba = 0.05)
dd$Dim.2$quanti
res = catdes(df_family, num.var=49, proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 

res.hcpc = HCPC(pca_family)

res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var


# real
df_real <- joined_df[joined_df$Stimulus.word_POS == 'настоящий_ADJ', ]
View(df_real)
df_real$Gender=as.factor(df_real$Gender)
summary(df_real$Gender)
df_real = df_real[, -c(1:4)]
df_real1=PCA(df_real, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=FALSE)
head(df_real1$eig)
fviz_pca_var(df_real1)
plot(df_real1, choix="ind")
dd=dimdesc(df_real1, axes = 1:3, proba = 0.05)
dd$Dim.2$quanti
res = catdes(df_real, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(df_real1, proba=0.05)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2  

# good
good <- joined_df[joined_df$Stimulus.word_POS == 'добро_NOUN', ]
View(df_real)
good$Gender=as.factor(good$Gender)
summary(df_real$Gender)
good = good[, -c(1:4)]
good1=PCA(good, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=T)
head(good1$eig)
fviz_pca_var(df_real1)
plot(df_real1, choix="ind")
dd=dimdesc(good1, axes = 1:3, proba = 0.05)
dd$Dim.1$quanti
res = catdes(good, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(df_real1, proba=0.05, min=2)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2

require(Factoshiny)
res <- Factoshiny(iris[,1:4])

# 	счастье
happy <- joined_df[joined_df$Stimulus.word_POS == 'счастье_NOUN', ]
View(df_real)
happy$Gender=as.factor(happy$Gender)
summary(df_real$Gender)
happy = happy[, -c(1:4)]
happy1=PCA(happy, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=T)
head(happy1$eig)
fviz_pca_var(happy1)
plot(happy1, choix="ind")
dd=dimdesc(happy1, axes = 1:3, proba = 0.05)
dd$Dim.1$quanti
res = catdes(happy, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(happy1, proba=0.05, min=2)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2

# 	друг_NOUN
friend <- joined_df[joined_df$Stimulus.word_POS == 'друг_NOUN', ]
View(df_real)
friend$Gender=as.factor(friend$Gender)
summary(df_real$Gender)
friend = friend[, -c(1:4)]
friend1=PCA(friend, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=T)
head(friend1$eig)
fviz_pca_var(happy1)
plot(friend1, choix="ind")
dd=dimdesc(friend1, axes = 1:3, proba = 0.05)
dd$Dim.2$quanti
res = catdes(friend, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(friend1, proba=0.05, min=2)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2

# жизнь_NOUN

life <- joined_df[joined_df$Stimulus.word_POS == 'жизнь_NOUN', ]
View(df_real)
life$Gender=as.factor(life$Gender)
summary(df_real$Gender)
life = life[, -c(1:4)]
life1=PCA(life, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=T)
head(life1$eig)
fviz_pca_var(happy1)
plot(life1, choix="ind")
dd=dimdesc(life1, axes = 1:3, proba = 0.05)
dd$Dim.2$quanti
res = catdes(life, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(life1, proba=0.05, min=2)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2

# хотеть_VERB

want <- joined_df[joined_df$Stimulus.word_POS == 'хотеть_VERB', ]
View(df_real)
want$Gender=as.factor(want$Gender)
summary(df_real$Gender)
want = want[, -c(1:4)]
want1=PCA(want, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=T)
head(want1$eig)
fviz_pca_var(want1)
plot(want1, choix="ind")
dd=dimdesc(want1, axes = 1:3, proba = 0.05)
dd$Dim.2$quanti
res = catdes(want, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(want1, proba=0.05, min=2)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2

# мир_NOUN

world <- joined_df[joined_df$Stimulus.word_POS == 'мир_NOUN', ]
View(df_real)
world$Gender=as.factor(world$Gender)
summary(df_real$Gender)
world = world[, -c(1:4)]
world1=PCA(world, quanti.sup = c(47, 48, 50:68), quali.sup = 49, graph=T)
head(world1$eig)
fviz_pca_var(want1)
plot(world1, choix="ind")
dd=dimdesc(world1, axes = 1:3, proba = 0.05)
dd$Dim.1$quanti
res = catdes(world, num.var=49,  proba=0.1)
res$test.chi2
res$category
res$quanti # for correlations between Dims and all quan var 
res.hcpc = HCPC(world1, proba=0.05, min=2)
res.hcpc$desc.var$quanti
res.hcpc$desc.var$test.chi2
res.hcpc$desc.var$quanti.var # Eta2

# mixed-effect model
data=joined_df[, c(1, 3, 51, 53, 65, 66)]
head(data)
data$AU=as.factor(data$AU)
data$Gender=as.factor(data$Gender)
data$Stimulus.word_POS=as.factor(data$Stimulus.word_POS)

summary(data$Cognitive_pauses)

data$Cognitive_pauses = as.numeric(data$Cognitive_pauses)
gf = goodfit(data$Cognitive_pauses,type= "poisson", method= "ML")
summary(gf)
m0.qp = glmmPQL(Cognitive_pauses ~ 1, random = ~ 1 | Stimulus.word_POS, data =  data, 
                family = quasipoisson(link='log'))
m1.qp <- update(m0.qp, .~.+ AU)
Anova(m1.qp, test = "Chi") 
summary(m1.qp)
# base-line mixed-model
# generate models
m0.glm <- glm(log(Cognitive_pauses) ~ 1, family =  gaussian, data = data) 

m0.lmer = lmer(log(Cognitive_pauses) ~ 1 + (1|AU), REML = T, data = data)
m1.lmer = lmer(log(Cognitive_pauses) ~ 1 + (1|Stimulus.word_POS), REML = T, data = data)
mg.lmer = lmer(log(Cognitive_pauses) ~ 1 + (1|AU) + Gender, REML = T, data = data)
mp.lmer = lmer(log(Cognitive_pauses) ~ 1 + (1|AU) + PositiveIntegral, REML = T, data = data)
AIC(logLik(m0.glm))
AIC(logLik(m0.lmer))
AIC(logLik(m1.lmer))
AIC(logLik(mg.lmer))
AIC(logLik(mp.lmer))
anova(m0.glm, m0.lmer, test = "Chisq")
anova(m0.lmer, m1.lmer, test = "Chisq")
summary(mg.lmer)

gf = goodfit(data$Cognitive_pauses)
summary(gf)
ggplot(data, aes(Stimulus.word_POS, Cognitive_pauses)) +
  geom_point() +
  facet_wrap(~ AU, nrow = 10) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date of composition", y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 6))
 
summary(m0.lmer)
plot(m0.lmer, AU ~ resid(.), abline = 0 )
plot(m0.lmer, resid(., type = "pearson") ~ fitted(.) | AU, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

leveneTest(data$Cognitive_pauses, data$AU, center = mean)

data %>%
  ggplot(aes(x = Gender, y = Cognitive_pauses, fill = Gender)) +
  geom_boxplot()  
  
   
# correlation matrix
library(corrplot)
data_df= joined_df[,-c(1:4)]
data_df$Gender=as.numeric(data_df$Gender)
M = cor(data_df)
str(M)
write.xlsx(as.data.frame(M), "corr.xlsx", rowNames=T, colnames=T)
head(testRes)
corrplot(M, method = 'number')
corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
corrplot(M, order = 'hclust', addrect = 2)
testRes = cor.mtest(data_df, conf.level = 0.95)
corrplot(M, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 2)
corrplot(M, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10), number.cex = 0.3, tl.cex = 0.3)


save_correlation_matrix(data_df, "correlation-matrix.csv")
# descriptive statistics
p3 <- ggplot(data, aes(x = reorder(Stimulus.word_POS, -Long_pauses), y = Long_pauses)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")
p4 <- ggplot(data, aes(Cognitive_pauses)) +
  geom_histogram() +
  theme_bw() + 
  labs(y = "Count", x = "Frequency (Cognitive_pauses)")
p3
View(data)
kruskal.test(Cognitive_pauses ~ Stimulus.word_POS, data = data)
res.fried <- friedman_test(data, Cognitive_pauses ~ Stimulus.word_POS|AU)
res.fried
?friedman_test
p3 <- ggplot(data, aes(x = reorder(AU, -Cognitive_pauses), y = Cognitive_pauses)) +
  geom_boxplot() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(x = "Genre", y = "Frequency\n(Prepositions)")


