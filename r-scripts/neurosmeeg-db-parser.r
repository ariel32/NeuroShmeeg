library(RSQLite)
library(dplyr)
library(magrittr)

# init constants
# db.path <- '~/Downloads/downloads-on-dl/database(1).sqlite'
db.path <- 'D:/Work/Programming/NeuroShmeeg/r-scripts/database(1).sqlite'

# ops names
{
ops.name <- c('Rx1',
              'Rx2',
              'Rx3',
              'Rx -',
              'ИМТ/ТАА',
              'RxL',
              '00: Постановка в очередь',
              '01: Обращение к провизору',
              '02: Фармэкспертиза рецепта',
              '03: Док-т для Л',
              '04: Информация о применении',
              '05: Поиск в БД',
              '06: Поиск на МХ',
              '07: Таксировка',
              '08: Оформление отрывных корешков',
              '09: Карта ЛО',
              '10: Демонстрация МИ',
              '11: Инф. об эксп. МИ',
              '12: Гарантийный талон',
              '13: Упаковка-Маркировка-Срок годности',
              '14: Копия инструкции',
              '15: Копия рецепта',
              '16: Не проходящий товар',
              '17: Скидка',
              '18a: Расчеты (наличные)',#'18: Расчеты',
              '18b: Расчеты (безналичные)',
              '19: Обратная сторона',
              '20: Рецептурный журнал',
              'Окончание обслуживания',
              'Сброс',
              'Пауза',
              'Старт',
              'а. Подготовительные работы',
              'б. Предоставление информации по телефону',
              'в. Приемка товаров, приемочный контроль',
              'г. Пополнение отделов аптеки ЛС, МИ, ТАА',
              'д. Оформление витрин аптеки',
              'е. Контроль сроков годности и реализации',
              'ж. Ведение учета и отчетности',
              'з. Рассмотрение замечаний и предложений',
              'и. Сбор информации о побочных реакциях на ЛС',
              'к. Санитарно-просветительная работа',
              'л. Контроль температуры и влажности',
              'м. Информирование мед. представителями',
              'н. Регламентированный перерыв',
              'о-1. Перерыв из-за отсутствия посетителей',
              'о-2. Другие нерегламентированные перерывы',
              'п. Заключительные работы',
              'Добавить комментарий',
              'Добавить объект')

names(ops.name) <- c('ButtonRx',
                      'ButtonRx2',
                      'ButtonRx3',
                      'ButtonNotRx',
                      'ButtonIMT',
                      'ButtonRxL',
                      'b00',
                      'b01',
                      'b02',
                      'b03',
                      'b04',
                      'b05',
                      'b06',
                      'b07',
                      'b08',
                      'b09',
                      'b10',
                      'b11',
                      'b12',
                      'b13',
                      'b14',
                      'b15',
                      'b16',
                      'b17',
                      'b18a',#'b18',
                      'b18b',
                      'b19',
                      'b20',
                      'END', #bExit',
                      'ButtonTimeShiftNow',
                      'ButtonTimeShiftPause',
                      'ButtonTimeShiftStart',
                      'bops01',
                      'bops02',
                      'bops03',
                      'bops04',
                      'bops05',
                      'bops06',
                      'bops07',
                      'bops08',
                      'bops09',
                      'bops10',
                      'bops11',
                      'bops12',
                      'bops13',
                      'bops14',
                      'bops15',
                      'bops16',
                      'ButtonAddComment',
                      'ButtonAdd')
}

# mode names
{
modes <- c('Rx1',
           'Rx2',
           'Rx3',
           'RxL',           
           'Rx -',
           'ИМТ/ТАА')

names(modes) <- c('ButtonRx',
                  'ButtonRx2',
                  'ButtonRx3',
                  'ButtonRxL',
                  'ButtonNotRx',
                  'ButtonIMT')
}

# read tables
con <- dbConnect(SQLite(),
                 db.path)

if(exists("ch.data")){
  rm(ch.data)
}

tryCatch(
  ch.data <- fetch(dbSendQuery(con,
                            'SELECT os.sessid AS sessid,
                                    CAST(os.inqnum AS REAL) AS inqnum,
                                    os.tos AS tos,
                                    os.stdabbrev AS stdabbrev,
                                    os.visitorCategory AS visitorCategory,
                                    CAST(os.timestamp AS REAL) AS timestamp,
                                    prv.*
                            FROM observations AS os
                            LEFT JOIN provisors AS prv ON os.CSNumber = prv.CSNumber 
                                      AND os.provisorTNumber = prv.provisorTNumber')),
  error = function(e){print(toupper("невозможно выполнить запрос к БД! проверьте путь к файлу"))},
  finally = dbDisconnect(con)
)

# mangling data
# convert tos codes to modes
ch.data %<>%
  mutate(mode.txt = apply(matrix(intToBits(tos),
                                  ncol=32,
                                  byrow = T)[,1:7],
                          1,
                          function(x){paste0(modes[as.logical(x)],
                                             collapse = '; ')}),
         sess.in = as.POSIXct(as.POSIXlt(sessid,
                                         origin="1970-01-01",
                                         tz = "GMT")),
         inqnum.in = as.POSIXct(as.POSIXlt(inqnum/1000,
                                           origin="1970-01-01",
                                           tz = "GMT")),
         timestamp.in = as.POSIXct(as.POSIXlt(timestamp/1000,
                                              origin="1970-01-01",
                                              tz = "GMT")),
         op.txt = ops.name[stdabbrev]) %>% 
  group_by(inqnum) %>% 
  mutate(duration = (lead(timestamp,1)-timestamp)/1000) %>% 
  ungroup()

Encoding(ch.data$orgName) <- "UTF-8"
Encoding(ch.data$CSNumber) <- "UTF-8"
Encoding(ch.data$CSCategory) <- "UTF-8"
Encoding(ch.data$CSDescription) <- "UTF-8"
Encoding(ch.data$provisorName) <- "UTF-8"
Encoding(ch.data$provisorSex) <- "UTF-8"
Encoding(ch.data$provisorEducation) <- "UTF-8"
Encoding(ch.data$provisorCategory) <- "UTF-8"
Encoding(ch.data$provisorDescription) <- "UTF-8"

ch.data %>% 
  select(ДатаВремяСессии          = sessid,
          КатегорияПосетителя     = visitorCategory,
          ВидыОбслуживания        = mode.txt,
          ЭтапОбслуживания        = op.txt,
          ДатаВремяПосещения      = inqnum.in,
          ДатаВремяОперации       = timestamp.in,
          АптекаОрганизация       = orgName,
          АптекаНомер             = CSNumber,
          АптекаКатегоря          = CSCategory,
          АптекаАдрес             = CSAddres,
          АптекаОписание          = CSDescription,
          ПровизорИмя             = provisorName,
          ПровизорТабельныйНомер  = provisorTNumber,
          ПровизорПол             = provisorSex,
          ПровизорВозраст         = provisorAge,
          ПровизорОбразование     = provisorEducation,
          ПровизорКатегоря        = provisorCategory,
          ПровизорОписание        = provisorDescription,
          ПровизорНепрерывныйСтаж = provisorExpCont,
          ПровизорОбщийСтаж       = provisorExpGeneral,
          Длительность            = duration) %>% 
  write.csv(file = paste(strftime(Sys.time(),'%Y-%m-%d-%H-%M'),
                          'nsh',
                          'base.csv',
                          sep = '-'),
            row.names = FALSE,
            fileEncoding = 'UTF-8')
