install.packages("readxl")
library(readxl)
install.packages("pillar")
library(pillar)
install.packages("writexl")
library(writexl)
install.packages("stringr)
library(stringr)
install.packages("knitr")


kpop<-read_excel("20180430_data_kpop.xlsx",skip=1)
head(kpop)

official<-read.csv("author_list_official.csv")
kpop$공식채널 = ifelse(kpop$Author=='SMTOWN'|kpop$Author=='1theK (원더케이)'|
                     kpop$Author=='jypentertainment'|kpop$Author=='starshipTV'|
                     kpop$Author=='BIGBANG'|
                     kpop$Author== '2NE1'| kpop$Author=='RealVIXX'|                                                 
                    kpop$Author=='United CUBE (CUBE Entertainment Official YouTube Channel)'|
                     kpop$Author=='ibighit'  |                                               
                    kpop$Author=='ASTRO 아스트로 '|                                          
                     kpop$Author==' 남상미  '|                                                 
                   kpop$Author==' 2am  ' |                                                  
                     kpop$Author=='OfficialEpikHigh' |                                       
                    kpop$Author=='4Minute 포미닛(Official YouTube Channel)' |                
                     kpop$Author=='2pm ' |                                                    
                    kpop$Author=='Crayon Pop' |                                              
                     kpop$Author==' gugudan '|                                                 
                    kpop$Author=='officialpsy' |                                             
                     kpop$Author=='Areia Creations '|                                         
                   kpop$Author==' BLACKPINK' |                                               
                     kpop$Author=='WINNER '  |                                                
                    kpop$Author=='SEVENTEEN'|                                                
                     kpop$Author==' YG ENTERTAINMENT'|                                         
                   kpop$Author==' VICTON 빅톤 '|                                             
                     kpop$Author==' wondergirls'|                                              
                   kpop$Author==' B1A4 OFFICIAL + '  |                                       
                     kpop$Author==' AOA (에이오에이)'|                                         
                    kpop$Author=='OFFICIAL LABOUM' |                                         
                     kpop$Author==' EXID_OFFICIAL'   |                                         
                   kpop$Author==' SF9'  |                                                    
                     kpop$Author==' Apink (에이핑크) ' |                                       
                  kpop$Author==' HUNUS Entertainment '   |                                  
                     kpop$Author==' GOT7'  |                                                   
                  kpop$Author=='  U-KISS'  |                                                 
                     kpop$Author=='BTOB 비투비 (Official YouTube Channel)'  |                 
                  kpop$Author==' KANGELMIYOUN '  |                                          
                     kpop$Author==' MAMAMOO ' |                                                
                   kpop$Author==' CNBLUE (씨엔블루)'   |                                     
                     kpop$Author==' OfficialDaraOnline'  |                                     
                   kpop$Author==' Lukas - 루카스'  |                                         
                     kpop$Author==" RAIN's Official Channel"|                                 
                     kpop$Author==' DAY6'   |                                                  
                   kpop$Author==' BEAST 비스트 (Official YouTube Channel)'  |                
                     kpop$Author==' 철이' |                                                    
                   kpop$Author==' YG Family Tube'  |                                         
                     kpop$Author==' 에릭남 매력 TV' |                                          
                 kpop$Author=='박지윤 크리에이티브'   |                                   
                     kpop$Author==' Warner Music Japan'  |                                     
                  kpop$Author==' 2AMClubVEVO'   |                                           
                     kpop$Author==' iKON'   |                                                  
                  kpop$Author==' DARA TV ' |                                                
                     kpop$Author=='  워너뮤직코리아 (Warner Music Korea)' |                     
                  kpop$Author=='  CNBLUEofficialjp' |                                        
                     kpop$Author==' DisneyMusicVEVO' |                            
                  kpop$Author==' Happyface entertainment'   ,1 ,0)   

list<-read_excel("20180430_data_kpop.xlsx",sheet = 2)
list1=list[,c(1,3)]




media<-read.csv("author_list_media.csv")
media[,1]

apply(kpop$media,2,ifelse(kpop$Author %in% c(ARIRANG K-POP ,            
                          MBCkpop,                   
                          CJENMMUSIC Official,       
                          ALLIPevent,                
                          KBS CoolFM ,               
                          KBS KONG ,                 
                          KBS World TV ,             
                          ARIRANG TV                
                        ,  MBCentertainment          
                       ,   ARIRANG RADIO             
                       ,   SBSNOW                    
                       ,   SBS Radio100              
                       ,   MTV 我愛偶像 Idols of Asia
                      ,    JTBC Entertainment        
                      ,    Mnet Official             
                       ,   KOCOWA TV                 
                      ,    BANGTANTV                 
                       ,   티캐스트 tcast            
                       ,   StarSeoulTV               
                       ,   EBSCulture (EBS 교양)     
                       ,   MBCentertain              
                       ,   tvN                       
                      ,    KBS 한국방송 (MyloveKBS)  
                       ,   KMREACTS),1,0))
kpop$media= ifelse(kpop$Author %in% c('ARIRANG K-POP' ,            
                         ' MBCkpop',                   
                          'CJENMMUSIC Official',       
                          'ALLIPevent',                
                          'KBS CoolFM ',               
                          'KBS KONG ',                 
                          'KBS World TV' ,             
                          'ARIRANG TV  '              
                          ,  'MBCentertainment '         
                          ,  ' ARIRANG RADIO   '          
                          ,  'SBSNOW  '                  
                          ,  ' SBS Radio100  '            
                          ,  ' MTV 我愛偶像 Idols of Asia'
                          ,   ' JTBC Entertainment '       
                          ,   ' Mnet Official '            
                          ,   'KOCOWA TV '                
                          ,  '  BANGTANTV  '               
                          ,  ' 티캐스트 tcast  '          
                          ,  ' StarSeoulTV'               
                          ,  ' EBSCulture (EBS 교양) '    
                          ,  ' MBCentertain  '            
                          ,  ' tvN '                      
                          ,  ' KBS 한국방송 (MyloveKBS) ' 
                          ,  ' KMREACTS'),1,0)
kpop$UGC=ifelse(kpop$공식채널==1|kpop$media ==1,0,1)
title = as.data.frame(str_split(kpop$Title," "))
title = sapply(kpop,str_split(kpop$Title," "))
kpop$cover=ifelse('cover' %in% c(str_split(kpop$Title," "),str_split(kpop$Description," "),str_split(kpop$Tags," ")),1,0)
substr("Statistics", 7, 10)
str_split(kpop$Title," ")

kpop$cover=ifelse(kpop$Title %in% kpop$Title[grep('cover', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Cover', kpop$Title)]|kpop$Title %in% kpop$Title[grep('COVER', kpop$Title)],1,0)
kpop$cover=ifelse(kpop$Description %in% kpop$Description[grep('cover', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Cover', kpop$Description)]|kpop$Description %in% kpop$Description[grep('COVER', kpop$Description)],1,0)
kpop$cover=ifelse(kpop$Tags %in% kpop$Tags[grep('cover', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Cover', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('COVER', kpop$Tags)],1,0)

kpop$reaction=ifelse(kpop$Title %in% kpop$Title[grep('reaction', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Reaction', kpop$Title)]|kpop$Title %in% kpop$Title[grep('REACTION', kpop$Title)],1,0)
kpop$reaction=ifelse(kpop$Description %in% kpop$Description[grep('reaction', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Reaction', kpop$Description)]|kpop$Description %in% kpop$Description[grep('REACTION', kpop$Description)],1,0)
kpop$reaction=ifelse(kpop$Tags %in% kpop$Tags[grep('reaction', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Reaction', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('REACTION', kpop$Tags)],1,0)

kpop$fancam=ifelse(kpop$Title %in% kpop$Title[grep('fancam', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Fancam', kpop$Title)]|kpop$Title %in% kpop$Title[grep('FANCAM', kpop$Title)],1,0)
kpop$fancam=ifelse(kpop$Description %in% kpop$Description[grep('fancam', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Fancam', kpop$Description)]|kpop$Description %in% kpop$Description[grep('FANCAM', kpop$Description)],1,0)
kpop$fancam=ifelse(kpop$Tags %in% kpop$Tags[grep('fancam', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Fancam', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('FANCAM', kpop$Tags)],1,0)

kpop$flashmob=ifelse(kpop$Title %in% kpop$Title[grep('flashmob', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Flashmob', kpop$Title)]|kpop$Title %in% kpop$Title[grep('FLASHMOB', kpop$Title)],1,0)
kpop$flashmob=ifelse(kpop$Description %in% kpop$Description[grep('flashmob', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Flashmob', kpop$Description)]|kpop$Description %in% kpop$Description[grep('FLASHMOB', kpop$Description)],1,0)
kpop$flashmob=ifelse(kpop$Tags %in% kpop$Tags[grep('flashmob', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Flashmob', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('FLASHMOB', kpop$Tags)],1,0)

kpop$playlist=ifelse(kpop$Title %in% kpop$Title[grep('playlist', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Playlist', kpop$Title)],1,0)
kpop$playlist=ifelse(kpop$Description %in% kpop$Description[grep('playlist', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Playlist', kpop$Description)],1,0)
kpop$playlist=ifelse(kpop$Tags %in% kpop$Tags[grep('playlist', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Playlist', kpop$Tags)],1,0)

table(kpop$playlist)

kpop$chart=ifelse(kpop$Title %in% kpop$Title[grep('chart', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Chart', kpop$Title)]|kpop$Title %in% kpop$Title[grep('CHART', kpop$Title)],1,0)
kpop$chart=ifelse(kpop$Description %in% kpop$Description[grep('chart', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Chart', kpop$Description)]|kpop$Description %in% kpop$Description[grep('CHART', kpop$Description)],1,0)
kpop$chart=ifelse(kpop$Tags %in% kpop$Tags[grep('chart', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Chart', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('CHART', kpop$Tags)],1,0)

table(kpop$chart)

kpop$lyrics=ifelse(kpop$Title %in% kpop$Title[grep('lyrics', kpop$Title)]|kpop$Title %in% kpop$Title[grep('Lyrics', kpop$Title)]|kpop$Title %in% kpop$Title[grep('LYRICS', kpop$Title)],1,0)
kpop$lyrics=ifelse(kpop$Description %in% kpop$Description[grep('lyrics', kpop$Description)]|kpop$Description %in% kpop$Description[grep('Lyrics', kpop$Description)]|kpop$Description %in% kpop$Description[grep('LYRICS', kpop$Description)],1,0)
kpop$lyrics=ifelse(kpop$Tags %in% kpop$Tags[grep('lyrics', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('Lyrics', kpop$Tags)]|kpop$Tags %in% kpop$Tags[grep('LYRICS', kpop$Tags)],1,0)

table(kpop$lyrics)
                 




head(kpop$Tags[grep('cover', kpop$Tags)])


writexl::write_xlsx(kpop,"20180503_data_kpop.xlsx")

# year update > word cloud > 