library(shiny)
library(plotrix)
library(plotly)
library(hrbrthemes)
library(ggplot2)
xValue <- c(1995,1996,1997,1998,2002,2003,2004,2011 ,2012 ,2013,2014,2015,2016,2017,2018,2019)
yValue <- c(13.6,8.86,9.9,14.57,16.48,17.3 ,17.97 ,18.02,17.48,16.5,18.3 ,18.52,17.59,18.89,18.56,18.29)
xtabn2 <-
    c(34.04,25.34,27.45,18.73,25.58,28.98,16.48,24.66,13.76,14.59,22.48,22.25,20.33,25.61,18.29,17.34,24.67,14.80,22.71,24.07)
ytabn2 <- c('Danimarka','Mbreteri e Bashkuar','Suedi','Sllovaki','Austri','Zelanda e
Re','Turqi','Itali','Spanje','Rumani','Hungari','Kroaci','Bullgari','Greqi','Shqiperi'
            ,'Poloni','France','Ceki','Belgjike','Serbi')
tabela4h <- read.table(
    header=TRUE, text=' "Lloji i takses" Viti teArdhura
1 "Taksat e energjise" 2012 24578
2 "Taksat e transportit" 2012 10290
3 "Taksat e ndotjes" 2012 1878
4 "Taksat e burimeve natyrore" 2012 9
5 "Taksat e energjise" 2013 25057
6 "Taksat e transportit" 2013 10461
7 "Taksat e ndotjes" 2013 1853
8 "Taksat e burimeve natyrore" 2013 11
9 "Taksat e energjise" 2014 27312
10 "Taksat e transportit" 2014 14892
11 "Taksat e ndotjes" 2014 1889
12 "Taksat e burimeve natyrore" 2014 14
13 "Taksat e energjise" 2015 22924
14 "Taksat e transportit" 2015 19255
15 "Taksat e ndotjes" 2015 1782
16 "Taksat e burimeve natyrore" 2015 19
17 "Taksat e energjise" 2016 24539
18 "Taksat e transportit" 2016 21342
19 "Taksat e ndotjes" 2016 1880
20 "Taksat e burimeve natyrore" 2016 16
')
Shteteth<-c("Kosova","Shqiperi","Maqedoni","Bosnje","Serbi","Mali i Zi")
TatimeDirekteh<-c(3,6,4,3,5,4)
TatimeIndirekth<-c(19,10,11,13,16,18)
TaksaSocialeh<-c(1,6,8,14,13,11)
datah<-data.frame(Shteteth,TatimeDirekteh,TatimeIndirekth,TaksaSocialeh)
figh <- plot_ly(datah, x = Shteteth, y = TatimeDirekteh, type = 'bar', name =
                    'Tatime Direkte')
figh <- figh %>% add_trace(y = TatimeIndirekth, name = 'Tatime Indirekte')
figh <- figh %>% add_trace(y = TaksaSocialeh, name = 'Taksa Sociale')
data<-proveRe
slices <- c(278984, 21975, 97699, 18953)
lbls <- c("Tatime dhe dogana", "Pushteti lokal", "Fonde speciale", "Te ardhura jo tatimore")
xRe=c(9076,15656,28769,29794,38121,41149,46113,50625,58161,64534,74268,87771,107094,110062,113998,119189,116533,111940,123730,125783,131390,139541,143464,132412,130354)
yRe=c(673,814,1167,3138,4590,6300,6149,6414,6852,7402,8580,14850,24498,26820,27058,27967,27989,29570,21479,29661,31412,32102,36517,46124,33658)
sh <- c("Kroaci", "Bullgari",
        "Greqi","Rumani","Bosnje","Serbi","Shqiperi","Kosove","Maqedoni","Mali i Zi")
p<- c(546,311,758,466,207,343,213,174,282,331)
tvsh <- c(136.50,62.20,174.34,88.54,35.19,68.60,42.60,27.84,50.76,69.51)
data2 <- data.frame(sh, p, tvsh)
fig <- plot_ly(data2, x = sh, y = ~p, type = 'bar', name = 'Paga minimale, euro/muaj',marker
               = list(color = ' #1E7DB6'))
fig <- fig %>% add_trace(y = ~tvsh, name = 'Sasia qe shkon per tvsh',marker = list(color =
                                                                                       '#34A6EC'))
fig <- fig %>% layout(yaxis = list(title = 'Euro'), barmode = 'stack')
meviola<-read.table(
    header=TRUE, text='"Lloji i tatimit" viti vlera
1 "nga tatimet dhe doganat" 2010 223019
2 "nga pushteti lokal" 2010 11898
3 "nga fondet speciale" 2010 53647
4 "nga tatimet dhe doganat" 2011 235509
5 "nga pushteti lokal" 2011 11791
6 "nga fondet speciale" 2011 56627
7 "nga tatimet dhe doganat" 2012 232591
8 "nga pushteti lokal" 2012 10859
9 "nga fondet speciale" 2012 57411
10 "nga tatimet dhe doganat" 2013 229031
11 "nga pushteti lokal" 2013 10825
12 "nga fondet speciale" 2013 60033
13 "nga tatimet dhe doganat" 2014 253413
14 "nga pushteti lokal" 2014 12447
15 "nga fondet speciale" 2014 70008
16 "nga tatimet dhe doganat" 2015 258882
17 "nga pushteti lokal" 2015 11700
18 "nga fondet speciale" 2015 71726
19 "nga tatimet dhe doganat" 2016 275780
20 "nga pushteti lokal" 2016 14951
21 "nga fondet speciale" 2016 79153
22 "nga tatimet dhe doganat" 2017 293386
23 "nga pushteti lokal" 2017 18447
24 "nga fondet speciale" 2017 86795
25 "nga tatimet dhe doganat" 2018 304318
26 "nga pushteti lokal" 2018 21863
27 "nga fondet speciale" 2018 93153
28 "nga tatimet dhe doganat" 2019 304758
29 "nga pushteti lokal" 2019 23102
30 "nga fondet speciale" 2019 98411
31 "nga tatimet dhe doganat" 2020 278984
32 "nga pushteti lokal" 2020 21975
33 "nga fondet speciale" 2020 97699
')
df<-data.frame(x=factor(c("0-30000","30001-150000","150001")),y=c(0,13,23))
a<-
    c("04/20","05/20","06/20","07/20","08/20","09/20","10/20","11/20","12/20","01/21","02/21
","03/21")
b<-
    c(790048,969596,1226268,1502242,1822135,2065726,2321951,2585722,2962570,240518,
      479887,749988)
xn=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
yn=c(673,814,1167,3138,4590,6300,6149,6414,6852,7402,8580,14850,24498,26820,27058,27967,27989,29570,21479,29661,31412,32102,36517,46124)
ui<-fluidPage(
    p(div(HTML("<h1>Sistemi i taksave ne Shqiperi</h1>"))),
    p(div(HTML("<h4>Punoi: Sara Hoxhaj, Melisa Kaca, Meviola Karriqi</h4>"))),
    p(div(HTML("<br>"))),
    p(div(HTML("<em><h5><strong>Takse</strong></h5></em>"))),
    p("Detyrim, zakonisht ne para, qe, sipas ligjeve te caktuara, shteti e merr nga shtetasit e
vet ose me ane te doganes per te plotesuar buxhetin. Detyrim financiar qe personat fizike
ose personat juridike jane te detyruar ti paguajne shtetit. Personat qe detyrohen te pauajne
taksa quhen ndryshe dhe taksapagues.Ne nje kuptim me te gjere, taksa quhen edhe
detyrimet qe individet ose entitetet e ndryshme i paguajne formave organizative ekuivalente
me shtetin (siÃ§ mund te jene tribuja, levizjet separatiste apo revolucionare, etj.). "),
    p(div(HTML("<br>"))),
    p(div(HTML("<em><h5><strong>Tatim</strong></h5></em>"))),
    p("Takse qe i paguhet shtetit nga shtetasit, nga sipermarrjet etj. per te ardhurat ose per
fitimet e tyre. Detyrim shteteror te cilit i neshtrohen personat dhe mallrat sipas
perkufizimeve ne ligjet administrative te shtetit.Keto detyrime perdoren nga shteti me qellim
te institucionalizimit te administrates. Per te arritur mbledhjen e ketyre detyrimeve
parashihen procedurat tatimore sipas akteve administrative te shtetit "),
    p(div(HTML("<br>"))),
    tabsetPanel(
        tabPanel("Tabela 1.1 & 1.2",type = "tabs",
                 p(div(HTML("<strong>Te ardhurat nga taksat, perqindja e GDP (1995-2019) </
strong>"))),
                 tabPanel("Tabela 1.1", tableOutput("static")),
                 tabPanel("Grafiku 1.1",plotOutput("grafiku")),
                 p(div(HTML("<strong>Krahasim i te ardhurave nga taksat, perqindja e GDP
(2019)</strong>"))),
                 tabPanel("Tabela 1.2", tableOutput("staticu")),
                 tabPanel("Grafiku 1.2",plotlyOutput("grafikuu")),
                 tabPanel("Komente", textOutput("komente1"))
        ),
        tabPanel("Tabela 2",
                 type = "tabs",
                 p(div(HTML("<strong>Te ardhurat nga taksat e mjedisit dhe llojet e saj </strong>"))),
                 tabPanel("Tabela 2", tableOutput("tab2")),
                 tabPanel("Grafiku 2",plotOutput("graf2")),
                 tabPanel("Komente", textOutput("komente2"))
        ),
        tabPanel("Tabela 3",
                 type = "tabs",
                 p(div(HTML("<strong>Sistemi i taksave ne Ballkan ne vitin 2019 </strong>"))),
                 tabPanel("Tabela 3", tableOutput("tab3")),
                 tabPanel("Grafiku 3",plotlyOutput("graf3")),
                 tabPanel("Komente", textOutput("komente3"))
        ),
        tabPanel("Tabela 4",
                 type = "tabs",
                 tabPanel("Tabela 4", tableOutput("tab4")),
                 tabPanel("Grafiku 4",plotOutput("graf4")),
                 tabPanel("Komente", textOutput("komente4"))
        ),
        tabPanel("Tabela 5",
                 p(div(HTML("<strong>Te ardhurat tatimore ne vitin 2020</strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 5", tableOutput("tab5")),
                 tabPanel("Grafiku 5",plotOutput("graf5")),
                 tabPanel("Komente", textOutput("komente5"))
        ),
        tabPanel("Tabela 6",
                 p(div(HTML("<strong>Te ardhurat nga tatimi mbi te ardhurat personale</
strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 6", tableOutput("tab6")),
                 tabPanel("Grafiku 6",plotOutput("graf6")),
                 tabPanel("Komente", textOutput("komente6"))
        ),
        tabPanel("Tabela 7",
                 p(div(HTML("<strong>Krahasim i pages minimale dhe TVSH ne vendet e Ballkanit
</strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 7", tableOutput("tab7")),
                 tabPanel("Grafiku 7",plotlyOutput("graf7")),
                 tabPanel("Komente", textOutput("komente7"))
        ),
        tabPanel("Tabela 8",
                 p(div(HTML("<strong>Te ardhurat e shtetit nga tatimet ne periudhen 2010-2020
</strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 8", tableOutput("tab8")),
                 tabPanel("Grafiku 8",plotOutput("graf8")),
                 tabPanel("Komente", textOutput("komente8"))
        ),
        tabPanel("Tabela 9",
                 p(div(HTML("<strong>Vlera e tatimit mbi te ardhurat personale ne Shqiperi </
strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 9", tableOutput("tab9")),
                 tabPanel("Grafiku 9",plotOutput("graf9")),
                 tabPanel("Komente", textOutput("komente9"))
        ),
        tabPanel("Tabela 10",
                 p(div(HTML("<strong>Niveli i tatimit mbi te ardhurat personale </strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 10", tableOutput("tab10")),
                 tabPanel("Grafiku 10",plotlyOutput("graf10")),
                 tabPanel("Komente", textOutput("komente10"))
        ),
        tabPanel("Tabela 11",
                 p(div(HTML("<strong>Te ardhurat e Shqiperise nga taksat </strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 11", tableOutput("tab11")),
                 tabPanel("Grafiku 11",plotOutput("graf11")),
                 tabPanel("Komente", textOutput("komente11"))
        ),
        tabPanel("Tabela 12",
                 p(div(HTML("<strong>Te ardhurat e shtetit nga tatimi mbi te ardhurat personale
</strong>"))),
                 type = "tabs",
                 tabPanel("Tabela 12", tableOutput("tab12")),
                 tabPanel("Grafiku 12",plotOutput("graf12"))
        ),
        tabPanel("Fund",
                 p(div(HTML("<strong><em>Perfundime</em></strong>"))),
                 p(div(HTML("Nga studimi i sistemit te taksave ne Shqiperi, ndryshimit te yre nder
vite dhe ndikimi ne zhvillimin ekonomik te vendit, dalim ne disa perfundime te cilat
mendojme se e bejne sado pak me te qarte problematiken e taksave dhe vleres se tyre ne
vendin tone. "))),
                 p(div(HTML("<ul>
<li>Rritja e taksave me 1% ul GDP me 2-3%. => Vleresim dhe vemendje ndaj
GDP. Ulja e GDP dhe rritja e taksave, ul te ardhurat e qytetareve, ul konsumin dhe shkurton
vendet e punes. </li>
<li>Kujdes ne percaktimin e vlerave te taksave te mjedisit. Taksat mjedisore i
kushtojne shqiptareve me shume se cdo qytetari europian. Te vetmet qe i afrohen niveleve
te vendit tone jane shtetet e Europes Lindore qe kane nje cmim me te larte te karburanteve.
Duke u krahasuar me BE, shpenzimet tona per mjedisin zene 10 here me pak peshe. </li>
<li>Shqiperia ka vleren me te larte ne perqindje te tatimeve direkte ne krahasim
me vendet e Ballkanit. Tatimet direkte perfshijne : tatimi mbi te ardhurat, tatmimi i
korporatave, tatimi i prones, mbi trashegimine etj. </li>
<li>Vlera mesatare e tatimit mbi te ardhurat ne Shqiperi eshte afersisht sa dyfishi
i atyre ne Kosove, Serbi, Maqedoni e Rumani, vende te cilat kane vlere me te larte te pages
minimale. </li>
</ul>"))),
                 p(div(HTML("<strong><em>Referenca</em></strong>"))),
                 p(div(HTML("<a>http://www.instat.gov.al/media/4531/njoftim-per-media-taksate-
mjedisit-2016.pdf</a>"))),
                 p(div(HTML("<a>https://www.financa.gov.al/</a>"))),
                 p(div(HTML("<a>https://www.altax.al/en/blog/674-tax-burden-its-structuringand-
tax-rates-in-the-western-balkans-in-2018-2019</a>"))),
                 p(div(HTML("<a>https://www.theglobaleconomy.com/Albania/Tax_revenue/</
a>"))),
                 p(div(HTML("<a>https://www.financa.gov.al/wp-content/uploads/2017/09/
Analiza_e_te_ardhurave_ne_20_vjet_1992-2012rregulluar.pdf</a>"))),
                 p(div(HTML("<a>https://www.tatime.gov.al/d/8/45/0/847/ndryshimet-kryesore-tepaketes-
fiskale-2019</a>"))),
                 p(div(HTML("<a>https://www.ceicdata.com/en/indicator/albania/tax-revenue</
a>"))),
                 p(div(HTML("<a>https://www.bankofalbania.org/rc/doc/
Analize_krahasuese_e_sistemeve_te_taksave_ne_disa_vende_te_rajonit_1789_1_6318.pdf<
/a>"))),
                 p(div(HTML("<a>https://taxsummaries.pwc.com/albania/individual/taxes-onpersonal-
income</a>"))),
                 type = "tabs"
        )
    )
)
server<-function(input,output,session){
    output$static <- renderTable(tabela1_1)
    output$grafiku <- renderPlot(
        ggplot(tabela1_1, aes(x=xValue, y=yValue,xlab="Viti",ylab="Te ardhurat ne %"))+
            geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2)+
            theme_ipsum() +
            xlab("Viti") +
            ylab("Te ardhurat ne %") +
            ggtitle("Te ardhurat nga taksat, perqindja e GDP")
    )
    output$staticu <- renderTable(tabela1_2)
    output$grafikuu <- renderPlotly(
        plot_ly(x =xtabn2, y = ytabn2, type = 'bar', orientation = 'h')
    )
    output$komente1 <-renderText(
        "Nga tabela 1.1 dallojme se ne periudhen 1995-2019, vlera mesatare ka qene 16.3%
me nje minimin prej 8,86% ne 1996 dhe maksimum ne 2017 me 18.89% ndersa nga tabela
e dyte dallojme se shteti i cili ka te ardhurat me te larta nga taksat eshte Danimarka me
34.04% ndersa ai me te ardhurat me te uleta eshte Spanja me 13,76%.Midis sistemit te
taksave dhe GDP ekziston nje maredhenie e cila ndikon ne rritjen apo uljen e te ardhurave
te shtetit. Mesatarisht, nje rritje e taksave prej 1%, ul GDP reale me 2 deri ne 3%. Lidhja
midis GDP dhe taksave qendron se raporti i i tyre sherben si mates i te ardhurave nga
taksat e nje kombi ne krahasim me madhesine e ekonomise se tij te matur nga GDP. "
    )
    output$tab2 <- renderTable(tabela4h)
    output$graf2 <- renderPlot(
        ggplot(tabela4h, aes(factor(Viti), teArdhura, fill = Lloji.i.takses)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(" Te ardhurat nga taksat e mjedisit ") +
            scale_fill_brewer(palette = "Set1")
    )
    output$komente2 <-renderText("Ne kete tabele paraqiten te ardhurat e Shqiperise nga
taksat e mjedisit nga viti 2012-2016 ne milion leke. Pas studimit te te dhenave, dalim ne
perfundim se, deri ne vitin 2016, taksat e energjise zene pjesen me te madhe te te
ardhurave, me pas ato te transportit, ndotjes dhe burimeve natyrore. Niveli i te ardhurave
nga taksat e burimeve natyrore eshte gati e paperfillshme per shkak te vleres mjaft te vogel
qe ajo ka nde krahasim me taksat e tjera dhe fitimit qe ato sjellin per buxhetin e shtetit.
Gjithashtu veme re se gjate kesaj periudhe, taksa e transportit eshte rritur me shume se
pjesa tjeter e taksave te mjedisit, me 11052 milion leke nga viti 2012 deri ne 2016 nderkohe
qe taksa e ndotjes pothuajse nuk ka pesuar asnje ndryshim. ")
    output$tab3 <- renderTable(tabela7)
    output$graf3 <- renderPlotly(
        figh <- figh %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    )
    output$komente3<-renderText("Nga grafiku i kesaj tabele, shohim qe ne vendin tone
tatimet direkte perbejne 6% te GDP, tatimet indirekte 10% dhe taksat sociale 6%. Ne
krahasim me vendet e Ballkanit dhe ndikimin qe kane keto taksa ne vleren e GDP, shohim
se tatimet direkte ndikojne me shume ne Shqiperi, tatimet indirekte ne Kosove dhe taksat
sociale ne Bosnje dhe Hercegovine. ")
    output$tab4 <- renderTable(proveRe)
    output$graf4 <- renderPlot(
        data %>%
            ggplot(aes(x=Viti,y=rritjaGDPNePerqindje, size=TeArdhuratNgaTaksatNeMilionLeke)) +
            geom_point(alpha=0.6,color="seagreen3")+
            scale_size_continuous(range = c(1, 7))+
            labs(x="Viti", y="Rritja e GDP ne %", size="Te ardhurat nga taksat(milion leke)")
    )
    output$komente4<-renderText("Nga kjo tabele dhe nga grafiku bubble chart, arrijme te
vezhdojme 3 variabla te ndryshem nga viti 2000 deri ne vitin 2019, te cilet jane : rritja e GDP
ne %, te ardhurat nga taksat ne milion leke dhe totali i shpenzimeve. Ne grafik dallojme se
GDP ka arritur nivelin e saj me te larte me 8.29% ne vitin 2001 dhe nivelin e saj me te ulet
ne vitin 2013 me 1.002%. Gjithashtu shohim se ne vitin 2019 Shqiperia ka pasur nivelin te
me te larte te te ardhurave nga taksat me 426271 milion leke dhe nivelin me te ulet me
104112 milion leke ne vitin 2002. Me keto te dhena, arrijme serish te bejme lidhjen midis
GDP dhe taksave. Nga grafiku dallojme se sa me i larte te jete niveli i GDP, aq me te uleta
jane te ardhurat nga taksat. ")
    output$tab5 <- renderTable(pie)
    output$graf5 <- renderPlot(
        pie3D(slices,labels=lbls,explode = 0.3,radius = 1,cex=5, main="Te ardhura tatimore 2020
",col = c("gold1", "tomato1", "powderblue","palegreen2"))
    )
    output$komente5<-renderText("Ne grafikun pie / rrethor shohim se ne vitin 2020, pjesa
me e madhe e te ardhurave erdhen nga tatimet dhe doganat dhe me pas nga fondet
speciale si psh sigurimet shoqerore apo shendetesore, pushteti lokal dhe te ardhura jo
tatimore. Sipas vleres se tyre, tatimet dhe doganat perben 66%, pushteti lokal 5%, fondet
speciale 23% dhe te ardhurat jo tatimore rreth 4%.")
    output$tab6 <- renderTable(rePikash)
    output$graf6 <- renderPlot(
        plot(xRe, yRe, main = "Te ardhurat ne milion leke",
             xlab = "Tatimi mbi vleren e shtuar", ylab = "Tatimi mbi te ardhurat personale",
             pch = 19, frame = FALSE)
    )
    output$komente6<-renderText("Per keto te dhena, krijimi i nje reje pikash dhe vijes se
regresit nga ndihmojme te kuptojme nese ekziston nje marredhenise midis tatimit mbi te
ardhurat personale dhe tatimit mbi vleren e shtuar (TVSH) dhe se cila do te jete ecuria e tyre
ne te ardhmen. Pas ndertimit te grafikut dhe me ndihmen e drejtezes se regresit, kuptojme
se midis 2 variablave ekziston nje lidhje e forte lineare pozitive gje qe na le te kuptojme se
edhe ne vitet ne vijim, nje rritje ne vlere e TVSH, do te ndikoje ne rritjen e vleres se tatimit
mbi te ardhurat personale ")
    output$tab7 <- renderTable(paga)
    output$graf7 <- renderPlotly(
        fig
    )
    output$komente7<-renderText("Nga kjo tabele arrijme te bejme krahasimin e pages
minimale ne vendet e Ballkanit dhe TVSH qe paguan cdo qytatar. Shteti i cili ka pagen
minimale dhe tatimin mbi vleren e shtuar me vlere me te ulet eshte Kosova ndersa Greqia
ka pagen miminale dhe TVSH me vlere me te larte ne Ballkan. Duke studiuar grafikun,
shohim se ne Shqiperi, me nje page minimale 213 Euro/muaj, mesatarisht 42.6€ shkojne
per tatimin mbi vleren e shtuar.")
    output$tab8 <- renderTable(meviolak)
    output$graf8 <- renderPlot(
        ggplot(meviola, aes(factor(viti), vlera, fill = Lloji.i.tatimit)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(" Te ardhurat e shtetit (milion leke) ") +
            scale_fill_brewer(palette = "Set1")
    )
    output$komente8<-renderText("Per te dale ne perfundimin se cili nga tatimet ndikon me
shume buxhetin e shtetit, kemi ndertuar tabelen dhe grafikun ne te cilet shohim se
mesatarisht 262697 milion leke jane fitim nga tatimet dhe doganat, 15442 milion leke jane
nga pushteti lokal dhe 74696 nga fondet speciale. Lloji i tatimit qe ndihmon me shume ne
rritjen e te ardhurave te shtetit eshte ai nga tatimet dhe doganat i cili arriti vleren e tij
maksimale ne vitin 2019 me 304758 milion leke. Nderkohe tatimi nga pushteti lokal eshte
tatimi i cili nuk ka pesuar shume ndryshime me kalimin e viteve dhe ka qene i qendrueshem
ne vlera ndryshe nga dy tatimet e tjera te cilat kane pasur rritje te ndjeshme. ")
    output$tab9 <- renderTable(melisa)
    output$graf9 <- renderPlot(
        ggplot(df,aes(x,y))+geom_bar(stat="identity",fill="cornflowerblue")+
            labs(x="Paga", y="Taimi mbi fitimin ne %")+
            geom_text(aes(label=y),vjust=0)
    )
    output$komente9<-renderText("Tatimi mbi te ardhurat personale eshte nje sasi e caktuar
parash qe terhiqet cdo muaj nga paga e qytatetareve ne baze te te ardhurave te tyre. Duke
pare grafikun dhe tabelen, shohim se ne Shqiperi, per nje page qe varion nga 0 deri ne
30000 leke, shteti ka vendosur nje tatim prej 0%. Per nje page duke filluar nga 30001 leke
deri ne 150000 leke, qytetari tatohet 13% te shumes mbi 30000 lek ndersa per vlera duke
filluar nga 150001 leke, tatimi eshte 15600 leke + 23% te shumes mbi 150000 leke. ")
    output$tab10 <- renderTable(melisa1)
    output$graf10 <- renderPlotly(
        fig <- plot_ly(x = c(53.75,55,32.30,50,45,45,45,43,23,10,10,10), y =
                           c('Finlanda','Austria','Suedi','Belgjika','France','Gjermani','Greqi','Itali','Shqiperi','Serbi','Ko
sove','Maqedoni'), type = 'bar', orientation = 'h')
    )
    output$komente10<-renderText("Duke pasur parasysh vleren e tatimit mbi te ardhurat
personale ne Shqiperi, mund te bejme nje krahasim me vende te ndryshme te Ballkanit dhe
Europes. Nga grafiku veme re se shteti me vleren me te larte te ketij tatimi eshte Austria me
55% ndersa Serbia, Kosova dhe Maqedonia kane tatim prej 10%. Gjithashtu veme re se ne
krahasim me keto tre vende te Ballkanit, vendi yne ka vleren me te larte te tatimit mbi te
ardhurat personale.")
    output$tab11 <- renderTable(TabelaEPare)
    output$graf11 <- renderPlot(
        barplot(b,names.arg=a,xlab="periudha",ylab="te ardhurat ne billion",col="#71bc6c",
                main="Te ardhurat e Shqiperise nga taksat",border="#71bc6c")
    )
    output$komente11<-renderText("Ne kete tabele kemi te dhena per te ardhurat e
Shqiperise nga taksat ne milion dollare per muaj te ndryshem ne vitet 2020 dhe 2021.
Shohim se vlera me ulet eshte arritur ne Janar te vitit 2021 me vleren 240518 milion dollare
ndersa vlera me e larte ka vlere 2962570 ne dhjetor te vitit 2020. Mesatarisht, gjate kesaj
periudhe, vendi yne ka arritur nje shume prej 1476383 milion dollare te ardhura nga taksat.
")
    output$tab12 <- renderTable(nifi)
    output$graf12 <- renderPlot(
        plot(xn, yn, type = "b", pch = 19, col="cadetblue", xlab = "Viti", ylab = "Milion leke")
    )
}
shinyApp(ui, server)