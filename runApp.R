# Load packages
pacman::p_load('readxl','stringr','tidyr', 'dplyr', "shiny", 'tibble', 'purrr', 'jsonlite', 'htmltools')


# Define UI
ui <- fluidPage(
  
   includeCSS("www/style.css"),
  
  
  
  withTags(
    
    #head
    
    # Header
    header(div(class = "flex-container", 
               
               div(class = "logo", div(img(id = "logoDoc", src = "logo.png")),div(h3("Docplanner", br(), span("Formatter")))),
               
               nav(ul(class = "flex-container menuNav",
                 li(a("Documentation", href = "#")),
                 li(a("Tutorials", href = "#")),
                 li(a("Contribute", href = "#"))
                 
               ))))
  ),
  
  div( class = "sidebarLayout",
  
  # Layout function
  sidebarLayout(
    
    # left panel
    sidebarPanel(
      
      h2('Control panel'),
      
      # Select the type of task
      
      
      selectInput("type", label = strong("Select the type of task"),
                  choices = c(
                    
                    #Tasks
                    "IClinic", 
                    "Horizontal Spreadsheet",
                    "Erase empty files and columns",
                    "Fill and aggregate",
                    "IMedicina",
                    "Join tabs"),
                  selected = "IClinic"),
      
      
      
      # Upload data
      
      
      conditionalPanel(
        
        condition = "input.type == 'Horizontal Spreadsheet'||
                    input.type == 'IClinic' ||
                    input.type == 'Fill and aggregate' ||
                    input.type == 'IMedicina'",
        
      fileInput("file1", "Choose the file according to the task",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx"))),
      # Reactive UI 
      
      conditionalPanel(
        condition = "input.type == 'Horizontal Spreadsheet'",
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";"),
                     selected = ",")),
      
      
      conditionalPanel(
        condition = "input.type == 'Horizontal Spreadsheet'",
        div(
          textInput("lineMarker", "First Column"),
          textInput('columns', 'Other Colums'),
        )),
      
      conditionalPanel(
        condition = "input.type == 'Erase empty files and columns' ||
                    input.type == 'Join tabs'",
        textInput("directory", "Write the directory path"),
      ),
      
      conditionalPanel(
        condition = "input.type == 'Join tabs'",
        textInput("file2", "Write the file name"),
      ),
      
      conditionalPanel(
      condition = "input.type == 'Fill and aggregate'",
      selectInput("nColumns", label = strong("Number of columns"),
                  choices = c(
                    
                    #Tasks
                    "3", 
                    "4",
                    "5"),
                  selected = "3")
      ),
      
      
      ####
      
      actionButton("goButton2", "Go!"),
      
      conditionalPanel(
                    "input.type != 'Erase empty files and columns'",
        downloadButton("downloadData", "Download"),
        class= "downloadButton"
      )
      
    ),
    
    # Tables
    mainPanel(
      conditionalPanel(
        "input.type != 'Erase empty files and columns'",
        tabsetPanel(
          tabPanel("Before", tableOutput("before")),
          tabPanel("After", tableOutput("after"))
        )
      )
    )
  )),
)

server <- function(input, output) {
  
  
  ### Receive and verify the file ###
  dfUpload <- reactive({
    
    req(input$file1)
    
    if(input$file1$type == "application/vnd.ms-excel"){
      tryCatch(
        {
          dfUpload <-read.csv(input$file1$datapath, sep = input$sep, encoding = 'UTF-8')
          return(dfUpload)
        },
        error = function(e) {
          stop(safeError(e)) ### Estudar mais sobre as mensagens de erro e passar isso pro usuário
        }
      )
      
    }
    
    else {
      tryCatch(
        {
          dfUpload <-readxl::read_xlsx(input$file1$datapath)
          return(dfUpload)
        },
        error = function(e) {
          stop(safeError(e)) ## Estudar mais sobre as mensagens de erro e passar isso pro usuário
        }
      )
    }
    
    
  }) 
  
  # Substitute empty cells
  dfBefore <- reactive({
    dfBefore <- dfUpload()
    dfBefore <- dfBefore %>% mutate_all(funs(replace(., is.na(.), NA)))
    return(dfBefore)
  })
  
  # show table from before
  output$before <- renderTable({
    return(dfBefore())
  })
  
  #### Treatment ###
  treatment <- reactive({
    
    # Take a dependency on input$goButton
    
    input$goButton2
    
    isolate({
      
      # Iclinic File's (Brazil "issue")
      if(input$type == "IClinic"){
        req(input$file1);
        presp1 = presp3 = block =  attest = exam =  imagem = 
          tb_insert_attest = tb_insert_exam = tb_insert_presp1 = tb_insert_presp3 = 
          tbinsert = pacientes = tb_exam = tb_attest = tb_block = tb_presp1 = tb_presp3 =
          Attest_Desc = Block_Desc = Presp1_Desc = Presp3_Desc = Exam_Desc = physician_name = extra_pack =  NULL
        
        df <-read.csv(input$file1$datapath, sep=',', encoding = 'UTF-8') %>% 
          select(-physician_name,-extra_pack,-procedure_pack,-healthinsurance_pack) %>% 
          mutate(eventblock_pack = gsub("json::", "", eventblock_pack)) %>% 
          mutate(eventblock_pack = as.character(eventblock_pack)) %>% 
          mutate( P3 = case_when(
            str_detect(eventblock_pack, '"prescription_v3"')  ~ 'Prescription3',
            TRUE ~  "Outros"),
            P1 = case_when(
              str_detect(eventblock_pack, '"prescription_v1"')  ~ 'Prescription1',
              TRUE ~  "Outros"),
            EX = case_when(
              str_detect(eventblock_pack, '"exam_request"')  ~ 'Exam',
              TRUE ~  "Outros"),
            AT = case_when(
              str_detect(eventblock_pack, '"attest"')  ~ 'Attest',
              TRUE ~  "Outros"),
            BL = case_when(
              str_detect(eventblock_pack, '"block"')  ~ 'Block',
              TRUE ~  "Outros"),
            IM = case_when(
              str_detect(eventblock_pack, '"image"')  ~ 'Imagem', 
              TRUE ~  "Outros")
          )
        
        dadospacientes <- df %>% 
          select(pk,patient_id,patient_name,date,start_time,end_time)
        
        #### Verificando se as colunas existem na tabela ####
        
        existeP1 = FALSE
        
        for(numero in df$P1){
          if (numero != "Outros"){
            existeP1 = TRUE
          }
        }
        
        existeP3 = FALSE
        
        for(numero in df$P3){
          if (numero != "Outros"){
            existeP3 = TRUE
          }
        }
        
        existeEX = FALSE
        
        for(numero in df$EX){
          if (numero != "Outros"){
            existeEX = TRUE
          }
        }
        
        existeAT = FALSE
        
        for(numero in df$AT){
          if (numero != "Outros"){
            existeAT = TRUE
          }
        }
        
        existeBL = FALSE
        
        for(numero in df$BL){
          if (numero != "Outros"){
            existeBL = TRUE
          }
        }
        
        existeIM = FALSE
        
        for(numero in df$IM){
          if (numero != "Outros"){
            existeIM = TRUE
          }
        }
        
        
        
        
        #### TIPOS ####
        
        pacientes <- df %>% 
          select(pk,patient_id)
        
        if (existeP1 == TRUE){
          presp1 <- df %>% 
            filter(.,P1=="Prescription1")
        }
        
        
        if (existeP3 == TRUE){
          presp3 <- df %>% 
            filter(.,P3=="Prescription3")
        }
        
        if (existeEX == TRUE){
          exam <- df %>% 
            filter(.,EX=="Exam")
        }
        
        if (existeAT == TRUE){
          attest <- df %>% 
            filter(.,AT=="Attest")
        }
        
        
        if (existeBL == TRUE){
          block <- df %>% 
            filter(.,BL=="Block")
        }
        
        if (existeIM == TRUE){
          imagem <- df %>% 
            filter(.,IM=="Imagem")
        }
        
        #outros <- df %>% 
        # filter(.,FLAG=="Outros")
        
        
        #### CRIANDO DF ####
        #### imagem ####
        
        if (existeIM == TRUE){
          tb_imagem <- tibble()
          
          for (i in 1:nrow(block)){
            x <- imagem$eventblock_pack[i]
            tbinsert_imagem <-fromJSON(x, simplifyVector = FALSE) %>% 
              pluck('image') %>% 
              map_dfr(as.list) %>% 
              mutate(pk = imagem$pk[i],
                     patient_name = imagem$patient_name[i],
                     start_time = imagem$start_time[i],
                     end_time = imagem$end_time[i],
                     datadia = imagem$date[i]) %>% 
              select(pk,patient_name,start_time,end_time,datadia,image) %>% 
              mutate(image = 'Contem imagem') 
            tb_imagem <- tb_imagem %>% 
              bind_rows(tbinsert_imagem)  
          }
          
          tb_imagem <- tb_imagem %>% 
            unique() %>% 
            left_join(pacientes,by="pk") 
        }
        
        #### block ####
        if (existeBL == TRUE){
          tb_block <- tibble()
          
          for (i in 1:nrow(block)){
            x <- block$eventblock_pack[i]
            tbinsert <-fromJSON(x, simplifyVector = TRUE) %>% 
              pluck('block') %>% 
              select(-date_added,-kind,-ordering) %>% 
              rename(., nome = name) %>%
              rename(., descricao = value) %>% 
              mutate(pk = block$pk[i],
                     patient_name = block$patient_name[i],
                     start_time = block$start_time[i],
                     end_time = block$end_time[i],
                     datadia = block$date[i],
                     descricao = str_remove_all(.$descricao,'(<p>|</p>|<br/>|<br>)')
              ) %>%
              rename(., paciente = patient_name, hora_inicio = start_time, hora_fim = end_time) %>% 
              mutate(diagnostico = paste(nome,descricao,'\n')) %>% 
              select(-nome,-tab,-descricao) %>% 
              aggregate(diagnostico ~ pk + paciente + hora_inicio + hora_fim + datadia,
                        data = ., paste, collapse = "\n")
            tb_block <- tb_block %>% 
              bind_rows(tbinsert)  
          }
          
          tb_block <- tb_block %>% 
            left_join(pacientes,by="pk") 
        }
        
        #### attest ####
        if (existeAT == TRUE){
          tb_attest <- tibble()
          
          for (i in 1:nrow(attest)){
            x <- attest$eventblock_pack[i]
            tb_insert_attest <-fromJSON(x, simplifyVector = TRUE) %>% 
              pluck('attest') %>% 
              select(-date_added,-print_date) %>% 
              rename(., nome = name,descricao = value) %>% 
              mutate(pk = attest$pk[i],
                     patient_name = attest$patient_name[i],
                     start_time = attest$start_time[i],
                     end_time = attest$end_time[i],
                     datadia = attest$date[i]) %>% 
              rename(., paciente = patient_name, hora_inicio = start_time, hora_fim = end_time) %>% 
              mutate(diagnostico = paste(nome,descricao,'\n')) %>% 
              select(-nome,-tab,-descricao) %>% 
              aggregate(diagnostico ~ pk + paciente + hora_inicio + hora_fim + datadia,
                        data = ., paste, collapse = "\n") 
            tb_attest <- tb_attest %>% 
              bind_rows(tb_insert_attest) %>% 
              mutate(diagnostico = str_remove_all(.$diagnostico,'(<p style="text-align: center;">|<p>|</p>|<br/>|<br>|<b>|</b>|<p style="text-align: justify;">|&nbsp;|[A-Za-z0-9]{20,})'))
          }
          
          tb_attest <- tb_attest %>% 
            left_join(pacientes,by="pk") 
        }
        
        
        #### exam ####
        
        if (existeEX == TRUE){
          tb_exam <- tibble()
          
          for (i in 1:nrow(exam)){
            x <- exam$eventblock_pack[i]
            tb_insert_exam <-fromJSON(x, simplifyVector = TRUE) %>% 
              pluck('exam_request') %>% 
              mutate(., clinical_indication = if (exists('clinical_indication', where = .)) clinical_indication else NA,
                     date_added = if (exists('date_added', where = .)) date_added else NA,
                     exam_type = if (exists('exam_type', where = .)) exam_type else NA,
                     is_model = if (exists('is_model', where = .)) is_model else NA,
                     items = if (exists('items', where = .)) items else NA,
                     name = if (exists('name', where = .)) name else NA,
                     ordering = if (exists('ordering', where = .)) ordering else NA,
                     physician_id = if (exists('physician_id', where = .)) physician_id else NA,
                     print_date = if (exists('print_date', where = .)) print_date else NA,
                     tab = if (exists('tab', where = .)) tab else NA) %>% 
              select(-date_added,-ordering,-print_date,-physician_id,-exam_type,-is_model) %>% 
              rename(., nome = name) %>% 
              mutate(items = gsub("list", "", items),
                     tipo_exame = str_extract(items,pattern = '(?<= text )(.*)'),
                     pk = exam$pk[i],
                     patient_name = exam$patient_name[i],
                     start_time = exam$start_time[i],
                     end_time = exam$end_time[i],
                     datadia = exam$date[i],
              ) %>% 
              select(-items) %>% 
              rename(., paciente = patient_name, hora_inicio = start_time, hora_fim = end_time) %>% 
              mutate(descricao = paste(nome,tipo_exame,'\n')) %>%
              select(-nome,-tab,-clinical_indication,-tipo_exame) %>% 
              aggregate(descricao ~ pk + paciente + hora_inicio + hora_fim + datadia,
                        data = ., paste, collapse = "\n")
            tb_exam <- tb_exam %>% 
              bind_rows(tb_insert_exam) %>% 
              mutate(descricao = str_remove_all(.$descricao,'[0-9]{8}|("\\))'))
          }
          
          tb_exam <- tb_exam %>% 
            left_join(pacientes,by="pk")
        }
        
        
        #### presp3 ####
        if (existeP3 == TRUE){
          tb_presp3 <- tibble()
          
          for (i in 1:nrow(presp3)){
            x <- presp3$eventblock_pack[i]
            tb_insert_presp3 <-fromJSON(x, simplifyVector = FALSE) %>% 
              pluck('prescription_v3') %>% 
              map('items') %>% 
              map('drugs') %>% 
              map_dfr(as.list) %>% 
              select(-controlled,-drugKind,-id,-integration) %>% 
              mutate(., quantity = if (exists('quantity', where = .)) quantity else NA,
                     posology = if (exists('posology', where = .)) posology else NA,
                     description = if (exists('description', where = .)) description else NA,
                     name = if (exists('name', where = .)) name else NA) %>% 
              rename(., descricao = description,
                     nome = name,
                     posologia = posology,
                     quantidade = quantity) %>% 
              mutate(posologia = str_remove_all(.$posologia,'(<p>|</p>|<br>)'),
                     pk = presp3$pk[i],
                     patient_name = presp3$patient_name[i],
                     start_time = presp3$start_time[i],
                     end_time = presp3$end_time[i],
                     datadia = presp3$date[i]
              ) %>% 
              rename(., paciente = patient_name, hora_inicio = start_time, hora_fim = end_time) %>% 
              mutate(receita = paste('Medicação:',descricao,
                                     "\n",
                                     'Nome:',nome,
                                     "\n",
                                     'Posologia:',posologia,
                                     "\n",
                                     'Quantidade:',quantidade, 
                                     sep="\n")) %>%
              select(-descricao,-nome,-posologia,-quantidade) %>% 
              aggregate(receita ~ pk + paciente + hora_inicio + hora_fim + datadia,
                        data = ., paste, collapse = "\n")
            tb_presp3 <- tb_presp3 %>% 
              bind_rows(tb_insert_presp3) %>%
              select(pk,paciente,hora_inicio,hora_fim,datadia,receita) 
          }
          
          tb_presp3 <- tb_presp3 %>% 
            left_join(pacientes,by="pk") 
        }
        
        #### presp1 ####
        if (existeP1 == TRUE){
          tb_presp1 <- tibble()
          
          for (i in 1:nrow(presp1)){
            x <- presp1$eventblock_pack[i]
            tb_insert_presp1 <-fromJSON(x, simplifyVector = FALSE) %>% 
              pluck('prescription_v1') %>% 
              map('items') %>% 
              #map('drugs') %>% 
              map_dfr(as.list) %>% 
              select(-ordering,-kind) %>% 
              mutate(., quantity = if (exists('quantity', where = .)) quantity else NA,
                     posology = if (exists('posology', where = .)) posology else NA,
                     text = if (exists('text', where = .)) text else NA) %>% 
              rename(., texto = text,
                     posologia = posology,
                     quantidade = quantity) %>% 
              mutate(posologia = str_remove_all(.$posologia,'(<p>|</p>|<br>)'),
                     pk = presp1$pk[i],
                     patient_name = presp1$patient_name[i],
                     start_time = presp1$start_time[i],
                     end_time = presp1$end_time[i],
                     datadia = presp1$date[i]
              ) %>% 
              rename(., paciente = patient_name, hora_inicio = start_time, hora_fim = end_time) %>% 
              mutate(receita = paste('Medicação:',texto,
                                     "\n",
                                     'Posologia:',posologia,
                                     "\n",
                                     'Quantidade:',quantidade, 
                                     sep="\n")) %>%
              select(-texto,-posologia,-quantidade) %>% 
              aggregate(receita ~ pk + paciente + hora_inicio + hora_fim + datadia,
                        data = ., paste, collapse = "\n")
            tb_presp1 <- tb_presp1 %>% 
              bind_rows(tb_insert_presp1) %>% 
              select(pk,paciente,hora_inicio,hora_fim,datadia,receita) 
          }
          
          tb_presp1 <- tb_presp1 %>% 
            left_join(pacientes,by="pk")
        }
        
        ### edicao final ####
        
        rm(tb_insert_attest,tb_insert_exam,tb_insert_presp1,tb_insert_presp3,tbinsert,
           presp3,exam,pacientes,attest)
        
        lista1 <- c('pk')
        
        lista2 <- c()
        
        lista_df_final <- list()
        
        numero <- 1
        
        if (existeAT == TRUE){
          tb_attest <- tb_attest %>% 
            rename(., Attest_Desc = diagnostico);
          lista1 <- append(lista1, 'Attest_Desc');
          lista_df_final[[1]] <- tb_attest;
          numero = numero + 1;
        }else {Attest_Desc = NULL}
        
        if (existeBL == TRUE){
          tb_block <- tb_block %>% 
            rename(., Block_Desc = diagnostico)
          lista1 <- append(lista1, 'Block_Desc');
          lista_df_final[[numero]] <- tb_block;
          numero = numero + 1;
        } else{Block_Desc = NULL}
        
        if (existeEX == TRUE){
          tb_exam <- tb_exam %>% 
            rename(., Exam_Desc = descricao);
          lista1 <- append(lista1, 'Exam_Desc');
          lista_df_final[[numero]] <- tb_exam;
          numero = numero + 1;
        } else {Exam_Desc = NULL}
        
        if (existeP1 == TRUE){
          tb_presp1 <- tb_presp1 %>% 
            rename(., Presp1_Desc = receita);
          lista1 <- append(lista1, 'Presp1_Desc');
          lista_df_final[[numero]] <- tb_presp1;
          numero = numero + 1;
        } else {Presp1_Desc = NULL}
        
        if (existeP3 == TRUE){
          tb_presp3 <- tb_presp3 %>% 
            rename(., Presp3_Desc = receita)
          lista1 <- append(lista1, 'Presp3_Desc')
          lista_df_final[[numero]] <- tb_presp3;
          numero = numero + 1;
        }else{Presp3_Desc = NULL}
        
        if (existeIM == TRUE){
          tb_imagem <- tb_imagem %>% 
            rename(., Imagem = 'image')
          lista1 <- append(lista1, 'Imagem')
          lista_df_final[[numero]] <- tb_imagem;
        }else{Imagem = NULL}
        
        # concatenar por id paciente
        
        df_final <- lista_df_final %>% 
          reduce(full_join, by = "pk") %>% 
          select(lista1) %>% 
          mutate(Diagnostico = paste(Attest_Desc,
                                     "\n",
                                     Block_Desc,
                                     "\n",
                                     Presp1_Desc,
                                     "\n",
                                     Presp3_Desc,
                                     "\n",
                                     Exam_Desc,
                                     "\n",
                                     Imagem,
                                     "\n",
                                     sep="\n")) %>% 
          select(-Attest_Desc,-Block_Desc,-Presp1_Desc,-Presp3_Desc,-Exam_Desc,-Imagem) %>% 
          left_join(dadospacientes, by='pk')
        
        ###
        
        df_final <- df_final %>% 
          mutate(Diagnostico = gsub("NA|(<p)(.*)(>)|<u>|</u>|<ul>|<li>|<span>|</li>|<strong>|</strong>|</span>|<b>|</b>|\n\n\n\n\n\n\n\n\n|\n\n\n", " ", Diagnostico)) %>% 
          select(patient_id,patient_name,date,start_time,end_time,Diagnostico)
        
        dfFinal <- df_final[!duplicated(df_final), ]
        
      }
    
      # Horizontal spreadsheet
      else if(input$type == "Horizontal Spreadsheet"){
        
        req(input$file1)
        
        df1 <- dfBefore()
        
        df1 <- df1 %>%
          mutate_all(funs(replace(., is.na(.), "EMPTY CELL")))
        
        lineMarker <- input$lineMarker #First word
        
        x <- as.vector(unlist(strsplit(input$columns,","))) # Other words
        
        columnMarkers <- c(x)
        listSize <- length(columnMarkers)
        
        df2 <- data.frame()
        df1Columns <- ncol(df1)
        df1Lines <- nrow(df1)
        lineDF2 <- 0
        columnDF2 =  1
        
        
        for (l in 1:df1Lines){
          c = 1
          while (c <= df1Columns){
            x = 1
            if(df1[l,c] == lineMarker){
              lineDF2 = lineDF2 + 1 
              columnDF2 =  1
            }
            while(x <= listSize){
              if(df1[l,c] == columnMarkers[x]){
                columnDF2 <-  columnDF2 + 1
                x = listSize 
              }
              x = x +1
            }
            if ( df1[l,c] != "EMPTY CELL" ){
              df2[lineDF2,columnDF2] <- df1[l,c]
            }
            c = c + 1
          }
        }
        
        dfFinal <- df2
        
      }
      
      # Fill and aggragate
      else if(input$type == "Fill and aggregate"){
        
        req(input$file1)
        
        df <- dfBefore()
        
        
        ###########################################
        lastColumn <- 3
        column1 <- c()
        y <- 1
        lines <- nrow(df)
        
        for (i in 1:lines) {
          if (is.na(df[i,1]) == FALSE) {
            column1[y] <- df[i,1]
            x <- df[i,1]
            y = y + 1
          }else {
            column1[y] <- x
            y = y + 1
          }
          
        }
        
        column1Final <- c()
        
        y <- 1
        for (i in 1:lines) {
          column1Final[y] <- column1[[y]]
          y = y+1
        }
        
        
        
        ###########################################
        
        column2 <- c()
        y <- 1
        
        for (i in 1:lines) {
          if (is.na(df[i,2]) == FALSE) {
            column2[y] <- df[i,2]
            x <- df[i,2]
            y = y + 1
          }else {
            column2[y] <- x
            y = y + 1
          }
          
        }
        
        column2Final <- c()
        
        y <- 1
        for (i in 1:lines) {
          column2Final[y] <- column2[[y]]
          y = y+1
        }
        
        
        
        ###########################################
        
        
        if(input$nColumns == "4"){
          lastColumn <- 4
          column3 <- c()
          y <- 1
          
          for (i in 1:lines) {
            if (is.na(df[i,3]) == FALSE) {
              column3[y] <- df[i,3]
              x <- df[i,3]
              y = y + 1
            }else {
              column3[y] <- x
              y = y + 1
            }
            
          }
          column3Final <- c()
          
          y <- 1
          for (i in 1:lines) {
            column3Final[y] <- column3[[y]]
            y = y+1
          }
          
        }else{
          column3Final <- c()
          y <- 1
          for (i in 1:lines) {
            column3Final[y] <- NA
            y = y+1
          }
          
        }
        
        
        
        
        ###########################################
        
        if(input$nColumns == "5"){
          
          lastColumn <- 5
          
          column3 <- c()
          y <- 1
          
          for (i in 1:lines) {
            if (is.na(df[i,3]) == FALSE) {
              column3[y] <- df[i,3]
              x <- df[i,3]
              y = y + 1
            }else {
              column3[y] <- x
              y = y + 1
            }
            
          }
          column3Final <- c()
          
          y <- 1
          for (i in 1:lines) {
            column3Final[y] <- column3[[y]]
            y = y+1
          }
          
          
          column4 <- c()
          y <- 1
          
          for (i in 1:lines) {
            if (is.na(df[i,4]) == FALSE) {
              column4[y] <- df[i,4]
              x <- df[i,4]
              y = y + 1
            }else {
              column4[y] <- x
              y = y + 1
            }
            
          }
          column4Final <- c()
          
          y <- 1
          for (i in 1:lines) {
            column4Final[y] <- column4[[y]]
            y = y+1
          }
          
        }else{
          column4Final <- c()
          y <- 1
          for (i in 1:lines) {
            column4Final[y] <- NA
            y = y+1
          }
          
        }
        
        
        
        ###########################################
        
        
        
        df2 <- data.frame(column1Final, column2Final, column3Final,column4Final,df[lastColumn])
        
        colnames(df2)[5] <- "lastColumn"
        
        y <- 1
        
        for (i in 1:lines) {
          if(is.na(column3Final[y]) == TRUE){
            df2$column3Final <- NULL
            break
          }
        }
        
        for (i in 1:lines) {
          if(is.na(column4Final[y]) == TRUE){
            df2$column4Final <- NULL
            break
          }
        }
        
        
        
        ###########################################
        if(input$nColumns == "3"){
          dfFinal <- df2 %>%
            aggregate(lastColumn ~ column1Final + column2Final,
                      data = ., paste, collapse = "\n")
        }
        
        if(input$nColumns == "4"){
          dfFinal <- df2 %>%
            aggregate(lastColumn ~ column1Final + column2Final +  column3Final,
                      data = ., paste, collapse = "\n")
        }
        
        if(input$nColumns == "5"){
          dfFinal <- df2 %>%
            aggregate(lastColumn ~ column1Final + column2Final +  column3Final +  column4Final,
                      data = ., paste, collapse = "\n")
        }
      
        
        
      }
      
      else if(input$type == "IMedicina"){
        
        req(input$file1)
        
        df <- dfBefore()
        
        dfFinal <- data.frame()%>%
          mutate("ID", "Nome","evento", "Inicio", "Anamnese")
        
        linhas <- nrow(df)
        colunas <- ncol(df)
        
        
        
        l = 0
        
        y = 0
        
        while(y<=linhas){
          y = y+1
          x = 3
          
          while(x<=colunas){
            c = 1
            if(is.na(df[y,x]) == FALSE){
              l = l + 1
              dfFinal[l,c] <- df[y,1]
              c = c + 1
              dfFinal[l,c] <- df[y,2]
              # Fazer split do atendimento
              splitInicio <-c(str_split(df[y,x], "inicio:"))
              c = c + 1
              dfFinal[l,c] <- splitInicio[[1]][1]
              
              
              c = c + 1
              splitFim <-c(str_split(splitInicio[[1]][2], "fim:"))
              dfFinal[l,c] <- splitFim[[1]][1]
              
              
              c = c + 1
              splitAnamnese <-c(str_split(splitInicio[[1]][2], "anamnese:"))
              dfFinal[l,c] <- splitAnamnese[[1]][2]
            }
            x = x +1
          }
        }
      }
      
      return(dfFinal)
    })
    
  })
  
  # Erase an empty file in a specific path
  ObserveCases <- observe({
    
    input$goButton2
    
    isolate({
      
      if(input$type == "Erase empty files and columns"){ 
      
      diretorio <- input$directory
      
      setwd(diretorio)
      
      arquivos <- list.files(diretorio, pattern = "\\.xlsx")
      
      for (arquivo in arquivos){
        
        arquivo <- str_c(diretorio, "/", arquivo)
        
        df <-readxl::read_xlsx(arquivo)
        
        dfLines <- nrow(df)
        dfCols = ncol(df)
        c = 1
        lista <- c();
        
        if(dfLines == 0) {
          file.remove(arquivo)
        }else {
          
          while(c <= dfCols){
            l = 1
            hasValue = FALSE;
            while(l<=dfLines){
              if(is.na(df[l,c]) == FALSE){
                lista <- append(lista, c)
                break;
              }
              l = l+1
            }
            
            c = c+1
          }
          
          c = dfCols
          while(c > 0) {
            if(c %in% lista == FALSE){
              df[c] = NULL;
            }
            c = c - 1
          }
          writexl::write_xlsx(df,arquivo)
        }
        
        
      }
      
      

      }
      
      else if(input$type == "Join tabs"){
        directory <- input$directory
        setwd(directory)
        file <- input$file2
        planilhas <- lapply(excel_sheets(file), read_excel,path = file)
        nomes <- excel_sheets(file)
        dfFinal <- data.frame()
        
        
        l = 0
        numeroPlanilha = 0
        for(planilha in planilhas){
          numeroPlanilha = numeroPlanilha +1
          colunas = ncol(planilha)
          linhas = nrow(planilha)
          x = 1
          while(x < linhas){
            y = 1
            l = l +1
            while(y < colunas +1){
              if(is.na(planilha[x,y]) == FALSE) {
                dfFinal[l,y] = planilha[x,y]
              } else {
                dfFinal[l,y] = "celula vazia"
              }
              
              y = y +1
            }
            dfFinal[l, y] <- nomes[numeroPlanilha]
            x = x+1
            
          }
        }
        finalPath <- str_c(directory, "/" ,"FinalFile.xlsx")
        
        writexl::write_xlsx(dfFinal, finalPath)
        
      }
    })
  })
  
  # Show end table
  output$after <- renderTable({
    return(treatment())
  })
  
  ### Download Table 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("arquivoFinal", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(treatment(),file)
    }
  )
  
}

shinyApp(ui, server)
