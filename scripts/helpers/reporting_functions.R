get_intervals <- function(ref_day){
        
        
        # list(ytd_prev_start = ymd(paste0(year(ref_day)-1,"-","02","-","01")),
        #      ytd_prev_end = ymd(paste0(year(ref_day)-1,"-",month(ref_day),"-",day(ref_day))),
        #      ytd_curr_start = ymd(paste0(year(ref_day),"-","02","-","01")),
        #      ytd_curr_end = ref_day,
        #      mtd_prev_start = ymd(paste0(year(ref_day)-1,"-",month(ref_day),"-","01")),
        #      mtd_prev_end = ymd(paste0(year(ref_day)-1,"-",month(ref_day),"-",day(ref_day))),
        #      mtd_curr_start = ymd(paste0(year(ref_day),"-",month(ref_day),"-","01")),
        #      mtd_curr_end = ymd(paste0(year(ref_day),"-",month(ref_day),"-",day(ref_day)))
        # )
        
        # prepare for 2018
        list(ytd_prev_start = ymd(paste0(year(ref_day)-1,"-","01","-","01")),
             ytd_prev_end = ymd(paste0(year(ref_day)-1,"-",month(ref_day),"-",day(ref_day))),
             ytd_curr_start = ymd(paste0(year(ref_day),"-","01","-","01")),
             ytd_curr_end = ref_day,
             mtd_prev_start = ymd(paste0(year(ref_day)-1,"-",month(ref_day),"-","01")),
             mtd_prev_end = ymd(paste0(year(ref_day)-1,"-",month(ref_day),"-",day(ref_day))),
             mtd_curr_start = ymd(paste0(year(ref_day),"-",month(ref_day),"-","01")),
             mtd_curr_end = ymd(paste0(year(ref_day),"-",month(ref_day),"-",day(ref_day)))
        )
        
}

get_path <- function(key, base_path = "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/ecommerce_report/"){
        
        
        
        if(!exists("paths")){
                path_file <- paste0(base_path,"data/paths/Paths.xlsx")
                paths <<- read_excel(path_file, sheet = "Paths")
        }
        
        
        res <- paths %>% 
                filter(pointer == key) %>% 
                pull(path) 
        
        
        if(length(res) == 0){
                stop(paste0("Not found file pointer to ",key))
        } else{paste0(base_path,res)}
        
        
        
}


# SALES DATA --------------------------------------------------------------
load_retail <- function(ref_day){
        
        
        #define name of columns
        col_names <- c("date",
                       "country_code",
                       "country_text",
                       "currency",
                       "brand_code",
                       "brand_text",
                       "val_saldi_loc_curr",
                       "val_reg_loc_curr",
                       "qty_saldi",
                       "qty_reg")
        
        
        #define type of coulmns
        col_types <- cols(
                .default = col_character(),
                val_saldi_loc_curr = col_number(),
                val_reg_loc_curr = col_number(),
                qty_saldi = col_number(),
                qty_reg = col_number()
        )
        
        
        
        #load previous year
        f <- get_path("retail_previous_year")
        retail_prev <- read_delim(f, delim = ";", skip = 5, col_types = col_types, col_names = col_names, trim_ws = T, na = c("","#"), locale = locale(decimal_mark = ","))
        m <- sprintf("load_retail(): Loaded previous year retail data: %d rows from file %s",nrow(retail_prev), f)
        message(m)
        
        
        
        #load current year
        f <- get_path("retail_current_year")
        retail_curr <- read_delim(f, delim = ";", skip = 5, col_types = col_types, col_names = col_names, trim_ws = T, na = c("","#"), locale = locale(decimal_mark = ","))
        m <- sprintf("load_retail(): Loaded current year retail data: %d rows from file %s",nrow(retail_curr), f)
        message(m)
        
        
        
        #bind rows
        retail <- retail_prev %>% bind_rows(retail_curr)
        
        
        #format columns
        retail <- retail %>% 
                mutate(date = dmy(date)) %>% 
                mutate(isoweek = as.double(isoweek(date))) %>% 
                mutate(isoyear = as.double(isoyear(date))) %>% 
                mutate_if(is.numeric, funs(if_else(is.na(.),0,.)))
        
        # adjust for 1t Jan 2017
        retail <- retail %>% 
                mutate(isoweek = case_when(date == ymd("20170101") ~ 1, TRUE ~ isoweek)) %>% 
                mutate(isoyear = case_when(date == ymd("20170101") ~ 2017, TRUE ~ isoyear))
        
        
        #edit "MILAN"
        retail <- retail %>%  
                mutate(country_code = case_when(country_code == "MI" ~ "IT", T ~ country_code)) %>% 
                mutate(country_text = case_when(country_text == "Milano" ~ "Italy", T ~ country_text))
        
        #clean data
        not_found <- retail %>% filter(is.na(country_code)) %>% nrow()
        if(not_found > 0){
                m <- sprintf("load_retail(): Warning - dropped %d records from retail data having <NA> country_code", not_found)
                message(m)
                
                retail <- retail %>% 
                        filter(!is.na(brand_code))
                
        }
        
        #order columns, assign sales channel and add ytd mtd flags
        retail <<- retail %>% 
                select(isoyear,isoweek,date:brand_text,qty_saldi,val_saldi_loc_curr,qty_reg,val_reg_loc_curr) %>% 
                mutate(sales_channel = "retail") %>% 
                mutate(ytd = (date <= get_intervals(ref_day)$ytd_curr_end & date >= get_intervals(ref_day)$ytd_curr_start) | (date <= get_intervals(ref_day)$ytd_prev_end & date >= get_intervals(ref_day)$ytd_prev_start)) %>%
                mutate(mtd = (date <= get_intervals(ref_day)$mtd_curr_end & date >= get_intervals(ref_day)$mtd_curr_start) | (date <= get_intervals(ref_day)$mtd_prev_end & date >= get_intervals(ref_day)$mtd_prev_start)) %>% 
                mutate(ref_day = ref_day)
        
        
        
        
}

load_ecommerce <- function(ref_day){
        
        
        #define name of columns
        col_names <- c("business_area_code",
                       "business_area_text",
                       "date",
                       "country_code",
                       "country_text",
                       "currency",
                       "brand_code",
                       "brand_text",
                       "shipping_shop_code",
                       "shipping_shop_text",
                       "collection_code",
                       "collection_text",
                       "commercial_class_code",
                       "commercialclass_text",
                       "budget_class_code",
                       "budgetclass_text",
                       "merchandising_class_code",
                       "merchandising_class_text",
                       "level_1_gmo_code",
                       "level_1_gmo_text",
                       "level_2_gmo_code",
                       "level_2_gmo_text",
                       "level_3_gmo_code",
                       "level_3_gmo_text",
                       "article_style_code",
                       "article_style_text",
                       "parte_code",
                       "parte_text",
                       "variant_type_code",
                       "variant_type_text",
                       "variant_code",
                       "variant_text",
                       "color_code",
                       "color_text",
                       "brand_eqv_code",  
                       "brand_eqv_text",
                       "collection_eqv_code",
                       "collection_eqv_text",
                       "commercial_eqv_class_code",
                       "commercialclass_eqv_text",
                       "budget_class_eqv_code",
                       "budgetclass_eqv_text",
                       "merchandising_class_eqv_code",
                       "merchandising_class_eqv_text",
                       "article_style_eqv_code",
                       "article_style_eqv_text",
                       "parte_eqv_code",
                       "parte_eqv_text",
                       "variant_type_eqv_code",
                       "variant_eqv_code",
                       "color_eqv_code",
                       "color_eqv_text",
                       "qty_reg",
                       "val_reg_loc_curr",
                       "qty_saldi",
                       "val_saldi_loc_curr")
        
        
        #define type of coulmns
        col_types <- cols(
                .default = col_character(),
                val_saldi_loc_curr = col_number(),
                val_reg_loc_curr = col_number(),
                qty_saldi = col_number(),
                qty_reg = col_number()
        )
        
        
        
        #load previous year
        f <- get_path("ecommerce_previous_year")
        ecommerce_prev <- read_delim(f, delim = ";", skip = 5,col_names = col_names, col_types = col_types, trim_ws = T, na = c("","#"), locale = locale(decimal_mark = ","))
        m <- sprintf("load_ecommerce(): Loaded previous year ecommerce data: %d rows from file %s",nrow(ecommerce_prev), f)
        message(m)
        
        
        #load current year
        f <- get_path("ecommerce_current_year")
        ecommerce_curr <- read_delim(f, delim = ";", skip = 5,col_names = col_names, col_types = col_types, trim_ws = T, na = c("","#"), locale = locale(decimal_mark = ","))
        m <- sprintf("load_ecommerce(): Loaded current year ecommerce data: %d rows from file %s",nrow(ecommerce_curr), f)
        message(m)
        
        
        
        #bind rows
        ecommerce <- ecommerce_prev %>% bind_rows(ecommerce_curr)
        
        
        #format columns
        ecommerce <- ecommerce %>% 
                mutate(date = dmy(date)) %>% 
                mutate(isoweek = as.double(isoweek(date))) %>% 
                mutate(isoyear = as.double(isoyear(date))) %>% 
                mutate_if(is.numeric, funs(if_else(is.na(.),0,.)))
        
        
        # adjust for 1t Jan 2017
        ecommerce <- ecommerce %>% 
                mutate(isoweek = case_when(date == ymd("20170101") ~ 1, TRUE ~ isoweek)) %>% 
                mutate(isoyear = case_when(date == ymd("20170101") ~ 2017, TRUE ~ isoyear))
        
        
        #order columns, add sales channel and ytd and mtd flags
        ecommerce <- ecommerce %>% 
                select(isoyear,isoweek,business_area_code:color_eqv_text,qty_saldi,val_saldi_loc_curr,qty_reg,val_reg_loc_curr) %>% 
                mutate(sales_channel = if_else(grepl("farfetch", business_area_text, ignore.case = T),"farfetch","ecommerce")) %>% 
                mutate(ytd = (date <= get_intervals(ref_day)$ytd_curr_end & date >= get_intervals(ref_day)$ytd_curr_start) | (date <= get_intervals(ref_day)$ytd_prev_end & date >= get_intervals(ref_day)$ytd_prev_start)) %>%
                mutate(mtd = (date <= get_intervals(ref_day)$mtd_curr_end & date >= get_intervals(ref_day)$mtd_curr_start) | (date <= get_intervals(ref_day)$mtd_prev_end & date >= get_intervals(ref_day)$mtd_prev_start)) %>% 
                mutate(ref_day = ref_day)
        
        
        #clean data
        not_found <- ecommerce %>% filter(is.na(brand_code)) %>% nrow()
        if(not_found > 0){
                m <- sprintf("load_ecommerce(): Warning - dropped %d records from ecommerce data having <NA> brand_code", not_found)
                message(m)
                
                ecommerce <- ecommerce %>% 
                        filter(!is.na(brand_code))
                
        }
        
        not_found <- ecommerce %>% filter(country_text == "Not assigned") %>% nrow()
        if(not_found > 0){
                m <- sprintf("load_ecommerce(): Warning - dropped %d records from ecommerce data having 'Not assigned' country_text", not_found)
                message(m)
                
                ecommerce <- ecommerce %>% 
                        filter(country_text != "Not assigned")
        }
        
        
        ecommerce <<- ecommerce
        
        
        
}

add_ecommerce_flag <- function(){
        retail <<- ecommerce %>% 
                select(country_code) %>% 
                distinct() %>% 
                mutate(ecommerce_country = "e-Comm. Countries") %>% 
                right_join(retail, by = "country_code") %>% 
                mutate(ecommerce_country = if_else(is.na(ecommerce_country),"Non e-Comm. Countries",ecommerce_country)) %>% 
                mutate(ecommerce_country = if_else(country_code == "MI","e-Comm. Countries",ecommerce_country))
        
        
}

add_exchange_rates <- function(){
        
        exchange_rate <- read_excel(path = get_path("exchange_rates"), sheet = "exchange_rates", trim_ws = T)
        
        
        ecommerce <<- ecommerce %>% 
                left_join(exchange_rate, by = "currency") %>% 
                mutate(val_saldi_eur = val_saldi_loc_curr/exchange_rate,
                       val_reg_eur = val_reg_loc_curr/exchange_rate)
        
        
        retail <<- retail %>%
                left_join(exchange_rate, by = "currency") %>%
                mutate(val_saldi_eur = val_saldi_loc_curr/exchange_rate,
                       val_reg_eur = val_reg_loc_curr/exchange_rate)
        
        
        
        
}

add_classe <- function(){
        
        classe <- read_excel(path = get_path("categories_map"), sheet = "categories")
        
        
        
        ecommerce <<- ecommerce %>% 
                mutate(cat_id = paste0(brand_code,commercial_class_code,collection_code)) %>% 
                left_join(classe, by = "cat_id")
        
        not_found <- ecommerce %>% filter(is.na(classe_new))
        if(not_found %>% nrow() > 0){
                n <- not_found %>% nrow
                d <- max(not_found$date)
                m <- sprintf("add_classe(): Warning - classe_new is <NA> for %d records in ecommerce data, last record is on date %s. Check %s for missing categorization", n, d, categories_path)
                message(m)
        }
        
        
}

add_totals <- function(){
        
        
        
        ecommerce <<- ecommerce %>% 
                mutate(qty_tot = qty_reg + qty_saldi,
                       val_tot_eur = val_reg_eur+val_saldi_eur)
        
        retail <<- retail %>% 
                mutate(qty_tot = qty_reg + qty_saldi,
                       val_tot_eur = val_reg_eur+val_saldi_eur)
        
        
}

add_sku_metadata <- function(){
        
        
        ecommerce <<- ecommerce %>% 
                mutate(sku_id_std = sku_id_std(model = article_style_code, type_variant = variant_type_code, variant = variant_code, material = parte_code, color = color_code, sep = "_")) %>% 
                mutate(sku_id_website = sku_id_custom(article_style_code,parte_code,color_code,variant_type_code,variant_code, sep = "_")) %>% 
                mutate(sku_eqv_id_std = sku_id_std(model = article_style_eqv_code, type_variant = variant_type_eqv_code, variant = variant_eqv_code, material = parte_eqv_code, color = color_eqv_code, sep = "_")) %>% 
                mutate(sku_eqv_id_website = sku_id_custom(article_style_eqv_code,parte_eqv_code,color_eqv_code,variant_type_eqv_code,variant_eqv_code, sep = "_")) %>% 
                mutate(img_link = case_when(
                        brand_code %in% c("P","PS") ~ paste0("https://assets.prada.com/content/dam/prada-ecommerce/images/zoom/",sku_id_website,"-1.jpg/_jcr_content/renditions/original"),
                        brand_code == "M" ~ paste0("http://assets.miumiu.com/content/images/products/zooms/",sku_id_website,"-1.jpg")))
        
}

add_special_flags <- function(){
        
        #ballerina flag
        ecommerce <- ecommerce %>% 
                mutate(ballerina_flag = if_else(brand_code == "M" & article_style_code == "5F466A","Ballerina","All others"))
        
        
        #galleria flag
        galleria_bags <- read_excel(get_path("galleria_bags"), sheet = "GalleriaList") %>% 
                transmute(sku_eqv_id_std = sku_id_std(model = article_style_eqv_code, type_variant = variant_type_eqv_code, variant = variant_eqv_code, material = parte_eqv_code, color = color_eqv_code, sep = "_")) %>% 
                mutate(galleria_flag = T) %>% 
                distinct()
        
        ecommerce <<- ecommerce %>% 
                left_join(galleria_bags,by = "sku_eqv_id_std") %>% 
                replace_na(list(galleria_flag = F)) %>% 
                mutate(galleria_flag = if_else(galleria_flag,"Galleria","All others"))
        
        
}

drop_outliers <- function(){
        
        
        out <- read_excel(get_path("outliers"))
        
        m <- sprintf("drop_outliers(): dropped %s outliers specified in file %s",nrow(out),get_path("outliers"))
        message(m)
        
        ecommerce <<- out %>% 
                mutate(date = as.Date(date)) %>% 
                anti_join(ecommerce,., by = c("date", "country_code", "qty_tot", "sku_id_std"))
        
        
}

save_datasets <- function(drop_outliers = T){
        
        # override default scientific notations
        options(scipen=999) 
        
        m <- sprintf("save_datasets(): Saving retail data")
        message(m)
        
        retail <<- retail %>% 
                select_if(is.Date) %>% 
                bind_cols(retail %>% select_if(is.character)) %>% 
                bind_cols(retail %>% select_if(is.numeric)) %>% 
                bind_cols(retail %>% select_if(is.logical))
        
        write_csv(retail, path = get_path("retail_output"), na = "")
        
        m <- sprintf("save_datasets(): Saving ecommerce data")
        message(m)
        
        if(drop_outliers){
                drop_outliers()
        }
        
        ecommerce <<- ecommerce %>% 
                select_if(is.Date) %>% 
                bind_cols(ecommerce %>% select_if(is.character)) %>% 
                bind_cols(ecommerce %>% select_if(is.numeric)) %>% 
                bind_cols(ecommerce %>% select_if(is.logical))
        
        write_csv(ecommerce, path = get_path("ecommerce_output"), na = "")
        
        
        
}




# BEST SELLERS --------------------------------------------------------------
bs_clean_data <- function(){
        
        
        #clean brand and collection
        clean <<- ecommerce %>% 
                mutate(tmp_brand = if_else(brand_code == "PS","P",brand_code)) %>% 
                mutate(tmp_brand_text = if_else(brand_text == "Prada Linea Rossa","Prada",brand_text)) %>% 
                filter(tmp_brand %in% c("M","P")) %>% 
                filter(collection_code %in% c("D","U"))
        
}

bs_stitch_images <- function(filename, brand, collection, start_date, end_date, sort_type, top_n){
        
        #download files 
        
        m <- sprintf("bs_stitch_images(): Downloading and stitching images for file %s",filename)
        message(m)
        
        temp <- clean %>% 
                filter(tmp_brand == brand) %>% 
                filter(collection_code == collection) %>% 
                filter(!grepl("Farfetch|Aizel",business_area_text, ignore.case = T)) %>% 
                filter(date >= start_date & date <= end_date) %>% 
                group_by(sku_id_website,classe_new,img_link) %>% 
                summarise_at(vars(qty_tot,val_tot_eur),sum) %>% 
                ungroup()
        
        if(sort_type == "quantity"){
                temp <- temp %>% 
                        arrange(desc(qty_tot),desc(val_tot_eur)) %>% 
                        slice(1:top_n)
        } else {
                temp <- temp %>% 
                        arrange(desc(val_tot_eur),desc(qty_tot)) %>% 
                        slice(1:top_n)
        }
        
        
        #img_temp <- image_read(temp$img_link) %>% image_scale(geometry = "200")
        img_read_safe <- possibly(.f = ~ image_read(.x) %>% image_scale(geometry = "200"), otherwise = image_read(get_path("not_found_image")) %>% image_scale(geometry = "200"))
        img_temp <- map(temp$img_link,img_read_safe)
        
        img_temp <- image_append(c(image_append(c(img_temp[[1]],img_temp[[2]],img_temp[[3]],img_temp[[4]]),stack = F),
                                   image_append(c(img_temp[[5]],img_temp[[6]],img_temp[[7]],img_temp[[8]]),stack = F),
                                   image_append(c(img_temp[[9]],img_temp[[10]],img_temp[[11]],img_temp[[top_n]]),stack = F)),
                                 stack = T)
        image_write(img_temp,paste0(get_path("best_sellers_path"),filename),format = "jpg", quality = 100)
        
        #return data
        best_sellers <<- temp %>% 
                mutate(ref_day = ref_day) %>% 
                mutate(filename = filename) %>% 
                mutate(rank = row_number()) %>% 
                bind_rows(best_sellers,.)
        
        
        
}

bs_stitch_images_grouped <- function(filename, brand, collection, start_date, end_date, sort_type, top_n){
        
        #download files 
        
        m <- sprintf("bs_stitch_images(): Downloading and stitching images for file %s",filename)
        message(m)
        
        temp <- clean %>% 
                filter(tmp_brand == brand) %>% 
                filter(collection_code == collection) %>% 
                filter(!grepl("Farfetch|Aizel|Selfridges",business_area_text, ignore.case = T)) %>% 
                filter(date >= start_date & date <= end_date) %>% 
                mutate(sku_id_website = sku_id_custom(article_style_code,variant_type_code,variant_code,parte_code,item5 = "*",sep = "_")) %>% 
                group_by(sku_id_website,classe_new) %>% 
                summarise(img_link = first(img_link),
                          qty_tot = sum(qty_tot),
                          val_tot_eur = sum(val_tot_eur)) %>% 
                ungroup()
        
        if(sort_type == "quantity"){
                temp <- temp %>% 
                        arrange(desc(qty_tot),desc(val_tot_eur)) %>% 
                        slice(1:top_n)
        } else {
                temp <- temp %>% 
                        arrange(desc(val_tot_eur),desc(qty_tot)) %>% 
                        slice(1:top_n)
        }
        
        
        #img_temp <- image_read(temp$img_link) %>% image_scale(geometry = "200")
        img_read_safe <- possibly(.f = ~ image_read(.x) %>% image_scale(geometry = "200"), otherwise = image_read(get_path("not_found_image")) %>% image_scale(geometry = "200"))
        img_temp <- map(temp$img_link,img_read_safe)
        
        img_temp <- image_append(c(image_append(c(img_temp[[1]],img_temp[[2]],img_temp[[3]],img_temp[[4]]),stack = F),
                                   image_append(c(img_temp[[5]],img_temp[[6]],img_temp[[7]],img_temp[[8]]),stack = F),
                                   image_append(c(img_temp[[9]],img_temp[[10]],img_temp[[11]],img_temp[[top_n]]),stack = F)),
                                 stack = T)
        image_write(img_temp,paste0(get_path("best_sellers_path"),filename),format = "jpg", quality = 100)
        
        #return data
        best_sellers <<- temp %>% 
                mutate(ref_day = ref_day) %>% 
                mutate(filename = filename) %>% 
                mutate(rank = row_number()) %>% 
                bind_rows(best_sellers,.)
        
        
        
}

bs_get_images <- function(ref_day, top_n = 10){
        
        #setup
        file.remove(list.files(get_path("best_sellers_path"), full.names = T))
        ytd_start <- get_intervals(ref_day)$ytd_curr_start      
        best_sellers <<- tbl_df(NULL)
        
        
        
        #get Miu Miu data - last 28 days rolling
        bs_stitch_images_grouped(filename = "miumiu_woman_28roll_quantity.jpg",brand = "M", collection = "D", start_date = ref_day-28,end_date = ref_day,sort_type = "quantity",top_n)
        bs_stitch_images_grouped(filename = "miumiu_woman_28roll_value.jpg",brand = "M", collection = "D", start_date = ref_day-28,end_date = ref_day,sort_type = "value",top_n)
        
        
        #get Prada Woman data - last 28 days rolling
        bs_stitch_images(filename = "prada_woman_28roll_quantity.jpg",brand = "P", collection = "D", start_date = ref_day-28, end_date = ref_day,sort_type = "quantity",top_n)
        bs_stitch_images(filename = "prada_woman_28roll_value.jpg",brand = "P", collection = "D", start_date = ref_day-28, end_date = ref_day,sort_type = "value",top_n)
        
        #get Prada Man data - last 28 days rolling
        bs_stitch_images(filename = "prada_man_28roll_quantity.jpg",brand = "P", collection = "U", start_date = ref_day-28,end_date = ref_day,sort_type = "quantity",top_n)
        bs_stitch_images(filename = "prada_man_28roll_value.jpg",brand = "P", collection = "U", start_date = ref_day-28,end_date = ref_day,sort_type = "value",top_n)
        
}

bs_save_dataset <- function(){
        
        write.csv2(best_sellers, file = get_path("best_sellers_output"), na = "", row.names = F)
        
        
        
}



# GA TRAFFIC ---------------------------------------------------------
ga_get_data <- function(start_date, end_date, brand, dimensions, metrics, segments = NULL, filters = NULL, split_daywise = T, paginate_query = F,use_miumiu_mirror = F){
        
        # details on dimensions and metrics
        # https://developers.google.com/analytics/devguides/reporting/core/dimsmets
        
        # query explorer -> use to retrieve segment ids'
        # https://ga-dev-tools.appspot.com/query-explorer/
        
        #load token and get views
        load(get_path("ga_token"))
        profiles <- tbl_df(GetProfiles(token = oauth_token))
        
        if(brand == "Prada"){
                view <- profiles %>% 
                        filter(id == "126281707")
        } else if(brand == "Miu Miu" & use_miumiu_mirror){
                view <- profiles %>% 
                        filter(id == "158198438")
        } else {
                view <- profiles %>% 
                        filter(id == "126514406")
        }
        
        message(paste0("Getting data for view: ",view$name))
        
        
        if(is.null(segments) & is.null(filters)){
                
                query <- Init(start.date = as.character(start_date),
                              end.date = as.character(end_date),
                              dimensions = dimensions,
                              metrics = metrics,
                              max.results = 10000,
                              table.id = paste0("ga:",view$id))
                
        } else if(!is.null(segments) & is.null(filters)){
                
                query <- Init(start.date = as.character(start_date),
                              end.date = as.character(end_date),
                              dimensions = dimensions,
                              metrics = metrics,
                              segments = segments,
                              max.results = 10000,
                              table.id = paste0("ga:",view$id))
                
        } else if(is.null(segments) & !is.null(filters)){
                
                query <- Init(start.date = as.character(start_date),
                              end.date = as.character(end_date),
                              dimensions = dimensions,
                              metrics = metrics,
                              filters = filters,
                              max.results = 10000,
                              table.id = paste0("ga:",view$id))
                
        } else {
                query <- Init(start.date = as.character(start_date),
                              end.date = as.character(end_date),
                              dimensions = dimensions,
                              metrics = metrics,
                              segments = segments,
                              filters = filters,
                              max.results = 10000,
                              table.id = paste0("ga:",view$id))
        }
        
        
        result <- tbl_df(GetReportData(query.builder = QueryBuilder(query), token =  oauth_token, split_daywise, paginate_query))
        
}



ga_get_grouped_traffic <- function(brand, start_date, end_date, segment_id, split_daywise){
        
        #  get traffic from social but logins
        visits <- ga_get_data(start_date = start_date, 
                              end_date = end_date, 
                              brand = brand,
                              dimensions = "ga:year,ga:isoWeek,ga:date,ga:country,ga:source,ga:medium,ga:campaign", 
                              metrics = "ga:sessions,ga:transactions,ga:bounces,ga:newUsers,ga:pageviews", 
                              segments = segment_id,
                              filters = "ga:landingPagePath!@SocialSignIn",
                              split_daywise = split_daywise) %>% 
                mutate(brand = brand, landingPagePath = "not social")
        
        #  get traffic from social logins (after 5-5-2017 for Prada, after 29-12-2017 for miu miu)
        if(brand == "Prada"){
                temp_start_date <- max(start_date,ymd("20170505"))
        } else {
                temp_start_date <- max(start_date,ymd("20171229"))
        }
        
        if(temp_start_date < end_date){
                visits <- ga_get_data(start_date = temp_start_date, 
                                      end_date = end_date, 
                                      brand = brand,
                                      dimensions = "ga:year,ga:isoWeek,ga:date,ga:country,ga:source,ga:medium,ga:campaign",
                                      metrics = "ga:sessions,ga:transactions,ga:bounces,ga:newUsers,ga:pageviews", 
                                      segments = segment_id,
                                      filters = "ga:landingPagePath=@SocialSignIn",
                                      split_daywise = F) %>% 
                        mutate(brand = brand,
                               landingPagePath = "social") %>% 
                        bind_rows(visits)
        }
        
        # new channel grouping
        visits <- visits %>% 
                mutate(custom_grouping = case_when(source == "(direct)" & medium == "(none)" ~ "Direct",
                                                   medium == "organic" ~ "Natural Search",
                                                   medium == "referral" & landingPagePath != "social" & campaign == "(not set)" & grepl(pattern = "(.*facebook.*)|(.*instagram.*)|(.*t\\.co$)|(.*pinterest.*)|(.*vk\\.com.*)|(.*twitter.*)|(.*youtube.*)|(^line$)", source) ~ "Referrals from socials",
                                                   medium == "referral" & landingPagePath != "social" & campaign == "(not set)" ~ "Referrals non-social",
                                                   grepl("social[-_]post",medium) & landingPagePath != "social" & campaign != "(not set)" ~ "Social Posts",
                                                   medium == "sa" | medium == "social_ad" ~ "Social Paid Campaigns",
                                                   grepl("email|mail",medium)  ~ "Email",
                                                   grepl("cpc|mse",medium) ~ "Paid Search",
                                                   grepl("display|affiliate|video|video_ad|branded_content|native",medium)  ~ "Display",
                                                   grepl(" ^(cpv|cpa|cpp|content-text)$",medium) | campaign != "(not set)" ~ "Other Campaigns",
                                                   landingPagePath == "social" & campaign == "(not set)" ~ "Social Login",
                                                   TRUE ~ "(Other)")) %>% 
                group_by(year,isoWeek,date,country,brand,custom_grouping) %>% 
                summarise_at(vars(sessions,transactions,bounces,newUsers,pageviews),sum)
        
        # renam to standard channel
        visits <- visits %>% 
                ungroup() %>% 
                rename(channelGrouping = custom_grouping)
        
        
}




ga_get_views <- function(brand, ref_day, lookback_weeks = 12, split_daywise = F){
        
        #subset to e-store countries
        segment_id <- "gaid::7xcJH6ZkRN2HQOGhoyz98A"
        
        if(brand == "Prada"){
                # modified for go live china
                segment_id <- "gaid::J09RpBPURA2XrNwnp9ih4A"
        }
        
        
        
        start_date <- ref_day-(7*lookback_weeks)+1
        
        visits <- ga_get_grouped_traffic(start_date = start_date, 
                              end_date = ref_day, 
                              brand = brand,
                              segment_id = segment_id, 
                              split_daywise = split_daywise)
        
        
        w <- isoweek(ref_day)
        w <- ifelse(w < 10,paste0("0",w),w)
        temp <-  ISOweek2date(paste0(year(ref_day)-1,"-W",w,"-7"))
        start_date <- temp-(7*lookback_weeks)+1
        
        visits <- ga_get_grouped_traffic(start_date = start_date, 
                              end_date = temp, 
                              brand = brand,
                              segment_id = segment_id, 
                              split_daywise = split_daywise) %>% 
                bind_rows(visits) %>% 
                mutate(country = case_when(country == "United States" ~ "USA",TRUE ~ country)) %>% 
                mutate(ref_day = ref_day)
        
        
}


ga_views_save_dataset <- function(){
        
        write.csv2(visits, file = get_path("ga_visits_output"), na = "", row.names = F)
        
}





# GA MOST VIEWED ---------------------------------------------------------
ga_get_most_viewed <- function(ref_day, brand, paginate_query = F, use_miumiu_mirror = F, lookback_days = 6){
        
        
        
        # ga_get_data(start_date = ref_day-lookback_days, 
        #             end_date = ref_day, 
        #             brand = brand,
        #             dimensions = "ga:pagePathLevel3,ga:pageTitle,ga:pagePathLevel4", 
        #             metrics = "ga:totalEvents",
        #             filters = "ga:eventCategory==ecommerce;ga:eventAction==detail",
        #             split_daywise = F, 
        #             paginate_query = paginate_query,
        #             use_miumiu_mirror = use_miumiu_mirror)
        
        ga_get_data(start_date = ref_day-lookback_days,
                    end_date = ref_day,
                    brand = brand,
                    dimensions = "ga:pagePathLevel3,ga:eventLabel",
                    metrics = "ga:totalEvents",
                    filters = "ga:eventCategory==ecommerce;ga:eventAction==detail",
                    split_daywise = F,
                    paginate_query = paginate_query,
                    use_miumiu_mirror = use_miumiu_mirror)
        
        
        
        
}

ga_get_most_viewed_china <- function(ref_day, brand, paginate_query = F, use_miumiu_mirror = F, lookback_days = 6){
        
        
        
        most_viewed_china <- ga_get_data(start_date = ref_day-lookback_days,
                                         end_date = ref_day,
                                         brand = brand,
                                         dimensions = "ga:pagePathLevel4",
                                         metrics = "ga:pageviews",
                                         filters = "ga:pagePath=~^www\\.prada\\.com/cn/*;ga:pagePathLevel4=~product",
                                         split_daywise = F,
                                         paginate_query = paginate_query,
                                         use_miumiu_mirror = use_miumiu_mirror)
        
        most_viewed_china <- most_viewed_china %>% 
                mutate(sku = str_extract(pagePathLevel4,"[A-Z0-9_]*\\.html$") %>% gsub("\\.html$","",.)) %>% 
                group_by(sku) %>% 
                summarise(views = sum(pageviews)) %>% 
                mutate(country_code = "CN", brand = brand) %>% 
                filter(sku != "")
        
        
        
}

most_viewed_reshape <- function(){
        
        
        most_viewed <- most_viewed %>% 
                mutate(sku = gsub("(\\?|#).*","",eventLabel)) %>% 
                group_by(pagePathLevel3,sku) %>% 
                summarise(totalEvents = sum(totalEvents)) %>% 
                filter(grepl("^[A-Z0-9]",sku)) %>% 
                ungroup()
        
        most_viewed <- most_viewed %>% 
                mutate(pagePathLevel3 = gsub("^/|/$","",pagePathLevel3)) %>% 
                mutate(country_code = toupper(str_sub(pagePathLevel3,-2))) %>% 
                mutate(brand = str_sub(pagePathLevel3,1,-3)) %>% 
                mutate(brand = case_when(brand == "miumiu" ~ "Miu Miu",brand == "prada" ~ "Prada", T ~ brand))
        
        
        
        most_viewed <<- most_viewed %>% 
                mutate(sku = gsub("-","_",sku)) %>% 
                group_by(brand,country_code,sku) %>% 
                summarise(views = sum(totalEvents)) %>% 
                arrange(desc(views))
        
        
        
}

most_viewed_enrich <- function(){
        
        prada_link <- c("https://assets.prada.com/content/dam/prada-ecommerce/images/zoom/","-1.jpg/_jcr_content/renditions/original")
        miu_link <- c("http://www.miumiu.com/assets/images/products/zooms/","-1.jpg")
        
        
        most_viewed <- most_viewed %>% 
                mutate(img_link = case_when(brand == "Prada" ~ paste0(prada_link[1],gsub("-","\\_",sku),prada_link[2]),
                                            brand == "Miu Miu" ~ paste0(miu_link[1],gsub("-","\\_",sku),miu_link[2]),
                                            TRUE ~ "Not Found")) %>% 
                mutate(ref_day = ref_day)
        
        most_viewed <<- ecommerce %>%
                select(country_code,country_text) %>%
                distinct() %>%
                inner_join(most_viewed, by = "country_code")
        
}

most_viewed_add_marketplaces <- function(){
        
        marketplaces <- read_excel(path = get_path("marketplaces"), sheet = "marketplaces", trim_ws = T)
        
        most_viewed <<- marketplaces %>% 
                inner_join(most_viewed, by = "country_text")
        
        
}

most_viewed_stitch_images <- function(brand_par, marketplace_par, filename, top_n = 10){
        
        m <- sprintf("most_viewed_stitch_images(): Downloading and stitching images for file %s",filename)
        message(m)
        
        temp <- most_viewed %>% 
                filter(brand == brand_par & marketplace == marketplace_par)
        
        #img_temp <- image_read(temp$img_link) %>% image_scale(geometry = "200")
        img_read_safe <- possibly(.f = ~ image_read(.x) %>% image_scale(geometry = "200"), otherwise = image_read(get_path("not_found_image")) %>% image_scale(geometry = "200"))
        img_temp <- map(temp$img_link,img_read_safe)
        
        img_temp <- image_append(c(image_append(c(img_temp[[1]],img_temp[[2]],img_temp[[3]],img_temp[[4]]),stack = F),
                                   image_append(c(img_temp[[5]],img_temp[[6]],img_temp[[7]],img_temp[[8]]),stack = F),
                                   image_append(c(img_temp[[9]],img_temp[[10]],img_temp[[11]],img_temp[[top_n]]),stack = F)),
                                 stack = T)
        image_write(img_temp,paste0(get_path("most_viewed_path"),filename),format = "jpg", quality = 100)
        
        
        
}

most_viewed_get_images <- function(top_n = 10){
        
        #setup
        file.remove(list.files(get_path("most_viewed_path"), full.names = T))
        
        most_viewed <<- most_viewed %>% 
                group_by(ref_day,brand,marketplace,sku,img_link) %>% 
                summarise(views = sum(views)) %>% 
                arrange(brand,marketplace,desc(views)) %>% 
                group_by(brand,marketplace) %>% 
                slice(1:top_n) %>% 
                mutate(rank = row_number()) %>% 
                ungroup() %>% 
                mutate(filename = tolower(paste0(brand,"_",marketplace))) %>% 
                mutate(filename = paste0(gsub(" ","",filename),".jpg"))
        
        
        temp <- most_viewed %>% 
                select(brand,marketplace,filename) %>% 
                distinct()
        
        
        temp <- pmap(list(temp$brand, temp$marketplace, temp$filename, top_n), .f = most_viewed_stitch_images)
        
        
}

most_viewed_save_dataset <- function(){
        
        write.csv2(most_viewed, file = get_path("most_viewed_output"), na = "", row.names = F)
        
}




# GA OUT OF STOCK ---------------------------------------------------------
ga_get_out_of_stock <- function(ref_day, brand, paginate_query = F, use_miumiu_mirror = F, lookback_days = 7){
        
        
        # data on out of stock are reliable from 2017-10-25
        oos_data <- ga_get_data(start_date = max(ref_day - lookback_days + 1, ymd("20171025")), 
                                end_date = ref_day, 
                                brand = brand,
                                dimensions = "ga:date,ga:pagePathLevel1,ga:pagePathLevel3,ga:eventAction,ga:eventLabel", 
                                metrics = "ga:totalEvents",
                                filters = "ga:eventCategory==product_available",
                                split_daywise = F, 
                                paginate_query = paginate_query,
                                use_miumiu_mirror = use_miumiu_mirror) %>% 
                filter(grepl("^is",eventAction))
        
        #subset to e-store pages
        oos_data <- oos_data %>% 
                filter(grepl("miumiu|prada",pagePathLevel3)) %>% 
                mutate(brand = if_else(grepl("prada",pagePathLevel3),"Prada","Miu Miu"))
        
        #extract country and add ref_day and link
        oos_data <- oos_data %>% 
                mutate(country_store = str_extract(pattern = "../$",pagePathLevel3) %>% gsub("/","",.) %>% toupper()) %>% 
                mutate(ref_day = ref_day) %>% 
                mutate(link = paste0(pagePathLevel1,"en",pagePathLevel3,eventLabel)) %>% 
                mutate(link = paste0("https://",gsub("//","/",link)))
        
        
}

ga_out_of_stock_enrich <- function(){
        
        temp <- ecommerce %>% 
                mutate(key = str_sub(article_style_code,1,3)) %>% 
                select(classe_new,key) %>% 
                group_by(key) %>% 
                summarise(classe_new = first(classe_new))
        
        out_of_stock <<- out_of_stock %>% 
                mutate(key = str_sub(eventLabel,1,3)) %>% 
                left_join(temp, by = "key")
        
}

out_of_stock_save_dataset <- function(){
        
        write.csv2(out_of_stock, file = get_path("out_of_stock_output"), na = "", row.names = F)
        
}





# ANOMALY DETECTION -------------------------------------------------------
get_cdf_kde <- function(vals){
        
        distr <- tail(vals,-1)
        val <- tail(vals,1)
        if(length(distr) > 1){
                pdf <- density(distr)
                
                tibble(distr = c(pdf$x, val), cdf = c(cumsum(pdf$y)/sum(pdf$y),NA)) %>%  
                        arrange(distr) %>% 
                        mutate(cdf = case_when(is.na(cdf) & !is.na(lag(cdf)) ~ lag(cdf),
                                               is.na(cdf) ~ lead(cdf),
                                               TRUE ~ cdf)) %>% 
                        filter(distr == val) %>% 
                        pull(cdf)
        } else {NA}
        
        
        
}

make_weekly_data <- function(){
        weekly <- ecommerce %>% 
                mutate(brand_text = case_when(brand_code == "PS" ~ "Prada",TRUE ~ brand_text)) %>% 
                filter(brand_text %in% c("Prada","Miu Miu")) %>% 
                mutate(isoweek = ISOweek(date)) %>% 
                group_by(brand_text,country_text,isoweek) %>% 
                summarise_at(vars(qty_tot,val_tot_eur),sum)
        
        weekly <<- cross_df(list(isoweek = unique(weekly$isoweek), 
                                 brand_text = unique(weekly$brand_text), 
                                 country_text = unique(weekly$country_text))) %>% 
                arrange(isoweek) %>% 
                left_join(weekly, by = c("isoweek", "brand_text", "country_text")) %>% 
                replace_na(list(qty_tot = 0, val_tot_eur = 0)) %>% 
                arrange(brand_text, country_text, isoweek)
        
}

low_hi_sales_anomalies <- function(ref_day){
        
        kde_based <- weekly %>% 
                group_by(brand_text, country_text) %>% 
                arrange(brand_text,country_text,isoweek) %>% 
                summarise_at(vars(qty_tot,val_tot_eur),get_cdf_kde) %>% 
                mutate(value_anomaly = case_when(val_tot_eur < .15 ~ "LW value lower than expected", val_tot_eur > .85 ~ "LW value higher than expected", TRUE ~ "regular")) %>% 
                mutate(qty_anomaly = case_when(qty_tot < .15 ~ "LW qty lower than expected", qty_tot > .85 ~ "LW qty higher than expected", TRUE ~ "regular")) %>% 
                filter(value_anomaly ==  "LW value lower than expected" | qty_anomaly == "LW qty lower than expected") %>% 
                mutate(ref_day = ref_day) %>% 
                ungroup() %>% 
                arrange(brand_text,desc(value_anomaly),desc(qty_anomaly))
        
        write_csv(kde_based, path = get_path("performance_anomalies_output"), na = "")
        
}

no_sales_anomalies <- function(ref_day){
        
        lw_no_sales <- weekly %>% 
                filter(isoweek == ISOweek(ref_day)) %>% 
                filter(qty_tot == 0) %>% 
                select(brand_text,country_text) %>% 
                distinct() %>% 
                inner_join(weekly, by = c("brand_text", "country_text")) %>% 
                mutate(no_sales = qty_tot == 0)
        
        lw_no_sales <- lw_no_sales %>% 
                unite("ix", c(brand_text,country_text), remove = F) 
        lw_no_sales <- split(x = lw_no_sales,f = lw_no_sales$ix)
        lw_no_sales <- map_df(lw_no_sales, ~ tibble(lengths = rle(.x$no_sales)$lengths, 
                                                    value = rle(.x$no_sales)$values) %>% 
                                      filter(value == T) %>% 
                                      mutate(no_sales_streak = row_number()), .id = "ix"
        ) %>% 
                separate(ix, into = c("brand_text","country_text"), sep = "_")
        
        
        lw_no_sales <- lw_no_sales %>% 
                group_by(brand_text,country_text) %>% 
                summarise(pval = get_cdf_kde(lengths)) %>% 
                filter(!is.na(pval) & pval > .9) %>% 
                mutate(ref_day = ref_day)
        write_csv(lw_no_sales, path = get_path("no_sales_anomalies_output"), na = "")
        
        
        
}


price_anomalies <- function(ref_day){
        
        
        regular_price_anomalies <- ecommerce %>% 
                group_by(isoyear,isoweek,sku_eqv_id_std,country_code,brand_text) %>% 
                summarise_at(vars(val_reg_eur,qty_reg), sum) %>% 
                filter(qty_reg != 0) %>% 
                arrange(sku_eqv_id_std,country_code,isoyear,isoweek) %>% 
                group_by(sku_eqv_id_std,country_code) %>% 
                mutate(avg_price = val_reg_eur/qty_reg, ix = row_number()) %>% 
                mutate(delta_price = avg_price/lag(avg_price)-1) %>% 
                ungroup() %>% 
                filter(isoyear == isoyear(ref_day) & isoweek == isoweek(ref_day) & !is.na(delta_price) & delta_price != 0) %>% 
                mutate(ref_day = ref_day)
        
        write_csv(regular_price_anomalies, path = get_path("regular_price_anomalies_output"), na = "")
        
        if(nrow(regular_price_anomalies) > 0){
                m <- sprintf("WARNING!! found %s price anomalies, check file at %s",nrow(regular_price_anomalies), get_path("regular_price_anomalies_output"))
                message(m)
        }
        
        
}


