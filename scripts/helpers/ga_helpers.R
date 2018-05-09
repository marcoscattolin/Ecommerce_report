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



# GET TRAFFIC -------------------------------------------------------------
ga_get_views <- function(brand, start_date, end_date, split_daywise = F){
        
        #subset to e-store countries
        segment_id <- "gaid::7xcJH6ZkRN2HQOGhoyz98A"
        
        if(brand == "Prada"){
                # modified for go live china
                segment_id <- "gaid::J09RpBPURA2XrNwnp9ih4A"
        }
        
        
        
        #  get traffic from non social logins
        all <- ga_get_data(start_date = start_date, 
                              end_date = end_date, 
                              brand = brand,
                              dimensions = "ga:date,ga:country,ga:source,ga:medium,ga:campaign", 
                              metrics = "ga:sessions,ga:transactions,ga:bounces,ga:newUsers,ga:pageviews", 
                              segments = segment_id, 
                              filters = "ga:landingPagePath!@SocialSignIn",
                              split_daywise = T) %>% 
                mutate(brand = brand,
                       landingPagePath = "not social")
        
        
        #  get traffic from social logins (after 5-5-2017 for Prada, after 29-12-2017 for miu miu)
        start_date <- max(start_date,ymd("20170505"))
        end_date <- max(start_date,end_date)
        if(brand != "Prada"){
                start_date <- max(start_date,ymd("20171229"))
                end_date <- max(start_date,end_date)
        }
        
        all <- ga_get_data(start_date = start_date,
                           end_date = end_date,
                           brand = brand,
                           dimensions = "ga:date,ga:country,ga:source,ga:medium,ga:campaign", 
                           metrics = "ga:sessions,ga:transactions,ga:bounces,ga:newUsers,ga:pageviews", 
                           segments = segment_id, 
                           filters = "ga:landingPagePath=@SocialSignIn",
                           split_daywise = F) %>% 
                mutate(brand = brand,
                       landingPagePath = "social") %>% 
                bind_rows(all)
        
        
        
        
        # attribute traffic to custom channels
        all <- all %>% 
                mutate(custom_gouping = case_when(source == "(direct)" & medium == "(none)" ~ "Direct",
                                                  medium == "organic" ~ "Natural Search",
                                                  medium == "referral" & landingPagePath != "social" & campaign == "(not set)" & grepl(pattern = "(.*baidu.*)|(.*facebook.*)|(.*instagram.*)|(.*t\\.co$)|(.*pinterest.*)|(.*vk\\.com.*)|(.*twitter.*)|(.*youtube.*)|(^line$)", source) ~ "Referrals from socials",
                                                  medium == "referral" & landingPagePath != "social" & campaign == "(not set)" ~ "Referrals non-social",
                                                  grepl("social[-_]post",medium) & landingPagePath != "social" & campaign != "(not set)" ~ "Social Posts",
                                                  medium == "sa" | medium == "social_ad" ~ "Social Paid Campaigns",
                                                  grepl("email|mail",medium)  ~ "Email",
                                                  grepl("cpc|mse",medium) ~ "Paid Search",
                                                  grepl("display|affiliate|video|video_ad|branded_content|native|programmatic",medium)  ~ "Display",
                                                  grepl(" ^(cpv|cpa|cpp|content-text)$",medium) | campaign != "(not set)" ~ "Other Campaigns",
                                                  landingPagePath == "social" & campaign == "(not set)" ~ "Social Login",
                                                  TRUE ~ "(Other)")) %>% 
                group_by(date,country,brand,custom_gouping) %>% 
                summarise_at(vars(sessions,transactions,bounces,newUsers,pageviews),sum)
        
        # format country USA for compatibility with oter reports
        all <- all %>% 
                ungroup() %>% 
                mutate(country = case_when(country == "United States" ~ "USA",
                                           TRUE ~ country))
        
        
}



# GA MOST VIEWED ---------------------------------------------------------
ga_get_most_viewed <- function(end_date, brand, paginate_query = F, use_miumiu_mirror = F, lookback_days = 6){
        

        
        ga_get_data(start_date = end_date-lookback_days,
                    end_date = end_date,
                    brand = brand,
                    dimensions = "ga:pagePathLevel3,ga:eventLabel",
                    metrics = "ga:totalEvents",
                    filters = "ga:eventCategory==ecommerce;ga:eventAction==detail",
                    split_daywise = F,
                    paginate_query = paginate_query,
                    use_miumiu_mirror = use_miumiu_mirror)
        
        
        
        
}


ga_get_most_viewed_china <- function(end_date, brand, paginate_query = F, use_miumiu_mirror = F, lookback_days = 6){
        
        
        
        most_viewed_china <- ga_get_data(start_date = end_date-lookback_days,
                                         end_date = end_date,
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
