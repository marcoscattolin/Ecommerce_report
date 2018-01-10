
# get_dam_link_from_sku <- function(brand, sku){
#         
#         link <- NA
#         
#         prada_link <- c("https://assets.prada.com/content/dam/prada-ecommerce/images/zoom/","-1.jpg/_jcr_content/renditions/original")
#         miu_link <- c("http://www.miumiu.com/assets/images/products/zooms/","-1.jpg")
#         
#         if(grepl(pattern = "Prada",brand, ignore.case = T)){
#                 link <- paste0(prada_link[1],gsub("-","\\_",sku),prada_link[2])
#         } else if(grepl(pattern = "Miu Miu",brand, ignore.case = T)){
#                 link <- paste0(miu_link[1],gsub("-","\\_",sku),miu_link[2])
#         } else {
#                link 
#         }
#         
#       link  
# }




build_sku_id <- function(model, type_variant, variant, material, color, sep ="-"){
        
        stop("Deprecated, use function 'sku_id_std' instead of 'build_sku_id'")
        
        
}



custom_sku_id <- function(item1, item2, item3, item4, item5, sep ="-"){
        
        stop("Deprecated, use function 'sku_id_custom' instead of 'custom_sku_id'")
        
        
        
}





sku_id_std <- function(model, type_variant, variant, material, color, sep ="-"){
        
        model <- ifelse(is.na(model),"",paste0(model,sep))
        type_variant <- ifelse(is.na(type_variant),"",paste0(type_variant,sep))
        variant <- ifelse(is.na(variant),"",paste0(variant,sep))
        material <- ifelse(is.na(material),"",paste0(material,sep))
        color <- ifelse(is.na(color),"",paste0(color,sep))
        
        res <- paste0(model, type_variant, variant, material, color)
        gsub("[^a-z0-9]$","",res,ignore.case = T)
        
        
}




sku_id_custom <- function(item1, item2, item3, item4, item5, sep ="-"){
        
        item1 <- ifelse(is.na(item1),"",paste0(item1,sep))
        item2 <- ifelse(is.na(item2),"",paste0(item2,sep))
        item3 <- ifelse(is.na(item3),"",paste0(item3,sep))
        item4 <- ifelse(is.na(item4),"",paste0(item4,sep))
        item5 <- ifelse(is.na(item5),"",paste0(item5,sep))
        
        res <- paste0(item1, item2, item3, item4, item5)
        gsub("[^a-z0-9]$","",res,ignore.case = T)
        
        
}



sku_edit_std <- function(model, type_variant, variant, material, sep ="-"){
        
        model <- ifelse(is.na(model),"",paste0(model,sep))
        type_variant <- ifelse(is.na(type_variant),"",paste0(type_variant,sep))
        variant <- ifelse(is.na(variant),"",paste0(variant,sep))
        material <- ifelse(is.na(material),"",paste0(material,sep))
        
        res <- paste0(model, type_variant, variant, material)
        gsub("[^a-z0-9]$","",res,ignore.case = T)
        
        
}




sku_edit_custom <- function(item1, item2, item3, item4, sep ="-"){
        
        item1 <- ifelse(is.na(item1),"",paste0(item1,sep))
        item2 <- ifelse(is.na(item2),"",paste0(item2,sep))
        item3 <- ifelse(is.na(item3),"",paste0(item3,sep))
        item4 <- ifelse(is.na(item4),"",paste0(item4,sep))
        
        res <- paste0(item1, item2, item3, item4)
        gsub("[^a-z0-9]$","",res,ignore.case = T)
        
        
}

