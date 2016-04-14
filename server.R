
library(shiny)
library(ggplot2)
library(RCurl)
library(shinyapps)
library(rjson)
library(dplyr)
library(plyr)
require(rCharts)

title<-c()
pic<-c()
nick<-c()
detail_url<-c()
view_price<-c()
view_sales<-c()
page_count<-c()
new_url<-""



category_url<-c('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=&imgfile=&js=1&stats_click=search_radio_all%3A1&initiative_id=staobaoz_20150506&ie=utf8&cps=yes&fs=1&bcoffset=0&cat=50011981&ppath=21299%3A27023',
                'http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433333333674_1175&callback=jsonp1176&q=&imgfile=&js=1&stats_click=search_radio_all%3A1&initiative_id=staobaoz_20150608&ie=utf8&cps=yes&fs=1&cat=50011986&ppath=21299%3A27023&bcoffset=0',
                'http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433333500807_2380&callback=jsonp2381&q=&imgfile=&js=1&stats_click=search_radio_all%3A1&initiative_id=staobaoz_20150608&ie=utf8&cps=yes&fs=1&bcoffset=0&cat=50011987&ppath=21299%3A27023'
                
)
category_name<-c('面膜','眼部护理','胸部护理')


url<-data.frame(category_name,category_url)
url$category_name<-as.character(url$category_name)
url$category_url<-as.character(url$category_url)


myheader=c(
  "Referer"= 'http://detail.tmall.com/item.htm?spm=a220m.1000858.1000725.5.F13Qrw&id=45019289022&areaId=440300&cat_id=2&rn=4e13073a113b3447f9324cf805e30bb8&user_id=772669743&is_b=1',
  "Connection"=' keep-alive',
  "User-Agent"="Mozilla/5.0 (Windows NT 6.1; WOW64; rv:45.0) Gecko/20100101 Firefox/45.0"
)




shinyServer(function(input,output,session){
  
  
  randomVals <- eventReactive(input$go, {
    
    
    if(input$Mode == '按类目展现'){
      
      if(input$displaydatas==1){
        url<-filter(.data=url,category_name==input$Product_Categorys)$category_url
        m<-1
        cur<-input$displaydatas
        
      }else{
        url<-filter(.data=url,category_name==input$Product_Categorys)$category_url
        m<-input$displaydatas-1
        cur<-input$displaydatas
        n<-paste("data-value=",m*44,sep = "")
        url<-gsub('data-value=[^&]',n,url)    
        
      }
    }
    if(input$Mode == '按关键词搜索'){
      if(input$displaydatas==1){
        
        q=input$key
        url<-paste('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=',q,sep="")
        m<-1
        cur<-input$displaydatas
        
      }else{
        q=input$key
        url<-paste('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=',q,sep="")
        m<-input$displaydatas-1
        cur<-input$displaydatas
        n<-paste("data-value=",m*44,sep = "")
        url<-gsub('data-value=[^&]',n,url)    
        
      }
      
    }
    
    
    
    
    
    
    if(input$Tmall){
      url<-paste(url,"&seller_type=tmall",sep="")
    }
    
    url<-paste(url,"&sort=",input$sortby,sep = "")
    
    
    
    
    webpage<-getURL(url,httpheader=myheader)
    a<-gregexpr('\\{.*\\}',webpage) 
    page<-substr(webpage,a[[1]],a[[1]]+attr(a[[1]],'match.length')-1)
    newdata<-fromJSON(page,unexpected.escape = 'skip',method = 'C')
    
    page_count<-newdata$mods$pager$data$totalPage
    
    
    
    
    
    for(i in 1:43){
      
      title<-append(title,newdata$mods$itemlist$data$auctions[[i]]$title)
      pic<-append(pic,paste('http:',newdata$mods$itemlist$data$auctions[[i]]$pic_url,sep = ""))
      detail_url<-append(detail_url,newdata$mods$itemlist$data$auctions[[i]]$detail_url)
      view_price<-append(view_price,newdata$mods$itemlist$data$auctions[[i]]$view_price)
      nick<-append(nick,newdata$mods$itemlist$data$auctions[[i]]$nick)
      view_sales<-append(view_sales,newdata$mods$itemlist$data$auctions[[i]]$view_sales)
      
    } 
    newdata<-data.frame(pic,title,nick,detail_url,view_price,view_sales)
    
    
    
    
    updateNumericInput(session = session,inputId = 'displaydatas',label = paste('宝贝页数',cur,'/',page_count),
                       min = 1,max = page_count)
    
    
    
    return(newdata)
    
  })    
  
  
  
  
  
  
  randomVals2 <- eventReactive(input$go2, {
    
    
    if(input$Mode == '按类目展现'){
      
      if(input$displaydatasC==1){
        url<-filter(.data=url,category_name==input$Product_Categorys)$category_url
        cur<-input$displaydatasC
        m<-1
        new_url<-append(new_url,url)
        
      }else{
        url<-filter(.data=url,category_name==input$Product_Categorys)$category_url
        cur<-input$displaydatasC
        m<-input$displaydatasC-1
        for(i in 0:m){
          n<-paste("data-value=",i*44,sep = "")
          a<-gsub('data-value=[^&]',n,url) 
          new_url<-append(new_url,a)
        }
        
      }
    }
    else{
      if(input$displaydatasC==1){
        cur<-input$displaydatasC
        q=input$key
        url<-paste('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=',q,sep="")
        m<-1
        new_url<-append(new_url,url)
        
      }else{
        cur<-input$displaydatasC
        q=input$key
        url<-paste('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=',q,sep="")
        m<-input$displaydatasC-1
        for(i in 0:m){
          n<-paste("data-value=",i*44,sep = "")
          a<-gsub('data-value=[^&]',n,url) 
          
          new_url<-append(new_url,a)
        }   
        
      }
      
    }
    
    
    
    if(input$Tmall){
      new_url<-paste(new_url,"&seller_type=tmall",sep="")
    }
    
    new_url<-paste(new_url,"&sort=",input$sortby,sep = "")
    
    
    new_cur<-cur+1
    for(j in 2:new_cur){
      
      
      webpage<-getURL(new_url[j],httpheader=myheader)
      a<-gregexpr('\\{.*\\}',webpage) 
      page<-substr(webpage,a[[1]],a[[1]]+attr(a[[1]],'match.length')-1)
      newdata<-fromJSON(page,unexpected.escape = 'skip',method = "C")
      page_count<-newdata$mods$pager$data$totalPage
      
      for(i in 1:43){
        
        title<-append(title,newdata$mods$itemlist$data$auctions[[i]]$raw_title)
        pic<-append(pic,paste('http:',newdata$mods$itemlist$data$auctions[[i]]$pic_url,sep = ""))
        detail_url<-append(detail_url,newdata$mods$itemlist$data$auctions[[i]]$detail_url)
        view_price<-append(view_price,newdata$mods$itemlist$data$auctions[[i]]$view_price)
        
        nick<-append(nick,newdata$mods$itemlist$data$auctions[[i]]$nick)
        view_sales<-append(view_sales,newdata$mods$itemlist$data$auctions[[i]]$view_sales)
        
        
      }
      
      view_price<-as.numeric(as.character(view_price))
      newdata<-data.frame(pic,title,nick,detail_url,view_price,view_sales) 
    }
    
    
    updateSliderInput(session = session,inputId = 'displaydatasC',label = paste('前',cur,'页的产品统计',cur,'/',page_count),
                      min = 1,max = page_count)
    
    
    
    return(newdata)
    
    
    
    
  })
  
  
  
  randomVals3 <- eventReactive(input$go3, {
    
    
    if(input$Mode == '按类目展现'){
      
      if(input$displaydatasC==1){
        url<-filter(.data=url,category_name==input$Product_Categorys)$category_url
        cur<-input$displaydatasC
        m<-1
        new_url<-append(new_url,url)
        
      }else{
        url<-filter(.data=url,category_name==input$Product_Categorys)$category_url
        cur<-input$displaydatasC
        m<-input$displaydatasC-1
        for(i in 0:m){
          n<-paste("data-value=",i*44,sep = "")
          a<-gsub('data-value=[^&]',n,url) 
          new_url<-append(new_url,a)
        }
        
      }
    }
    else{
      if(input$displaydatasC==1){
        cur<-input$displaydatasC
        q=input$key
        url<-paste('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=',q,sep="")
        m<-1
        new_url<-append(new_url,url)
        
      }else{
        cur<-input$displaydatasC
        q=input$key
        url<-paste('http://s.taobao.com/search?data-key=s&data-value=0&ajax=true&_ksTS=1433732646230_763&callback=jsonp764&q=',q,sep="")
        m<-input$displaydatasC-1
        for(i in 0:m){
          n<-paste("data-value=",i*44,sep = "")
          a<-gsub('data-value=[^&]',n,url) 
          
          new_url<-append(new_url,a)
        }   
        
      }
      
    }
    
    
    
    if(input$Tmall){
      new_url<-paste(new_url,"&seller_type=tmall",sep="")
    }
    
    new_url<-paste(new_url,"&sort=",input$sortby,sep = "")
    
    
    new_cur<-cur+1
    for(j in 2:new_cur){
      
      
      webpage<-getURL(new_url[j],httpheader=myheader)
      a<-gregexpr('\\{.*\\}',webpage) 
      page<-substr(webpage,a[[1]],a[[1]]+attr(a[[1]],'match.length')-1)
      newdata<-fromJSON(page,unexpected.escape = 'skip',method = "C")
      page_count<-newdata$mods$pager$data$totalPage
      
      for(i in 1:43){
        
        title<-append(title,newdata$mods$itemlist$data$auctions[[i]]$raw_title)
        pic<-append(pic,paste('http:',newdata$mods$itemlist$data$auctions[[i]]$pic_url,sep = ""))
        detail_url<-append(detail_url,newdata$mods$itemlist$data$auctions[[i]]$detail_url)
        view_price<-append(view_price,newdata$mods$itemlist$data$auctions[[i]]$view_price)
        
        nick<-append(nick,newdata$mods$itemlist$data$auctions[[i]]$nick)
        view_sales<-append(view_sales,newdata$mods$itemlist$data$auctions[[i]]$view_sales)
        
        
      }
      
      view_price<-as.numeric(as.character(view_price))
      newdata<-data.frame(pic,title,nick,detail_url,view_price,view_sales) 
    }
    
    
    
    
    
    updateSliderInput(session = session,inputId = 'displaydatasC',label = paste('前',cur,'页的产品统计',cur,'/',page_count),
                      min = 1,max = page_count)
    
    return(newdata)
    
    
    
    
  })
  
  
  
  
  
  
  output$mianmohtmloutput<-renderText({
    
    if (input$go == 0)
      return()
    
    
    newdata<-randomVals()
    
    
    
    paste0("
           
           
           <tr>
           <td> <img src='",newdata$pic,"' width=200px height=200px  /></td>
           <td>",newdata$title,"</td>
           <td ><a href='",newdata$detail_url,"'>shop link</td>
           <td > ",newdata$view_price,"元</td>
           <td> ",newdata$nick,"</td>
           <td > ",newdata$view_sales,"</td>
           </tr>
           
           ")
    
    
  })
  
  
  observe({
    
    if (input$go2 == 0)
      return()
    
    newdata<-randomVals2()
    newdata$pic<-NULL
    newdata$detail_url<-NULL
    output$DT<-DT::renderDataTable(newdata,caption='统计前几页数据的表格,支持搜索和排序,但销量排序暂不可用,可以使用左侧面板来排序')
    
    
    
  })
  
  
  
  output$myChart <- renderChart({
    ###statics Data
    
    
    observe({
      
      if (input$go3 == 0)
        return()
    })
    
    newdata<-randomVals3()
    
    format_sales<-as.character(newdata$view_sales)
    newdata$view_sales<-as.numeric(substr(format_sales,1,nchar(format_sales)-3))
    
    if(input$Statistics_Model=='商家宝贝计数'){
      tongji<-data.frame(table(newdata$nick)) 
      names(tongji)<-c('店铺名称','宝贝数量')
      tongji<-arrange(tongji,desc(宝贝数量))
      sum<-length(rownames(tongji))
      if(sum>20){
        tongji<-head(tongji,20)
      }else{
        tongji<-head(tongji,sum)
      }
      
      
      
      p1 <- rPlot(宝贝数量 ~ 店铺名称, data = tongji, type = 'bar')
      
      
      p1$addParams(dom = 'myChart')
      return(p1)
    }
    
    
    ### Statistics_Model
    if(input$Statistics_Model=='价格区间的销量'){
      
      sifen<-quantile(newdata$view_price)
      Q1<-filter(newdata,view_price <sifen[2])
      Q2<-filter(newdata,view_price>sifen[2] & view_price<sifen[3])
      Q3<-filter(newdata,view_price>sifen[3] & view_price<sifen[4])
      Q4<-filter(newdata,view_price>sifen[4] & view_price <sifen[5])
      
      
      Q1_count <-mean(Q1$view_sales)
      Q2_count <-mean(Q2$view_sales)
      Q3_count <-mean(Q3$view_sales)
      Q4_count <-mean(Q4$view_sales)
      
      Q_count<-c(Q1_count,Q2_count,Q3_count,Q4_count)
      x1<-paste('price in',sifen[1],'---',sifen[2],sep="")
      x2<-paste('price in',sifen[2],'---',sifen[3],sep="")
      x3<-paste('price in',sifen[3],'---',sifen[4],sep="")
      x4<-paste('price in',sifen[4],'---',sifen[5],sep="")
      Q_detail<-c(x1,x2,x3,x4)
      
      newdata_count<-data.frame(Q_count,Q_detail)
      
      names(newdata_count)<-c('平均销量','价格范围')
      
      
      p1 <- rPlot(平均销量 ~ 价格范围, data = newdata_count, type = 'bar')
      
      p1$addParams(dom = 'myChart')
      
      return(p1)
      
    }
    
    if(input$Statistics_Model=='商家宝贝平均价格'){
      
      shop_price<-data.frame(as.factor(tapply(newdata$view_price,newdata$nick,mean)))
      shopname<-rownames(shop_price)
      shop_price<-data.frame(shopname,shop_price)
      names(shop_price)<-c('店铺名称','平均价格')
      
      p1 <- rPlot(平均价格 ~ 店铺名称, data = shop_price, type = 'bar')
      
      p1$addParams(dom = 'myChart')
      
      return(p1)
      
    }
  })
  
})

