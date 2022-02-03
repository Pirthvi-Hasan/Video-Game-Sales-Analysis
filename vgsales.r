library(dplyr)
library(ggplot2)
library(RColorBrewer)

file_check <- function() {
    if (file.exists('/Data.csv') == FALSE ){
        vg_data = read.csv('/vgsales.csv')
        vg_data = filter(vg_data ,Year != 'N/A')
        print(unique(vg_data$Year))
        vg_data = arrange(vg_data,Year)
        vg_data = subset(vg_data , select = -Rank)
        write.csv(vg_data,'/Data.csv')
    }
}

graph_plot <- function(vg_data) {
    global_sales <- c()
    genre <- c('Action','Adventure','Fighting','Misc','Platform','Puzzle','Racing','Shooter','Role-Playing','Simulation','Sports','Strategy')
    genre_col <- select(vg_data , matches("Genre"))
    glo_col <- select(vg_data , matches("Global_Sales"))
    g_data <- cbind(genre_col , glo_col)
    for ( i in genre) {
        data <- filter(g_data , Genre == i)
        global_sales <- append(global_sales , sum(data[,2]))
    }
    dat <- data.frame(genre, global_sales)
    ggplot(dat, aes(x=genre, y=global_sales)) + geom_bar(stat="identity", fill="steelblue") + ggtitle("Video Game Sales") + xlab("Genre") + ylab("Global Sales")
}

series_plot <- function(vg_data) {
    plot_data <- filter(vg_data ,Genre == 'Action' )
    plot_data$Name <- plot_data$Platform <- plot_data$Publisher <- NULL
    year <- unique(vg_data$Year)
    year <- sort(year , decreasing = FALSE)
    na_sales <- c()
    eu_sales <- c()
    jp_sales <- c()
    other_sales <-c()
    global_sales <- c()
    for (i in year) {
        meta <- filter(plot_data ,Year == i)
        na_sales <- append(na_sales , sum(meta[,4]))
        eu_sales <- append(eu_sales , sum(meta[,5]))
        jp_sales <- append(jp_sales , sum(meta[,6]))
        other_sales <- append(other_sales , sum(meta[,7]))
        global_sales <- append(global_sales , sum(meta[,8]))
    }
    par(mfrow = c(3,2))
    plot(x=year, y=na_sales ,type='l',xlim=c(min(year),max(year)) ,ylim=c(min(na_sales),max(na_sales))  ,xlab='Year' ,ylab='Sales' ,main='NA Sales Action Genre' ,col='dodgerblue3')
    plot(x=year, y=eu_sales ,type='l',xlim=c(min(year),max(year)) ,ylim=c(min(eu_sales),max(eu_sales))  ,xlab='Year' ,ylab='Sales' ,main='EU Sales Action Genre' ,col='dodgerblue3')
    plot(x=year, y=jp_sales ,type='l',xlim=c(min(year),max(year)) ,ylim=c(min(jp_sales),max(jp_sales))  ,xlab='Year' ,ylab='Sales' ,main='JP Sales Action Genre' ,col='dodgerblue3')
    plot(x=year, y=other_sales ,type='l' ,xlim=c(min(year),max(year)) ,ylim=c(min(other_sales),max(other_sales)) ,xlab='Year' ,ylab='Sales' ,main='Other Sales Action Genre' ,col='dodgerblue3')
    plot(x=year, y=global_sales ,type='l' ,xlim=c(min(year),max(year)) ,ylim=c(min(global_sales),max(global_sales)) ,xlab='Year' ,ylab='Sales' ,main='Global Sales Action Genre' ,col='dodgerblue3')
    plot.ts(x=year, y=global_sales ,type='l' ,xlim=c(min(year),max(year)) ,ylim=c(min(global_sales),max(global_sales)) ,xlab='Year' ,ylab='Sales' ,main='Global Sales Action Genre' ,col='dodgerblue3')
}

box_plot <- function(vg_data) {
    action <- filter(vg_data, Genre=='Action')
    rows <- nrow(action)
    sales  <- c()
    values <- c()
    for(i in 7:10) {
        for(j in 1:rows) {
            values[(i-7)*rows+j] <- action[j,i]
            if(i==7) sales[(i-7)*rows+j] = 'na_sales'
            if(i==8) sales[(i-7)*rows+j] = 'eu_sales'
            if(i==9) sales[(i-7)*rows+j] = 'jp_sales'
            if(i==10) sales[(i-7)*rows+j] = 'other_sales'
        }
    }
    result <- data.frame(sales,values)
    result <- mutate(result , treatment = factor(sales,levels = unique(sales)))
    boxplot(values~treatment,data=result,col="dodgerblue3",ylab = "Sales", xlab = "Years")
}

oneway_anova <- function(vg_data) {
    action <- filter(vg_data, Genre=='Action')
    rows <- nrow(action)
    sales  <- c()
    values <- c()
    for(i in 7:10) {
        for(j in 1:rows) {
            values[(i-7)*rows+j] <- action[j,i]
            if(i==7) sales[(i-7)*rows+j] = 'na_sales'
            if(i==8) sales[(i-7)*rows+j] = 'eu_sales'
            if(i==9) sales[(i-7)*rows+j] = 'jp_sales'
            if(i==10) sales[(i-7)*rows+j] = 'other_sales'
        }
    }
    result <- data.frame(sales,values)
    result <- mutate(result , treatment = factor(sales,levels = unique(sales)))
    glimpse(result)
    cat('\n','ANOVA ~ ANALYSIS','\n')
    anova <- aov(values~treatment , data = result)
    print(summary(anova))
    cat('\n','PAIR ~ TEST')
    print(TukeyHSD(anova))
    plot(TukeyHSD(anova))
}

stack_plot <- function(vg_data) {
    platforms <- c('PS3','X360','PS2','N64','NES','XOne','PS4','PSP','GBA','PC')
    ps3 <- c()
    x360 <- c()
    ps2 <- c()
    n64 <- c()
    nes <- c()
    xone <- c()
    ps4 <- c()
    psp <- c()
    gba <- c()
    pc <- c()
    genre <- c('Action','Adventure','Fighting','Misc','Platform','Puzzle','Racing','Shooter','Role-Playing','Simulation','Sports','Strategy')
    for (i in platforms) {
        for (j in genre) {
            data <- filter(vg_data ,Genre==j ,Platform==i)
            if(i == 'PS3') ps3 <- append(ps3,nrow(data))
            if(i == 'X360') x360 <- append(x360,nrow(data))
            if(i == 'PS2') ps2 <- append(ps2,nrow(data))
            if(i == 'N64') n64 <- append(n64,nrow(data))
            if(i == 'NES') nes <- append(nes,nrow(data))
            if(i == 'XOne') xone <- append(xone,nrow(data))
            if(i == 'PS4') ps4 <- append(ps4,nrow(data))
            if(i == 'PSP') psp <- append(psp,nrow(data))
            if(i == 'GBA') gba <- append(gba,nrow(data))
            if(i == 'PC') pc <- append(pc,nrow(data))
         }
    }
    cols = brewer.pal(n=12 ,name="Paired")
    value <- c(ps3,x360,ps2,n64,nes,xone,ps4,psp,gba,pc)
    Platform <- c()
    for(i in platforms) {
        Platform <- append(Platform,rep(i,length(genre)))
    }
    Genre <- rep(genre, length(platforms))
    data <- matrix(value, nrow=12, byrow=TRUE, dimnames=list(genre ,platforms))
    barplot(data ,col=cols ,main="Sales by Platfrom" ,xlab="Platfroms" ,ylab="Sales" ,las=1 ,names=platforms)
    legend("topright" ,legend=genre ,fill=cols ,ncol=2 ,cex=0.5)
}

before_plot <- function(vg_data) {
    years <- c(1980:2016)
    sales <- c()
    for(yr in years) {
        data <- filter(vg_data ,Year==toString(yr))
        sales <- append(sales ,sum(data$Global_Sales))
    }
    plot(x = years , y = sales , main = 'VG~SALES',xlab = 'Years' , ylab = 'Sales', col = 'dodgerblue3' , type = 'o')
}

forecast <- function(vgdata) {
    years <- c(1980:2016)
    sales <- c()
    for(yr in years) {
        data <- filter(vg_data ,Year==toString(yr))
        sales <- append(sales ,sum(data$Global_Sales))
    }
    relation <- lm(sales~years)
    print(relation)
    result <- c()
    for(yr in 2017:2020) {
        df <- data.frame(years=yr)
        result <- append(result,predict(relation,df))
    }
    print(result)
    years <- append(years,c(2017:2020))
    sales <- append(sales,result) 
    plot(years ,sales ,col="dodgerblue3" ,main="Sales & Years Regression" ,cex=0.8 , pch = 9 ,xlab="Years",ylab="Sales in Millions",type = 'o')
}


file_check()
vg_data <- read.csv('/Data.csv' , sep = ',' , header = TRUE)
str(vg_data)
graph_plot(vg_data)
series_plot(vg_data)
box_plot(vg_data)
oneway_anova(vg_data)
stack_plot(vg_data)
before_plot(vg_data)
forecast(vg_data)
