library(rJava)
library(Rwordseg)
library(tm)
library(pacman)
library(BiocManager)
p_load(tidyverse,tidytext,data.table,rio,jiebaR)
p_load(cidian)

install.packages("devtools")  
install.packages("stringi")  
install.packages("pbapply")  
install.packages("Rcpp")  
install.packages("RcppProgress")  
library(devtools)  
install_github("qinwf/cidian")  


jd=read.csv("G:/jd_filter1.csv",fileEncoding = "utf-8")

#jd2=jd[!duplicated(jd$remark),]
#write.csv(jd2,"G:/jd2.csv",fileEncoding = "utf-8")
phone_info=read.csv("G:/phone_info2.csv",fileEncoding = "utf-8")
#keywords_cut=read.csv("G:/keywords_cut.csv")



jd$remark=as.character(jd$remark)
jd_cut1= filter(jd, remark !='&hellip' & remark != '2'& remark != '4'& remark != '首先'& remark != '说实话'& 
                  remark != '0'& remark != '嗯'& remark != '唉'& remark != '3'& remark != '5'& remark != '6'& 
                  remark != '但是'& remark != '总而言之'& remark != '总之'& remark != '哎'& remark != '手机'& 
                  remark != '短信'& remark != '的'& remark != '正在研究'& remark != '送的耳机'& remark != '电池')
rm(jd,gd_cut1)
gc()

jd_cut2=left_join(jd_cut1,jd_info,by="index")
jd_cut2=jd_cut2[,-c(1,5)]
jd_cut3=jd_cut2
#jd_cut3=jd_cut3[!duplicated(jd_cut3),]

keywords_match=list()
for(i in 1:length(jd_cut3$remark)){
  for(j in 1:length(keywords_cut$keywords1)){
    if(keywords_cut$keywords1[j] %in% jd_cut3$remark[i]){
      keywords_match[i]=keywords_cut$keywords1[j]
    }
    else{keywords_match[i]=0}
  }
}


#aa <- grep('(PDCD4|NFE2L2)', in_genes$SwissProt)


#aa = grep('(屏|曲面|全面|滑盖|全|刘海|水滴|折叠|home|HOME|隐藏|失灵|透明|边框|显示|窄边|英寸|支架|防水|
          外观|金属|电动|静音|塑料|彩屏|分辨率|碎屏|玻璃|镜面|光面|摔|磨砂尺寸|颜色|无孔|发黄|后盖|缝隙|手感|掉漆|
          电池|续航|温度|充|电|待机|电量|数据线|电源|电能|发热|烫|散热|辐射|耗电|掉电|快充|
          摄像头|照片|照相机|摄影|拍摄|拍照|图片|镜头|内置|储存|像素|px|PX|闪光灯|后置|前置|夜景|拍|补光|调光|滤镜|光圈|
          信号|电话|信息|通话|
          音质|噪音|扬声器|音乐|麦|音效|声音|话筒|
          设计|用户|关注|细节|功能|
          价格|市场|省钱|便宜|
          手指|指纹|密码|人脸|面部|虹膜|解锁|解|震动
          网络|蓝牙|信号|软件|无线|病毒|系统|上网|系统|安卓|IOS|ios|模式|配置|内存|电子|芯片|
          红外|操作|通用|设备|设定|场景|技术|数据|支持|体验|智能|
          性能|状态|速度|游戏|卡顿|卡|画质|画面|加载|流畅|顺畅|
          国产手机|国货|电信|全网通|版本|处理器|传统|品牌|跑分|性价比|手感|机型)',jd_cut3$remark)

aa = grep('(屏|曲面|全面|滑盖|全|刘海|水滴|折叠|home|HOME|隐藏|失灵|透明|边框|显示|窄边|英寸|支架|防水|
            外观|金属|电动|静音|塑料|彩屏|分辨率|碎屏|玻璃|镜面|光面|摔|磨砂尺寸|颜色|无孔|发黄|后盖|缝隙|手感|掉漆)',jd_cut3$remark)
pingmu <- jd_cut3[aa,]

bb = grep('(电池|续航|温度|充|电|待机|电量|数据线|电源|电能|发热|烫|散热|辐射|耗电|掉电|快充)',jd_cut3$remark)
dianchi <- jd_cut3[bb,]

cc = grep('(手指|指纹|密码|人脸|面部|虹膜|解锁|解|震动)',jd_cut3$remark)
jiesuo <- jd_cut3[cc,]



pingmu=jd[grep(pattern="屏|曲面|全面|滑盖|全|刘海|水滴|折叠|home|HOME|隐藏|失灵|透明|边框|显示|窄边|英寸|支架|防水|
          外观|金属|电动|静音|塑料|彩屏|分辨率|碎屏|玻璃|镜面|光面|摔|磨砂尺寸|颜色|无孔|发黄|后盖|缝隙|手感|掉漆",jd[,2]),]

n=grep(pattern="解锁",pingmu$remark)
m=grep(pattern="指纹识别",pingmu$remark)
all=c(n,m)
pingmu=pingmu[-all,]


jingtou=jd[grep(pattern="摄像头|照片|照相机|摄影|拍摄|拍照|图片|镜头|内置|储存|像素|px|PX|闪光灯|后置|前置|夜景|拍|补光|调光|滤镜|光圈|凸|突出",jd[,2]),]

n=grep(pattern="慎拍",jingtou$remark)
jingtou=jingtou[-n,]
jingtou$remark=as.character(jingtou$remark)
jingtou$tag=nchar(jingtou[,2])
jingtou=subset(jingtou,tag>2)


xitong=jd[grep(pattern="网络|蓝牙|信号|软件|无线|病毒|系统|上网|系统|安卓|IOS|ios|模式|配置|内存|电子|芯片|
性能|状态|速度|游戏|卡顿|卡|画质|画面|加载|流畅|顺畅|处理器|跑分|Android|miui|MIUI|EMUI|emui",jd[,2]),]

n=grep(pattern="慎拍",jingtou$remark)
jingtou=jingtou[-n,]
jingtou$remark=as.character(jingtou$remark)
jingtou$tag=nchar(jingtou[,2])
jingtou=subset(jingtou,tag>2)




info2$phone_name=gsub("，", " ", info$phone_name)
phone_info2=left_join(info2,phone_info,by="phone_name")
write.csv(phone_info2,"G:/phone_info79.csv")