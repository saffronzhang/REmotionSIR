
library(rwda)
access_token = "2.00iHo7dG0AYv8D83ed3fae78AgQt4D"
weiboid = "4050526184658139"
df_comments = get_comments(access_token, weiboid, maxpage = 20)
weibo_cloud(df_comments, stopwords = c("心心", "回复"))
gender_vioplot(df_comments, title_text = "微博用户性别与等级分布"
               verify_vioplot(df_comments, title_text = "微博用户认证与等级分布")
               