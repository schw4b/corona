library(rsconnect)

# Set the account info for deployment.
write(format(Sys.time(), "%a %b %d %H:%M"), file=file.path('lastupdate'))
setAccountInfo(name   = Sys.getenv("shinyapps_name"),
               token  = Sys.getenv("shinyapps_token"),
               secret = Sys.getenv("shinyapps_secret"))

# Deploy the application.
deployApp()