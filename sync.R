file.copy(from = "../api/getRedcap.R", to = "R", overwrite = TRUE)

file.copy(from = "../api/SecretsEnv.R", to = "R", overwrite = TRUE)
file.copy(from = "../api/ConfigEnv.R", to = "R", overwrite = TRUE)




file.copy(from = "../api/getSurvey.R", to = "R/getQualtrics.R", overwrite = TRUE)
file.copy(from = "../api/getTask.R", to = "R/getMongo.R", overwrite = TRUE)

file.copy(from = "../api/dataRequest.R", to = "R", overwrite = TRUE)
file.copy(from = "../api/ndaRequest.R", to = "R", overwrite = TRUE)
