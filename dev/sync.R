source("dev/process_file.R")

process_file("../api/getRedcap.R", "R/getRedcap.R")

file.copy(from = "../api/SecretsEnv.R", to = "R", overwrite = TRUE)
file.copy(from = "../api/ConfigEnv.R", to = "R", overwrite = TRUE)

file.copy(from = "../api/src/shortcuts.R", to = "R/aliases.R", overwrite = TRUE)
file.copy(from = "../api/src/animations.R", to = "R/animations.R", overwrite = TRUE)

process_file("../api/getSurvey.R", "R/getQualtrics.R")
process_file("../api/getTask.R", "R/getMongo.R")
file.copy(from = "../api/src/addPrefixToColumns.R", to = "R/addPrefixToColumns.R", overwrite = TRUE)


file.copy(from = "../api/redcap/capr-logic.R", to = "R/processCaprData.R", overwrite = TRUE)



process_file("../api/dataRequest.R", "R/dataRequest.R")

process_file("../api/ndaValidator.R", "R/ndaValidator.R")

process_file("../api/ndaRequest.R", "R/ndaRequest.R")

process_file("../api/testSuite.R", "R/testSuite.R")

process_file("../api/test/ndaCheckQualtricsDuplicates.R", "R/ndaCheckQualtricsDuplicates.R")

process_file("../api/test/checkQualtricsDuplicates.R", "R/checkQualtricsDuplicates.R")

process_file("../api/test/cleanDataFrameExists.R", "R/cleanDataFrameExists.R")

process_file("../api/test/ndaRequiredVariablesExist.R", "R/ndaRequiredVariablesExist.R")

process_file("../api/test/checkColumnPrefix.R", "R/checkColumnPrefix.R")

process_file("../api/test/checkInterviewAge.R", "R/checkInterviewAge.R")


process_file("../api/src/createCsv.R", "R/createCsv.R")
process_file("../api/src/createRda.R", "R/createRda.R")
process_file("../api/src/createSpss.R", "R/createSpss.R")

process_file("../api/src/ndaTemplate.R", "R/ndaTemplate.R")
