source("dev/process_file.R")

process_file("../api/getRedcap.R", "R/getRedcap.R")
process_file("../api/SecretsEnv.R", "R/SecretsEnv.R")
process_file("../api/ConfigEnv.R", "R/ConfigEnv.R")
process_file("../api/src/shortcuts.R", "R/aliases.R")
process_file("../api/src/animations.R", "R/animations.R")
process_file("../api/getSurvey.R", "R/getQualtrics.R")
process_file("../api/getTask.R", "R/getMongo.R")
process_file("../api/src/addPrefixToColumns.R", "R/addPrefixToColumns.R")
process_file("../api/redcap/capr-logic-wizaRdry.R", "R/processCaprData.R")
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
process_file("../api/src/createNda.R", "R/createNda.R")
process_file("../api/dataFilter.R", "R/dataFilter.R")
process_file("../api/dataMerge.R", "R/dataMerge.R")
process_file("../api/dataParse.R", "R/dataParse.R")

file.copy(from = "../api/scry.R", to = "R/scry.R", overwrite = TRUE)

process_file("../api/src/createExtract.R", "R/createExtract.R")

process_file("../api/src/checkKeys.R", "R/checkKeys.R")

process_file("../api/src/addColumnPrefix.R", "R/addColumnPrefix.R")

process_file("../api/src/checkDuplicateScripts.R", "R/checkDuplicateScripts.R")

