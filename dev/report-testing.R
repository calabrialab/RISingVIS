# test_workflow <- WorkFlow$new(name = "Test Workflow")
# test_path <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove/risingvis")
# load_all()
# .produce_report(
#     report_type = "standard_report",
#     params = list(workflow = test_workflow), path = test_path
# )
# # workflow with json opt
# test_workflow <- WorkFlow$new(name = "Test Workflow")
# test_workflow[[".__enclos_env__"]]$private$.isa_options$json_source <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove/isa_options.json")
# test_path <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove")
# load_all()
# .produce_report(
#     report_type = "standard_report",
#     params = list(workflow = test_workflow), path = test_path
# )



test_path <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove/risingvis")
test_workflow <- readRDS("~/RISingVIS/dev/test-workflow.rds")

load_all()
wf <- WorkFlow$new(name = "Test Workflow")
wf[[".__enclos_env__"]]$private$.isa_options <- test_workflow[[".__enclos_env__"]]$private$.isa_options
wf[[".__enclos_env__"]]$private$.isa_report_path <- test_workflow[[".__enclos_env__"]]$private$.isa_report_path
wf[[".__enclos_env__"]]$private$.metadata <- test_workflow[[".__enclos_env__"]]$private$.metadata
.produce_report(
    report_type = "standard_report",
    params = list(workflow = wf),
    path = test_path
)
