test_workflow <- WorkFlow$new(name = "Test Workflow")
test_path <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove")
load_all()
.produce_report(
    report_type = "standard_report",
    params = list(workflow = test_workflow), path = test_path
)



# workflow with json opt
test_workflow <- WorkFlow$new(name = "Test Workflow")
test_workflow[[".__enclos_env__"]]$private$.isa_options$json_source <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove/isa_options.json")
test_path <- fs::path("/Volumes/Macintosh HD - Data/Projects/Reports/Prove")
load_all()
.produce_report(
    report_type = "standard_report",
    params = list(workflow = test_workflow), path = test_path
)
