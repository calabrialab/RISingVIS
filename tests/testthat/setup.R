library(RISingVIS)

# Load sample files
sample_dataset_sym <- "sample_dataset"
utils::data(list = sample_dataset_sym, package = "RISingVIS",
            envir = rlang::current_env())
sample_matrix <- rlang::env_get(env = rlang::current_env(),
                                nm = "sample_matrix")
sample_metadata <- rlang::env_get(env = rlang::current_env(),
                                nm = "sample_metadata")
data_and_meta <- dplyr::left_join(sample_matrix, sample_metadata,
                                  by = "CompleteAmplificationID")
