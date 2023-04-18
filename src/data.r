library(writexl)

source("src/edmus.r")
source("src/biobank.r")

set.seed(12345)

patients <- edmus_personal |>
    left_join(edmus_diagnosis, by = "patient_id") |>
    filter(
        wait_and_see == FALSE,
        disease_course %in% c("RR", "SP-R", "SP-NR")
    )

ocrevus_treatments <- edmus_trt_dm |>
    filter(inn == "OCRELIZUMAB") |>
    semi_join(patients, by = "patient_id") |>
    select(
        patient_id,
        ocrevus_start = onset_date,
        ocrevus_status = status,
        ocrevus_end = end_date
    )

ocrevus_relapses <- edmus_episodes |>
    select(patient_id, relapse_id = episode_id, relapse_date = date) |>
    inner_join(ocrevus_treatments, by = "patient_id", relationship = "many-to-many") |>
    filter(
        relapse_date >= (ocrevus_start + dmonths(6)),
        ocrevus_status == "Ongoing" | relapse_date < ocrevus_end
    ) |>
    select(patient_id, relapse_id, relapse_date)

relapses_with_assessments <- ocrevus_relapses |>
    inner_join(
        edmus_clinical |> select(patient_id, assessment_id, assessment_date = date),
        by = "patient_id", relationship = "many-to-many"
    ) |>
    filter(assessment_date < relapse_date) |>
    mutate(time_between_relapse_and_last_assessment = relapse_date - assessment_date) |>
    filter(time_between_relapse_and_last_assessment < dmonths(9)) |>
    slice_min(
        time_between_relapse_and_last_assessment,
        by = c("patient_id", "relapse_id"),
        n = 1, with_ties = FALSE
    ) |>
    rename(
        last_assessment_id = assessment_id,
        date_of_last_assessment = assessment_date
    )

nonrelapse_assessments <- edmus_clinical |>
    select(patient_id, assessment_id, assessment_date = date) |>
    inner_join(ocrevus_treatments, by = "patient_id", relationship = "many-to-many") |>
    inner_join(ocrevus_relapses, by = "patient_id", relationship = "many-to-many") |>
    filter(
        assessment_date >= (ocrevus_start + dmonths(3)),
        ocrevus_status == "Ongoing" | assessment_date < ocrevus_end
    ) |>
    select(patient_id, assessment_id, assessment_date, relapse_id, relapse_date) |>
    mutate(time_between_relapse_and_assessment = assessment_date - relapse_date) |>
    filter(abs(time_between_relapse_and_assessment) >= dmonths(3)) |>
    slice_min(
        abs(time_between_relapse_and_assessment),
        by = c("patient_id", "relapse_id"),
        n = 1, with_ties = FALSE
    ) |>
    rename(
        closest_relapse_id = relapse_id,
        date_of_closest_relapse = relapse_date,
        time_between_assessment_and_closest_relapse = time_between_relapse_and_assessment
    )

patient_info <- patients |>
    select(
        patient_id,
        edmus_local_id = local_identifier,
        nhc = other_identifier
    ) |>
    mutate(
        sap = coalesce(
            biobank_nhc2sap(nhc),
            biobank_edmus2sap(edmus_local_id)
        )
    ) |>
    select(-nhc, -edmus_local_id)

assessment_cellcounts <- read_excel("data/ms-cellcounts.xlsx")

relapse_data <- relapses_with_assessments |>
    left_join(assessment_cellcounts, by = c(last_assessment_id = "assessment_id")) |>
    slice_sample(n = 1, by = "patient_id") |>
    left_join(patient_info, by = "patient_id") |>
    relocate(
        patient_id, sap, relapse_id, relapse_date, last_assessment_id,
        date_of_last_assessment, time_between_relapse_and_last_assessment
    ) |>
    arrange(patient_id, relapse_date)

nonrelapse_data <- nonrelapse_assessments |>
    anti_join(relapse_data, by = c(assessment_id = "last_assessment_id")) |>
    slice_sample(n = 1, by = "patient_id") |>
    left_join(patient_info, by = "patient_id") |>
    left_join(assessment_cellcounts, by = "assessment_id") |>
    relocate(
        patient_id, sap, assessment_id, assessment_date,
        date_of_closest_relapse, time_between_assessment_and_closest_relapse
    ) |>
    arrange(patient_id, assessment_date)

dir.create("output", showWarnings = FALSE)
write_xlsx(relapse_data, "output/ocrevus-relapses.xlsx")
write_xlsx(nonrelapse_data, "output/ocrevus-nonrelapses.xlsx")
