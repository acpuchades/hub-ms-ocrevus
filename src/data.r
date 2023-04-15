library(writexl)

source("src/edmus.r")

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
    select(patient_id, relapse_date = date) |>
    inner_join(ocrevus_treatments, by = "patient_id", relationship = "many-to-many") |>
    filter(
        relapse_date >= (ocrevus_start + dmonths(3)),
        ocrevus_status == "Ongoing" | relapse_date < ocrevus_end
    )

relapses_with_assessments <- ocrevus_relapses |>
    inner_join(
        edmus_clinical |> select(patient_id, assessment_date = date),
        by = "patient_id", relationship = "many-to-many"
    ) |>
    filter(assessment_date < relapse_date) |>
    mutate(time_between_relapse_and_last_assessment = relapse_date - assessment_date) |>
    group_by(patient_id) |>
    slice_min(time_between_relapse_and_last_assessment, n = 1, with_ties = FALSE) |>
    ungroup() |>
    rename(date_of_last_assessment = assessment_date)

data <- relapses_with_assessments |>
    left_join(edmus_personal |> select(
        patient_id,
        nhc = other_identifier, gender, date_of_birth
    ), by = "patient_id") |>
    left_join(edmus_diagnosis |> select(patient_id, disease_course, ms_onset), by = "patient_id") |>
    mutate(
        time_between_onset_and_ocrevus_start = ocrevus_start - ms_onset,
        time_between_relapse_and_onset = relapse_date - ms_onset,
        age_at_relapse = (relapse_date - date_of_birth) / dyears(1)
    ) |>
    rename(edmus_id = patient_id) |>
    relocate(
        edmus_id, nhc, date_of_birth, gender, disease_course, ms_onset,
        starts_with("ocrevus_"), time_between_onset_and_ocrevus_start,
        relapse_date, age_at_relapse, date_of_last_assessment,
        time_between_relapse_and_onset, time_between_relapse_and_last_assessment
    )

dir.create("output", showWarnings = FALSE)
write_xlsx(data, "output/ocrevus-relapses.xlsx")
