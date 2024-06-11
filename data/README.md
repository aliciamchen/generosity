# Codebook

This folder contains data and demographic info for each experiment.

## Data files

The below entries correspond to the non-processed wide-form data (tidy data with labels modified and organized is also provided under the `{}_tidy_data.csv` labels)

### Study 1

- `1a_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `relationship`: what relationship are the two characters in? (either `no_info`, `asymmetric`, or `symmetric`)
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `repeating`: likelihood of character repeating the generous act
        - `alternating`: likelihood of other character performing the generous act
        - `none`: likelihood of no future interaction
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `1b_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `relationship`: what relationship are the two characters in? (corresponds to relative status of generous character: `more`, `equal`, or `less`)
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `repeating`: likelihood of character repeating the generous act
        - `alternating`: likelihood of other character performing the generous act
        - `none`: likelihood of no future interaction
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `1c_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `altruistic_status`: what relationship are the two characters in? (corresponds to relative status of first-time experimenter-manipulated generous character: `more`, `equal`, or `less`); this corresponds to 'observed first time' in the manuscript
    - `first_meeting`: out of `alice` and `bob`, who was generous the first time? (labeling each character as `alice` or `bob` is so that we can keep track of counterbalancing between the actual names in the scenario)
    - `stage`: is this the participant's `first` response (i.e. implicit coordination response), or their `second` response (i.e. what they predicted happened the second time the two people interacted)?
    - `response`: who did the participant think was generous (either `alice` or `bob`)
    - `response_status`: did whoever they picked for their response have `more`, `equal`, or `less` power/status/influence?
    - `strategy`: did people expect `repeating` or `alternating` actions?
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

### Study 2

- `2a_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `social_interaction`: did the two participants in the scenario alternate (`reciprocity`) or repeat (`precedent`) generous acts, or did they not interact (`no_interaction`)?
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `asymmetric`: likelihood of the two people being in an asymmetric relationship
        - `symmetric`: likelihood of the two people being in a symmetric relationship
        - `no_relationship`: likelihood of the two people not being in any relationship
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `2b_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `social_interaction`: did the two participants in the scenario alternate (`reciprocity`) or repeat (`precedent`) generous acts, or did they not interact (`no_interaction`)?
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `equal`: likelihood of the generous character being equal power/status/influence
        - `more`: likelihood of the generous character having more power/status/influence
        - `less`: likelihood of the generous character having less power/status/influence
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

### Study 3

- `3a_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `altruistic_status_second`: who was generous the second time the two characters interacted?
    - `strategy`: what was the sequence of social interactions? (`alternating` or `repeating`)
    - `response`: how moral? (0-indexed, on a 7 point Likert scale)
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `3b_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - `altruistic_status_second`: who was generous the second time the two characters interacted?
    - `strategy`: what was the sequence of social interactions? (`alternating` or `repeating`)
    - `response`: how fair? (0-indexed, on a 7 point Likert scale)
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `3c_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - `altruistic_status_second`: who was generous the second time the two characters interacted?
    - `strategy`: what was the sequence of social interactions? (`alternating` or `repeating`)
    - Participant responses for each trial (0-indexed, on a 7 point Likert scale)
        - `annoyed`: how annoyed for the generous character? (0-indexed, on a 7 point Likert scale)
        - `satisfied`: how satisfied was the generous character? (0-indexed, on a 7 point Likert scale)
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `3d_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - `altruistic_status_second`: who was generous the second time the two characters interacted?
    - `strategy`: what was the sequence of social interactions? (`alternating` or `repeating`)
    - Participant responses for each trial (0-indexed, on a 7 point Likert scale)
        - `annoyed`: how annoyed was the recipient? (0-indexed, on a 7 point Likert scale)
        - `satisfied`: how satisfied was the recipient? (0-indexed, on a 7 point Likert scale)
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

### Study 4

- `4_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - `generous_status_second`: who was generous the second time the two characters interacted?
    - `strategy`: what was the sequence of social interactions? (`alternating` or `repeating`)
    - Participant responses for each trial (0-indexed, on a 7 point Likert scale): How much was the generous character motivated to...
        - `own_benefit`: benefit themselves
        - `other_benefit`: benefit their partner
        - `inequity_aversion`: make their benefits and their partner's benefits equal
        - `communicate_equal`: communicate a desire for an equal relationship
        - `communicate_hierarchy`: communicate a desire for a hierarchical relationship
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

### Pilot

- `validation_benefit_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - participant responses (0-indexed, on a 7 point Likert scale)
        - `expected_high_benefit`: how much was the (assigned) target of generosity expected to benefit, compared to not interacting
        - `expected_low_benefit`: how much was the (assigned) generous actor expected to benefit, compared to not interacting
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

- `validation_effort_data.csv`
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - participant responses (0-indexed, on a 7 point Likert scale)
        - `expected_high_benefit`: how much was the (assigned) target of generosity expected to put in effort, compared to not interacting
        - `expected_low_benefit`: how much was the (assigned) generous actor expected to put in effort, compared to not interacting
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

`validation_benefit_diff.csv` and `validation_effort_diff.csv` contain computed mean benefit/effort values for each scenario, and their difference