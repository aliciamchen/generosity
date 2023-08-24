function makeTrials(jsPsych) {
  var regularTrials = {
    timeline: [
      {
        type: jsPsychSurveyLikert,
        preamble: jsPsych.timelineVariable("vignette"),
        questions: [
          {
            prompt: jsPsych.timelineVariable("expected_high_benefit"),
            name: "expected_high_benefit",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("expected_low_benefit"),
            name: "expected_low_benefit",
            labels: params.likertLabels,
          },
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            expected_high_benefit: jsPsych.timelineVariable("expected_high_benefit"),
            expected_low_benefit: jsPsych.timelineVariable("expected_low_benefit")
          },
        },
        randomize_question_order: true,
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams().length
          );

          if (data.story == "attention") {
            if (
              data.response.expected_high_benefit == 6 &&
              data.response.expected_low_benefit == 0
            ) {
              data.passAttentionCheck = true;
            } else {
              data.passAttentionCheck = false;
            }
          }
        },
      },
      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Next scenario",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
    ],
    timeline_variables: fetchTrialParams(),
    randomize_order: true,
  };

  var attentionParams = fetchAttentionTrialParams();

  var attentionTrial = {
    type: jsPsychSurveyLikert,
    preamble: attentionParams.vignette,
    questions: [
      {
        prompt: attentionParams.expected_high_benefit,
        name: "expected_high_benefit",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.expected_low_benefit,
        name: "expected_low_benefit",
        labels: params.likertLabels,
      }
    ],
    data: {
      type: "response",
      story: attentionParams.story,
      vignette: attentionParams.vignette,
      answer_labels: {
        expected_high_benefit: attentionParams.expected_high_benefit,
        expected_low_benefit: attentionParams.expected_low_benefit
      },
    },
    randomize_question_order: false,
    button_label: "Submit",
    on_finish: function (data) {
        if (
          data.response.expected_high_benefit == 6 &&
          data.response.expected_low_benefit == 0
        ) {
          data.passAttentionCheck = true;
        } else {
          data.passAttentionCheck = false;
        }
    },
  };

  return [regularTrials, attentionTrial];
}
