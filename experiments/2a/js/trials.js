function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      {
        type: jsPsychSurveyLikert,
        preamble: jsPsych.timelineVariable("vignette"),
        questions: [
          {
            prompt: jsPsych.timelineVariable("asymmetric"),
            name: "asymmetric",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("symmetric"),
            name: "symmetric",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("no_relationship"),
            name: "no_relationship",
            labels: params.likertLabels,
          }
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          social_interaction: jsPsych.timelineVariable("social_interaction"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            asymmetric: jsPsych.timelineVariable("asymmetric"),
            symmetric: jsPsych.timelineVariable("symmetric"),
            no_relationship: jsPsych.timelineVariable("no_relationship"),
          },
        },
        randomize_question_order: false,
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );

          if (data.social_interaction == "attention") {
            if (
              data.response.asymmetric == 6 &&
              data.response.symmetric == 0 &&
              data.response.no_relationship == 6
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
    timeline_variables: fetchTrialParams(condition_number),
    randomize_order: true,
  };

  var attentionParams = fetchAttentionTrialParams();

  var attentionTrial = {
    type: jsPsychSurveyLikert,
    preamble: attentionParams.vignette,
    questions: [
      {
        prompt: attentionParams.asymmetric,
        name: "asymmetric",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.symmetric,
        name: "symmetric",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.no_relationship,
        name: "no_relationship",
        labels: params.likertLabels,
      }
    ],
    data: {
      type: "response",
      story: attentionParams.story,
      social_interaction: attentionParams.social_interaction,
      vignette: attentionParams.vignette,
      answer_labels: {
        asymmetric: attentionParams.asymmetric,
        symmetric: attentionParams.symmetric,
        no_relationship: attentionParams.no_relationship
      },
    },
    randomize_question_order: false,
    button_label: "Submit",
    on_finish: function (data) {
        if (
          data.response.asymmetric == 6 &&
          data.response.symmetric == 0 &&
          data.response.no_relationship == 6
        ) {
          data.passAttentionCheck = true;
        } else {
          data.passAttentionCheck = false;
        }
    },
  };

  return [regularTrials, attentionTrial];
}
