function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      {
        type: jsPsychSurveyLikert,
        preamble: jsPsych.timelineVariable("vignette"),
        questions: [
          {
            prompt: jsPsych.timelineVariable("less"),
            name: "less",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("more"),
            name: "more",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("equal"),
            name: "equal",
            labels: params.likertLabels,
          }
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          social_interaction: jsPsych.timelineVariable("social_interaction"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            less: jsPsych.timelineVariable("less"),
            more: jsPsych.timelineVariable("more"),
            equal: jsPsych.timelineVariable("equal"),
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
              data.response.less == 6 &&
              data.response.more == 0 &&
              data.response.equal == 6
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
        prompt: attentionParams.less,
        name: "less",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.more,
        name: "more",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.equal,
        name: "equal",
        labels: params.likertLabels,
      }
    ],
    data: {
      type: "response",
      story: attentionParams.story,
      social_interaction: attentionParams.social_interaction,
      vignette: attentionParams.vignette,
      answer_labels: {
        less: attentionParams.less,
        more: attentionParams.more,
        equal: attentionParams.equal
      },
    },
    randomize_question_order: false,
    button_label: "Submit",
    on_finish: function (data) {
        if (
          data.response.less == 6 &&
          data.response.more == 0 &&
          data.response.equal == 6
        ) {
          data.passAttentionCheck = true;
        } else {
          data.passAttentionCheck = false;
        }
    },
  };

  return [regularTrials, attentionTrial];
}
