function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      {
        type: jsPsychSurveyLikert,
        preamble: function() {
          var html = `<h2>${jsPsych.timelineVariable("story")}</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p><hr>
          <p>${jsPsych.timelineVariable("first_actual")}</p>
          <p>${jsPsych.timelineVariable("second_actual")}</p>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<p style="font-size: 20px;">${jsPsych.timelineVariable("q")}</p>\
              `;
              return html;
            }, // jsPsych.timelineVariable("q"),
            name: "q",
            labels: params.likertLabels,
          }
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          strategy: jsPsych.timelineVariable("strategy"),
          altruistic_status_second: jsPsych.timelineVariable("altruistic_status_second"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          second_actual: jsPsych.timelineVariable("second_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          q: jsPsych.timelineVariable("q"),
        },
        randomize_question_order: false,
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );

          if (data.relationship == "attention") {
            if (
              data.response.q == 6
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

  var attentionParams = fetchAttentionTrialParams()[0];

  var attentionTrial = {
    type: jsPsychSurveyLikert,
    preamble: function() {
      var html = `<h2>First time</h2>\
      <p><span class="vignette">${attentionParams.vignette}</span></p><hr>
      <p><span class="vignette">${attentionParams.first_actual}</span></p>
      <p><span class="vignette">${attentionParams.second_actual}</span></p>`
      return html;
    },
    questions: [
      {
        prompt: attentionParams.q,
        name: "q",
        labels: params.likertLabels,
      }
    ],
    data: {
      type: "response",
      story: attentionParams.story,
      altruistic_status_second: attentionParams.altruistic_status_second,
      first_actual: attentionParams.first_actual,
      second_actual: attentionParams.second_actual,
      vignette: attentionParams.vignette,
      q: attentionParams.q,
    },
    randomize_question_order: false,
    button_label: "Submit",
    on_finish: function (data) {
        if (
          data.response.q == 6
        ) {
          data.passAttentionCheck = true;
        } else {
          data.passAttentionCheck = false;
        }
    },
  };

  return [regularTrials, attentionTrial];
  // return [attentionTrial]
}
