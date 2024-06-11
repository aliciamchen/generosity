function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      {
        type: jsPsychSurveyLikert,
        preamble: function() {
          var html = `<h2>${jsPsych.timelineVariable("story")}</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p><hr>
          <p>${jsPsych.timelineVariable("first_actual")}</p>
          <p>${jsPsych.timelineVariable("second_actual")}</p><hr>
          <p><em>${jsPsych.timelineVariable("q")}</em></p>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `${jsPsych.timelineVariable("own_benefit")}
              `;
              return html;
            },
            name: "own_benefit",
            labels: params.labels,
          },
          {
            prompt: function() {
              var html = `${jsPsych.timelineVariable("other_benefit")}
              `;
              return html;
            },
            name: "other_benefit",
            labels: params.labels,
          },
          {
            prompt: function() {
              var html = `${jsPsych.timelineVariable("inequity_aversion")}
              `;
              return html;
            },
            name: "inequity_aversion",
            labels: params.labels,
          },
          {
            prompt: function() {
              var html = `${jsPsych.timelineVariable("communicate_equal")}
              `;
              return html;
            },
            name: "communicate_equal",
            labels: params.labels,
          },
          {
            prompt: function() {
              var html = `${jsPsych.timelineVariable("communicate_hierarchy")}
              `;
              return html;
            },
            name: "communicate_hierarchy",
            labels: params.labels,
          }
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          strategy: jsPsych.timelineVariable("strategy"),
          generous_status_second: jsPsych.timelineVariable("generous_status_second"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          second_actual: jsPsych.timelineVariable("second_actual"),
          q: jsPsych.timelineVariable("q"),
          own_benefit: jsPsych.timelineVariable("own_benefit"),
          other_benefit: jsPsych.timelineVariable("other_benefit"),
          inequity_aversion: jsPsych.timelineVariable("inequity_aversion"),
          communicate_equal: jsPsych.timelineVariable("communicate_equal"),
          communicate_hierarchy: jsPsych.timelineVariable("communicate_hierarchy"),
          vignette: jsPsych.timelineVariable("vignette")
        },
        randomize_question_order: false,
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );

          if (data.generous_status_second == "attention") {
            if (
              data.response.own_benefit == 0 && data.response.other_benefit == 6 && data.response.inequity_aversion == 6 && data.response.communicate_equal == 0 && data.response.communicate_hierarchy == 6
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
      <p><span class="vignette">${attentionParams.second_actual}</span></p>
      <p><span class="vignette">${attentionParams.q}</span></p>`
      return html;
    },
    questions: [
      {
        prompt: attentionParams.own_benefit,
        name: "own_benefit",
        labels: params.labels,
      },
      {
        prompt: attentionParams.other_benefit,
        name: "other_benefit",
        labels: params.labels,
      },
      {
        prompt: attentionParams.inequity_aversion,
        name: "inequity_aversion",
        labels: params.labels,
      },
      {
        prompt: attentionParams.communicate_equal,
        name: "communicate_equal",
        labels: params.labels,
      },
      {
        prompt: attentionParams.communicate_hierarchy,
        name: "communicate_hierarchy",
        labels: params.labels,
      }
    ],
    data: {
      type: "response",
      story: attentionParams.story,
      generous_status_second: attentionParams.generous_status_second,
      first_actual: attentionParams.first_actual,
      second_actual: attentionParams.second_actual,
      vignette: attentionParams.vignette,
      q: attentionParams.q,
      own_benefit: attentionParams.own_benefit,
      other_benefit: attentionParams.other_benefit,
      inequity_aversion: attentionParams.inequity_aversion,
      communicate_equal: attentionParams.communicate_equal,
      communicate_hierarchy: attentionParams.communicate_hierarchy,
    },
    randomize_question_order: false,
    button_label: "Submit",
    on_finish: function (data) {
        if (
          data.response.own_benefit == 0 && data.response.other_benefit == 6 && data.response.inequity_aversion == 6 && data.response.communicate_equal == 0 && data.response.communicate_hierarchy == 6
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
