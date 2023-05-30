function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      // First question
      {
        type: jsPsychSurveyMultiChoice,
        preamble: function() {
          var html = `<h2>First time</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p><hr>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<div class="container"><div class="text-box"><p>${jsPsych.timelineVariable("first_q")}</p></div></div>\
              `;
              return html;
            },
            name: "first_q",
            options: function() {
              var options = [
                `${jsPsych.timelineVariable("alice")}`,
                `${jsPsych.timelineVariable("bob")}`,
              ];
              return options;
            },
            required: true,
            horizontal: true,
          },
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        button_label: "Submit",
        //          if (data.story == "attention") {
        //            if (
        //              data.response.first_q == "alice" &&
        //              data.response.symmetric == 0 &&
        //              data.response.no_relationship == 6
        //            ) {
        //              data.passAttentionCheck = true;
        //            } else {
        //              data.passAttentionCheck = false;
        //            }
        //          }
      },
      // Submitted response
      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Submitting response...",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
      // Feedback
      {
        type: jsPsychHtmlButtonResponse,
        stimulus: function () {
          var html = `<h2>First time actual</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p>\
          <div class="container"><div class="text-box">
          <p><strong>What actually happened the first time: </strong>${jsPsych.timelineVariable("first_actual")}</p></div></div> <br>`
          return html;
        },
        choices: ["Continue"],
      },

      // Second question
      {
        type: jsPsychSurveyMultiChoice,
        preamble: function() {
          var html = `<h2>Second time</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p>\
          <p>The <strong>first time</strong>, ${jsPsych.timelineVariable("first_actual")}</p>
          <hr>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<div class="container"><div class="text-box"><p>${jsPsych.timelineVariable("second_q")}</p></div></div>`;
              return html;
            }
            ,
            name: "second_q",
            options: [
              jsPsych.timelineVariable("alice"),
              jsPsych.timelineVariable("bob"),
            ],
            required: true,
            horizontal: true,
          },
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },

        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );
        },
      },

      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Thank you for your responses! Next scenario...",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
    ],
    timeline_variables:   [{
      story: "conversation",
      altruistic_status: "more",
      first_meeting: "alice",
      vignette: "Consider Angela and Alice, who work at the same company. <strong>Alice and Angela are in a relationship where Angela has less power, status, or influence than Alice.</strong> Alice and Angela meet for one hour every week.",
      first_actual: "Alice spent most of the time talking, and Angela spent most of the time asking questions.",
      first_q: "The first time Alice and Angela met, who spent most of the time talking, and who spent most of the time asking questions?",
      alice: "Alice spent most of the time talking, and Angela spent most of the time asking questions.",
      bob: "Angela spent most of the time talking, and Alice spent most of the time asking questions.",
      second_q: "The second time Alice and Angela met, who spent most of the time talking, and who spent most of the time asking questions?"
    }],// fetchTrialParams(condition_number),
    randomize_order: true,
  };

  // var attentionParams = fetchAttentionTrialParams();

//  var attentionTrial = {
//    type: jsPsychSurveyLikert,
//    preamble: attentionParams.vignette,
//    questions: [
//      {
//        prompt: attentionParams.asymmetric,
//        name: "asymmetric",
//        labels: params.likertLabels,
//      },
//      {
//        prompt: attentionParams.symmetric,
//        name: "symmetric",
//        labels: params.likertLabels,
//      },
//      {
//        prompt: attentionParams.no_relationship,
//        name: "no_relationship",
//        labels: params.likertLabels,
//      },
//    ],
//    data: {
//      type: "response",
//      story: attentionParams.story,
//      social_interaction: attentionParams.social_interaction,
//      vignette: attentionParams.vignette,
//      answer_labels: {
//        asymmetric: attentionParams.asymmetric,
//        symmetric: attentionParams.symmetric,
//        no_relationship: attentionParams.no_relationship,
//      },
//    },
//    randomize_question_order: false,
//    button_label: "Submit",
//    on_finish: function (data) {
//      if (
//        data.response.asymmetric == 6 &&
//        data.response.symmetric == 0 &&
//        data.response.no_relationship == 6
//      ) {
//        data.passAttentionCheck = true;
//      } else {
//        data.passAttentionCheck = false;
//      }
//    },
//  };
//
  //return [regularTrials, attentionTrial];
  return [regularTrials]
}
