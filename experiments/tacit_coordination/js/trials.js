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
          stage: "first",
          // correct: jsPsych.timelineVariable("first_meeting") === data.response.first_q,
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        on_finish: function (data) {
          data.correct = jsPsych.timelineVariable("first_meeting") === data.response.first_q
          console.log(data)
        },
        button_label: "Submit",
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
          stage: "second",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );
          data.strategy = jsPsych.data.get().last(4).values()[0].response.first_q === data.response.second_q ? "repeating" : "alternating"
          console.log(data)
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

  var attentionTrial = {
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
          type: "attention",
          stage: "first",
          // correct: jsPsych.timelineVariable("first_meeting") === data.response.first_q,
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        on_finish: function (data) {
          data.correct = jsPsych.timelineVariable("first_meeting") === data.response.first_q
          console.log(data)
        },
        button_label: "Submit",
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
          type: "attention",
          stage: "second",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );
          data.passAttention = (data.response.second_q == jsPsych.timelineVariable("bob")) && (jsPsych.data.get().last(4).values()[0].response.first_q == jsPsych.timelineVariable("alice"))
          console.log(data.passAttention)
        },
      },

      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Thank you for your responses!",
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
        story: "attention",
        altruistic_status: "",
        first_meeting: "",
        vignette: "Thank you for contributing to this study! This page is an attention check so we can make sure you're not a bot and can award you your pay when you complete the study.",
        first_actual: "Please press continue.",
        first_q: "Please select \"Alice spent most of the time talking...\" for this question.",
        alice: "Alice spent most of the time talking, and Angela spent most of the time asking questions.",
        bob: "Angela spent most of the time talking, and Angela spent most of the time asking questions.",
        second_q: "Please select \"Angela spent most of the time talking...\" for this question."
    }],// fetchTrialParams(condition_number),
  };

  return [regularTrials, attentionTrial];
  // return [regularTrials]
}
