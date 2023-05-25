$(function () {
    $('#templates').hide();

    /* set up jsPsych, save data */

    var jsPsych = initJsPsych({
        on_finish: function () {
            if (local_testing) {
                jsPsych.data.get().localSave("json", "testdata.json");
            }
        },
        show_progress_bar: true,
        auto_update_progress_bar: false
    });

    var subject_id = local_testing || jsPsych.data.getURLVariable('PROLIFIC_PID') == ""
                    ? jsPsych.randomization.randomID(12)
                    : jsPsych.data.getURLVariable('PROLIFIC_PID');
    var study_id = jsPsych.data.getURLVariable('STUDY_ID');
    var session_id = jsPsych.data.getURLVariable('SESSION_ID');

    jsPsych.data.addProperties({
        subject_id: subject_id,
        study_id: study_id,
        session_id: session_id,
        params: params,
        url: window.location.href
    });

    /* Save stuff */
    function save_data_json(name, data) {
        var xhr = new XMLHttpRequest();
        xhr.open("POST", "php/save_data.php");
        xhr.setRequestHeader("Content-Type", "application/json");
        xhr.send(JSON.stringify({ filename: name, filedata: data }));
    };


    const save_data = {
        type: jsPsychCallFunction,
        func: function () {
            save_data_json(subject_id + "_output_all", jsPsych.data.get().json());
            save_data_json(subject_id + "_output_responses",
                        jsPsych.data.get().filter({ type: 'response' }).json());
        },
        timing_post_trial: 0
    };

    /* Experiment */

    var timeline = [];

    timeline.push(intro(params));
    timeline.push(instructions(params));
    timeline.push(beginning(jsPsych));
    timeline.push(makeTrials(jsPsych))
    timeline.push(exitSurvey(jsPsych))

    if (!local_testing) {
        timeline.push(save_data);
    }

    timeline.push(debrief())

    jsPsych.run(timeline.flat());
})