function fetchTrialParams() {
  var trials = null;

  $.ajax({
    async: false,
    global: false,
    url: "json/stimuli.json",
    dataType: "json",
    success: function (data) {
      trials = data.filter((x) => x.story != "attention");
    },
  });

  return trials;
}

function fetchAttentionTrialParams() {
  var trial = null;

  $.ajax({
    async: false,
    global: false,
    url: "json/stimuli.json",
    dataType: "json",
    success: function (data) {
      trial = data.filter((x) => x.story == "attention")[0]
    },
  });

  return trial;
}