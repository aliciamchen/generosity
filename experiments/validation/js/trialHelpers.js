var nCombinations = null;

$.ajax({
  async: false,
  global: false,
  url: "json/full_counterbalancing.json",
  dataType: "json",
  success: function (data) {
    nCombinations = data.length;
  },
});

function fetchTrialParams(condition_number) {
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