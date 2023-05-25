function instructions() {
    return {
        type: jsPsychInstructions,
        pages: [
            instructionsPage1(),
            instructionsPage2()
        ],
        show_clickable_nav: true,
        show_page_number: true
    }
}

function instructionsPage1() {
    return `
    <h2>Instructions</h2>
    <p>
    In this study we are interested in how people think about who puts in effort in social interactions. \
    We will describe a social interaction between two people, and then ask you how much each of the people puts in effort in the social interaction.
    </p>
    <p>
    You will read about <strong>${fetchTrialParams().length}</strong> total scenarios in this survey.
    </p>
    `
}

function instructionsPage2() {
    return `
    <h2>Instructions</h2>
    <p>
    You will be asked to rate how much effort each of the people puts in, in the social interaction.
    </p>
    <p>
    You will receive $${params.basePay} if you successfully complete this study.
    </p>
    <p style="color: red;">
    ⚠️ Press 'next' to begin the study. ⚠️
    </p>
    `
}
