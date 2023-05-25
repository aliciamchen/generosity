function instructions(condition_number) {
    return {
        type: jsPsychInstructions,
        pages: [
            instructionsPage1(condition_number),
            instructionsPage2()
        ],
        show_clickable_nav: true,
        show_page_number: true
    }
}

function instructionsPage1(condition_number) {
    return `
    <h2>Instructions</h2>
    <p>
    In this study we are interested in how people think about who benefits in social interactions. \
    We will describe a social interaction between two people, and then ask you how much each of the people benefits from the social interaction, compared to if the social interaction doesn't happen at all.
    </p>
    <p>
    You will read about <strong>${fetchTrialParams(condition_number).length}</strong> total scenarios in this survey.
    </p>
    `
}

function instructionsPage2() {
    return `
    <h2>Instructions</h2>
    <p>
    You will be asked to rate how much each of the people benefits in the social interaction, compared to <strong>if they don't interact</strong>.
    </p>
    <p>
    You will receive $${params.basePay} if you successfully complete this study.
    </p>
    <p style="color: red;">
    ⚠️ Press 'next' to begin the study. ⚠️
    </p>
    `
}
