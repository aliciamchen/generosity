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
    In this study we are interested in how we can make inferences about other people's relationships by observing their behavior. We will describe a behavior or interaction, and then ask you how likely it is that they are in an <em>asymmetric</em> or a <em>symmetric</em> relationship, or no relationship.
    </p>
    <p>
    <ul>
    <li>In an <b>asymmetric</b> relationship, one person is <b>higher</b> in rank, importance or influence than the other.</li>
    <li>In a <b>symmetric</b> relationship, the two people are <b>equal</b> in rank, importance, or influence.</li>
    </ul>
    </p>
    <p>
    We are interested in how people use just brief observation to guess what type of relationship is guiding people's expectations in the interaction.
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
    You will be asked to rate how likely each relationship is. Please consider each relationship <strong>independently</strong>.
    </p>
    <p>
    You will receive $${params.basePay} if you successfully complete this study.
    </p>
    <p style="color: red;">
    ⚠️ Press 'next' to begin the study. ⚠️
    </p>
    `
}
