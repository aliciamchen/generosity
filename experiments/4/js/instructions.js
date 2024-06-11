function instructions(condition_number) {
    return {
        type: jsPsychInstructions,
        pages: [
            instructionsPage1(condition_number),
            instructionsPage2(),
            instructionsPage3()
        ],
        show_clickable_nav: true,
        show_page_number: true
    }
}

function instructionsPage1(condition_number) {
    return `
    <h2>Instructions</h2>
    <p>
    In this study we are interested in how we can evaluate people's motives for behavior in their social relationships.
    </p>
    <p>
    You will read about <strong>${fetchTrialParams(condition_number).length}</strong> scenarios, each one describing a <strong>social relationship</strong> and some <strong>social interactions</strong> between two people.
    </p>
    <p>
    We are interested in how people use just brief observation to guess how social relationships guide people's motivations for behavior.
    </p>
    `
}

function instructionsPage2() {
    return `
    <h2>Instructions</h2>
    <p>For each scenario, we will tell you what happened the <strong>first time</strong> the two people interacted.</p>
    <p>Then, we will tell you what happened the <strong>second time</strong> they interacted.</p>
    <p>We will ask you to evaluate how much the person performing the generous action the second time was motivated to <strong>benefit themselves</strong>, <strong>benefit the other person</strong>, <strong>make sure the other person's benefits are as high as their own</strong>, <strong>communicate about wanting an equal relationship</strong>, and <strong>communicate about wanting a hierarchical relationship</strong>.</p>
 `
}

function instructionsPage3() {
    return `
    <h2>Instructions</h2>
    <p>
    You will receive $${params.basePay} if you successfully complete this study.
    </p>
    <p>Please make sure to read each scenario carefully! ☺️</p>
    <p style="color: red;">
    ⚠️ Press 'next' to begin the study. ⚠️
    </p>
    `
}
