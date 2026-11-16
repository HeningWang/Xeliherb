<template>
  <Experiment title="Colonists of Xelifan-3">
    <!-- A study link without a valid ?condition= never starts the study (see README). -->
    <Screen v-if="!condition" title="This study cannot start">
      <p>The study link is incomplete. Please return the study on Prolific and contact
        <a :href="'mailto:' + $magpie.contactEmail">{{ $magpie.contactEmail }}</a>;
        you will not be penalised.</p>
    </Screen>

    <template v-else>
      <!-- Screen 1. Welcome and consent -->
      <Screen>
        <p>
          Welcome, and thank you for your interest in this study! In this short study, you will read a fictitious
          story about colonists on a distant planet and then judge whether something that was said in the story was
          wrong, partly right or right. Please read the story carefully. There are no right or wrong answers: we are interested in your own judgement.
        </p>
        <p>
          Participation is voluntary. You can stop at any time by closing the browser window, without giving a
          reason. Your data will be anonymised and used only for research purposes.
        </p>
        <p>If you have any questions about this study, please contact hening.wang@uni-tuebingen.de.</p>
        <p>
          By clicking the button below, you confirm that you are at least 18 years old, that you have read this
          information and that you agree to take part.
        </p>
        <p>The study takes about three minutes.</p>
        <button @click="save({})">I agree, start the study</button>
        <Record :data="trialInfo('welcome')" />
      </Screen>

      <!-- Screen 2. Cover story, identical to Experiment 1 -->
      <Screen>
        <p>
          You are one of a group of colonists who have just settled on a distant planet called Xelifan-3. To survive
          there, the colony depends on a plant called <strong>xeliherb</strong>, which grows only on Xelifan-3. The colonists have not
          yet started to cultivate <strong>xeliherb</strong>, so for now they gather it where it grows wild, in patches scattered
          across the plains near the settlement. Some patches give a high yield of <strong>xeliherb</strong> and others a low yield,
          and the colonists would like to gather as much as possible. Another wild plant, ralocrop, grows in some of
          the patches.
        </p>
        <CoverPicture />
        <button @click="save({})">Continue</button>
        <Record :data="trialInfo('cover')" />
      </Screen>

      <!-- Screen 3. Report, ground truth and comprehension question. A wrong answer asks for another
           choice without giving the answer; Continue appears once the answer matches the finding. -->
      <Screen>
        <Slide>
          <ReportAndFinding :condition="condition" />
          <p class="lead">According to the researchers, why do the two plants occur together?</p>
          <MultipleChoiceInput :options="comprehensionOrder.map((o) => o.text)" @update:response="checkComprehension" />
          <p v-if="comprehensionWrong" class="retry">
            That is not what the researchers found. Please read their finding again and choose another answer.
          </p>
          <button v-if="comprehensionCorrect"
                  @click="save({ comprehension_attempts: attempts.join('|'), comprehension_first_correct: attempts[0] === condition.truth,
                                 comprehension_order: comprehensionOrder.map((o) => o.id).join('|') })">
            Continue
          </button>
          <Record :data="trialInfo('comprehension')" />
        </Slide>
      </Screen>

      <!-- Screen 4. Judgement, with the report and the finding still in view -->
      <Screen>
        <Slide>
          <ReportAndFinding :condition="condition" />
          <p class="lead">Given what the researchers found out, was the colonist's report wrong, partly right or right?</p>
          <MultipleChoiceInput :options="['Wrong', 'Partly right', 'Right']" @update:response="judgement = $event" />
          <button v-if="judgement" @click="save({ judgement })">Continue</button>
          <Record :data="trialInfo('judgement')" />
        </Slide>
      </Screen>

      <!-- Screen 5. Thanks and debriefing; the button submits the data -->
      <Screen>
        <h3>The experiment is finished. Thank you for taking part!</h3>
        <details class="debrief">
          <summary>About this study (click to read more)</summary>
          <p>
            The planet, the colony, xeliherb and ralocrop were invented for this study, so that nobody knew anything
            about these plants beforehand.
          </p>
          <p>
            When two things often occur together, people tend to conclude that one causes the other. But occurring
            together does not show this on its own: the second could affect the first, or both could depend on a
            third thing. For example, ice-cream sales and sunburn go together because both depend on sunny weather,
            not because ice cream causes sunburn.
          </p>
          <p>
            In this study, everyone read one report from the colonist and then learned what the researchers found out
            about the two plants. Both the wording of the report and the finding of the researchers differed between
            participants. We want to know when people judge a report about two things that occur together to be
            wrong, and when they judge it to be right but incomplete. This helps us understand what people take a
            report to say and what they only infer from it, for example in news reports about health or science.
          </p>
          <p>Please do not tell others what the study is about, because they may still take part.</p>
        </details>
        <button @click="$magpie.nextScreen()">Finish and return to Prolific</button>
      </Screen>

      <SubmitResultsScreen />
    </template>
  </Experiment>
</template>

<script>
import _ from 'lodash';
import conditions from '../trials/conditions.csv';
import CoverPicture from './components/CoverPicture.vue';
import ReportAndFinding from './components/ReportAndFinding.vue';

// Each cell runs as its own Prolific study whose link names the cell: ?condition=1a-H|1b-H|1c-H|1b-G|1a-G
// (association, causal or neutral sentence; ground truth H, the common cause, or G, the forward link).
const requested = new URL(window.location).searchParams.get('condition');
const DESIGN_VERSION = 'v1'; // change whenever the materials change

// The four causal structures of Experiment 1, as answers to the comprehension question.
const STRUCTURES = [
  { id: 'G', text: 'Ralocrop makes xeliherb grow better.' },
  { id: 'R', text: 'Xeliherb makes ralocrop grow better.' },
  { id: 'H', text: 'Fertile soil makes both plants grow better.' },
  { id: 'N', text: 'The two plants grew together by chance.' }
];

export default {
  name: 'App',
  components: { CoverPicture, ReportAndFinding },
  data() {
    return {
      condition: conditions.find((c) => c.condition === requested) || null,
      comprehensionOrder: _.shuffle(STRUCTURES),
      attempts: [], // every answer chosen on the comprehension question, as structure ids
      judgement: null
    };
  },
  computed: {
    lastAttempt() {
      return this.attempts[this.attempts.length - 1];
    },
    comprehensionCorrect() {
      return this.lastAttempt === this.condition.truth;
    },
    comprehensionWrong() {
      return this.lastAttempt !== undefined && !this.comprehensionCorrect;
    }
  },
  methods: {
    checkComprehension(text) {
      this.attempts.push(STRUCTURES.find((s) => s.text === text).id);
    },
    trialInfo(screen) {
      const c = this.condition;
      return { screen, condition: c.condition, cell: c.cell, truth: c.truth, sentence: c.sentence, design_version: DESIGN_VERSION };
    },
    save(responses) {
      Object.assign(this.$magpie.measurements, responses);
      this.$magpie.saveAndNextScreen();
      window.scrollTo(0, 0);
    }
  }
};
</script>

<style>
:root { --ink: #2B2A30; --muted: #6b6875; --rule: #D7D8E2; color-scheme: light; }
html, body { background: #fff; color: var(--ink); } /* a light page whatever the participant's system theme */

.scene { width: 100%; height: auto; display: block; margin: 12px 0 4px; }
.legend { display: flex; gap: 20px; font-size: 14px; align-items: center; margin-bottom: 8px; }
.legend span { display: inline-flex; align-items: center; gap: 6px; }
.icon { vertical-align: middle; }

.report { background: #fff; border: 1px solid var(--rule); border-radius: 6px; padding: 12px 20px; margin: 12px 0; text-align: left; }
.report h4 { margin: 4px 0 8px; font-size: 17px; }
.sentence { font-size: 17px; margin: 6px 0; }

.lead { margin-top: 24px; font-weight: 600; }
.multiple-choice .options { text-align: left; display: inline-block; }
.multiple-choice label { display: block; margin: 6px 0; cursor: pointer; }
.retry { color: #A33; }
.debrief summary { cursor: pointer; font-weight: 600; }
</style>
