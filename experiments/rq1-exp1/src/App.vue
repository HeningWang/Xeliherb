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
          story about colonists on a distant planet and then answer a few questions about it. Please read the story
          carefully. There are no right or wrong answers: we are interested in your own judgement.
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

      <!-- Screen 2. Cover story -->
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

      <!-- Screen 3. The evidence, and belief in G (main measure) -->
      <Screen>
        <Slide>
          <Evidence :condition="condition" :patches="harvestOrder" :shown="shown" with-intro />
          <button v-if="!allGathered" @click="gather">Gather the next patch</button>
          <template v-else>
            <Rating v-model="answers.G" question="How likely is it that ralocrop makes xeliherb grow better?" />
            <button v-if="answers.G !== null" @click="save({ G: answers.G, harvest_order: harvestOrderText,
                                                             reveal_ms: revealMs.join('|') || null, gather_ms: gatherMs })">Continue</button>
          </template>
          <Record :data="trialInfo('evidence')" />
        </Slide>
      </Screen>

      <!-- Screen 4. R, H and N, one slider each, in random order -->
      <Screen>
        <Slide>
          <Evidence :condition="condition" :patches="harvestOrder" />
          <p class="lead">
            Besides ralocrop making xeliherb grow better, the two plants could occur together for other reasons.
            How likely is each of these explanations?
          </p>
          <Rating v-for="a in alternatives" :key="a.id" v-model="answers[a.id]" :question="a.text" />
          <button v-if="alternatives.every((a) => answers[a.id] !== null)"
                  @click="save({ R: answers.R, H: answers.H, N: answers.N, alternatives_order: alternativesOrderText })">
            Continue
          </button>
          <Record :data="trialInfo('alternatives')" />
        </Slide>
      </Screen>

      <!-- Screen 5. Perceived strength of the association (C) -->
      <Screen>
        <Slide>
          <Evidence :condition="condition" :patches="harvestOrder" />
          <Rating v-model="answers.C" question="How strongly do high xeliherb yield and ralocrop go together?"
                  left="not at all" right="very strongly" />
          <button v-if="answers.C !== null" @click="save({ C: answers.C })">Continue</button>
          <Record :data="trialInfo('strength')" />
        </Slide>
      </Screen>

      <!-- Screen 6. Cultivation decision (D) -->
      <Screen>
        <Slide>
          <Evidence :condition="condition" :patches="harvestOrder" />
          <p class="lead">
            The colony is deciding whether to cultivate ralocrop together with xeliherb next season.
            Cultivating ralocrop takes extra land, water and work.
          </p>
          <Rating v-model="answers.D"
                  question="How likely is it that you would cultivate ralocrop together with xeliherb next season?" />
          <button v-if="answers.D !== null" @click="save({ D: answers.D })">Continue</button>
          <Record :data="trialInfo('decision')" />
        </Slide>
      </Screen>

      <!-- Screen 7. Thanks and debriefing; the button submits the data -->
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
            In this study, everyone learned about the same xeliherb patches: in most high-yield patches ralocrop
            grew, and in most low-yield patches it did not. Some participants gathered xeliherb from the patches
            themselves, some received a table of them from another colonist, and others received only a sentence
            from the colonist that described what was found, in different words. We want to know whether the way the
            same observation is passed on changes how strongly people believe that one thing causes the other. This
            helps us understand how people learn about causes from what others tell them, for example from news
            reports about health or science.
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
import patches from '../trials/patches.csv';
import CoverPicture from './components/CoverPicture.vue';
import Evidence from './components/Evidence.vue';
import Rating from './components/Rating.vue';

// Each group runs as its own Prolific study whose link names the group: ?condition=fields|note|1a|1b.
// Each batch publishes new studies whose links add &batch=N, so that the batch is recorded with every row.
const params = new URL(window.location).searchParams;
const requested = params.get('condition');
const BATCH = params.get('batch');
const DESIGN_VERSION = 'v1'; // change whenever the materials change

const ALTERNATIVES = [
  { id: 'R', text: 'Xeliherb makes ralocrop grow better, but ralocrop does not affect xeliherb.' },
  { id: 'H', text: 'Fertile soil makes both plants grow better, but neither plant affects the other.' },
  { id: 'N', text: 'The two plants do not affect each other and have no common cause; they grew together in these patches by chance.' }
];

export default {
  name: 'App',
  components: { CoverPicture, Evidence, Rating },
  data() {
    return {
      condition: conditions.find((c) => c.condition === requested) || null,
      harvestOrder: _.shuffle(patches), // the fields group gathers the patches in a random order
      alternatives: _.shuffle(ALTERNATIVES),
      shown: 0,
      revealMs: [],
      screenStart: Date.now(),
      answers: { G: null, R: null, H: null, N: null, C: null, D: null }
    };
  },
  computed: {
    allGathered() {
      return this.condition.evidence !== 'fields' || this.shown === patches.length;
    },
    harvestOrderText() { // the order of gathering, which only the fields group sees
      return this.condition.evidence === 'fields' ? this.harvestOrder.map((p) => p.patch).join('|') : null;
    },
    gatherMs() { // time from the first to the last gathering click, fields group only
      return this.revealMs.length ? this.revealMs[this.revealMs.length - 1] - this.revealMs[0] : null;
    },
    alternativesOrderText() {
      return this.alternatives.map((a) => a.id).join('|');
    }
  },
  methods: {
    gather() {
      this.revealMs.push(Date.now() - this.screenStart);
      this.shown += 1;
    },
    trialInfo(screen) {
      return { screen, condition: this.condition.condition, batch: BATCH, design_version: DESIGN_VERSION };
    },
    save(responses) {
      Object.assign(this.$magpie.measurements, responses);
      this.$magpie.saveAndNextScreen();
      this.screenStart = Date.now();
      window.scrollTo(0, 0);
    }
  }
};
</script>

<style>
:root { --ink: #2B2A30; --muted: #6b6875; --rule: #D7D8E2; color-scheme: light; }

.scene { width: 100%; height: auto; display: block; margin: 12px 0 4px; }
.legend { display: flex; gap: 20px; font-size: 14px; align-items: center; margin-bottom: 8px; }
.legend span { display: inline-flex; align-items: center; gap: 6px; }
.icon { vertical-align: middle; }

.report { background: #fff; border: 1px solid var(--rule); border-radius: 6px; padding: 12px 20px; margin: 12px 0; text-align: left; }
.report h4 { margin: 4px 0 8px; font-size: 17px; }
.report.note { max-width: 560px; margin-left: auto; margin-right: auto; }
.report.note .table-block { margin: 4px 0; }
.sentence { font-size: 17px; margin: 6px 0; }

.table-block { margin: 16px auto; max-width: 520px; }
.table-block h4 { margin: 0 0 6px; }
.table-block table { border-collapse: collapse; width: 100%; font-size: 15px; }
.table-block th { font-weight: 400; color: var(--muted); text-align: center; border-bottom: 1px solid var(--ink); padding: 4px 8px; }
.table-block td { text-align: center; padding: 5px 8px; }
.table-block th:first-child, .table-block td:first-child { text-align: left; }
.table-block tbody tr:last-child td { border-bottom: 1px solid var(--ink); }
.patches { max-width: none; overflow-x: auto; }
.patches table { width: auto; }
.patches td, .patches th { white-space: nowrap; min-width: 56px; padding: 5px 4px; }
.patches td.harvest { vertical-align: bottom; font-size: 14px; }
.patches .bar-wrap { height: 58px; display: flex; align-items: flex-end; justify-content: center; }
.patches .bar { width: 22px; background: #4F7A4A; border-radius: 2px 2px 0 0; }
.patches td.ralo { height: 36px; vertical-align: middle; }
.patches td.pending { background: #F2F1EC; border-left: 2px solid #fff; border-right: 2px solid #fff; }
.patches .none { color: var(--muted); }

.lead { margin-top: 24px; font-weight: 600; }
.rating { margin: 24px 0; }
.rating.unset .vue-slider-dot, .rating.unset .vue-slider-dot *, .rating.unset .vue-slider-process { visibility: hidden; }
.rating .vue-slider-rail { cursor: pointer; }
.debrief summary { cursor: pointer; font-weight: 600; }
</style>
