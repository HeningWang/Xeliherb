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

      <!-- Screen 2. Cover story. The text is the same in all six groups; the own-observation and data-reported
           groups also see a box with their kind of patches, the verbal-report groups see no kind of patch. -->
      <Screen>
        <p>
          You are one of a group of colonists who have just settled on a distant planet called Xelifan-3. The colony
          depends on a plant called <strong>xeliherb</strong> (shown as <XeliIcon />), which grows in patches on the
          plains near the settlement. Some patches give a <strong>high</strong> yield of xeliherb and others a
          <strong>low</strong> yield. Given its relevance for survival, obtaining a high yield of xeliherb is very important.
        </p>
        <p>Another plant, <strong>ralocrop</strong> (shown as <RaloIcon />), can also grow in these patches.</p>
        <PatchCard v-if="showsPatches" :info="condition.info" />
        <!-- Comprehension check: the participant can only continue after the correct answer. -->
        <div class="question-box">
          <p class="q"><span class="q-mark">Q:</span> Which plant does the colony depend on for its survival?</p>
          <MultipleChoiceInput :options="comprehensionOrder" @update:response="answerComprehension" />
          <p v-if="comprehension && comprehension !== COMPREHENSION_ANSWER" class="retry">
            That is not correct. Please read the story again and choose another answer.</p>
        </div>
        <button v-if="comprehension === COMPREHENSION_ANSWER"
                @click="save({ comprehension_first: comprehensionFirst, comprehension_errors: comprehensionErrors,
                               comprehension_order: comprehensionOrder.join('|') })">Continue</button>
        <Record :data="trialInfo('cover')" />
      </Screen>

      <!-- Screen 3. The evidence and the intervention prediction (main measure) -->
      <Screen>
        <Slide>
          <PatchCard v-if="showsPatches" :info="condition.info" caption />
          <Evidence :condition="condition" :patches="harvestOrder" :shown="shown" />
          <button v-if="!allGathered" @click="gather">Gather the next patch</button>
          <template v-else>
            <div class="question-box">
              <p class="context">
                Here are 100 other patches. Last season, all of them gave a <strong>low</strong> xeliherb yield,
                and none of them had ralocrop.
              </p>
              <LowPatches />
              <p class="context">The colonists are now going to plant ralocrop in all of these patches.</p>
              <Rating v-model="prediction" question="How many of them will give a high xeliherb yield this season?"
                      left="0" right="100" />
            </div>
            <button v-if="prediction !== null"
                    @click="save({ prediction, patch_order: patchOrderText,
                                   reveal_ms: revealMs.join('|') || null, gather_ms: gatherMs })">Continue</button>
          </template>
          <Record :data="trialInfo('critical')" />
        </Slide>
      </Screen>

      <!-- Screen 4. Cultivation policy, the decision of the earlier XeliHerb studies -->
      <Screen>
        <Slide>
          <div class="question-box">
            <p class="context">Cultivating ralocrop is costly: it needs extra resources, water and work.</p>
            <p class="q"><span class="q-mark">Q:</span> As a general policy, would you cultivate both xeliherb and
              ralocrop on the colony's fields, or only xeliherb?</p>
            <ForcedChoiceInput :options="policyOrder" @update:response="save({ policy: $event, policy_order: policyOrder.join('|') })" />
          </div>
          <Record :data="trialInfo('policy')" />
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
            In this study, everyone learned about the same ten xeliherb patches: in most high-yield patches ralocrop
            grew, and in most low-yield patches it did not. For some participants, ralocrop grew wild in these patches;
            for others, the colonists had planted it in a test field. Some participants gathered xeliherb from the
            patches themselves, some received a table of them from another colonist, and others received only a
            sentence from the colonist. We want to know whether the kind of evidence and the way it is passed on change
            how strongly people believe that one thing causes the other. This helps us understand how people learn
            about causes from what others tell them, for example from news reports about health or science.
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
import Evidence from './components/Evidence.vue';
import LowPatches from './components/LowPatches.vue';
import PatchCard from './components/PatchCard.vue';
import RaloIcon from './components/RaloIcon.vue';
import Rating from './components/Rating.vue';
import XeliIcon from './components/XeliIcon.vue';

// Each group runs as its own Prolific study whose link names the group, ?condition=obs-own|obs-data|obs-verbal|
// int-own|int-data|int-verbal. Each batch publishes new studies whose links add &batch=N, recorded with every row.
const params = new URL(window.location).searchParams;
const requested = params.get('condition');
const BATCH = params.get('batch');
const DESIGN_VERSION = 'v8'; // 2 x 3 design (information type x vehicle), blocking comprehension check, grey wild-patch box, verbal reminder on the critical screen; change whenever the materials change
const COMPREHENSION_ANSWER = 'Xeliherb';
const COMPREHENSION_OPTIONS = ['Xeliherb', 'Ralocrop', 'Neither of them'];
const POLICY_OPTIONS = ['Both xeliherb and ralocrop', 'Only xeliherb'];

export default {
  name: 'App',
  components: { Evidence, LowPatches, PatchCard, RaloIcon, Rating, XeliIcon },
  data() {
    return {
      condition: conditions.find((c) => c.condition === requested) || null,
      harvestOrder: _.shuffle(patches), // the order of the patch table, gathered or reported
      COMPREHENSION_ANSWER,
      comprehensionOrder: _.shuffle(COMPREHENSION_OPTIONS),
      comprehension: null,
      comprehensionFirst: null,
      comprehensionErrors: 0,
      policyOrder: _.shuffle(POLICY_OPTIONS),
      prediction: null,
      shown: 0,
      revealMs: [],
      screenStart: Date.now()
    };
  },
  computed: {
    showsPatches() { // the verbal-report groups learn nothing about the kind of patches
      return this.condition.vehicle !== 'verbal';
    },
    allGathered() {
      return this.condition.vehicle !== 'own' || this.shown === patches.length;
    },
    patchOrderText() { // the order of the patch table, which the verbal-report groups do not see
      return this.showsPatches ? this.harvestOrder.map((p) => p.patch).join('|') : null;
    },
    gatherMs() { // time from the first to the last gathering click, own-observation groups only
      return this.revealMs.length ? this.revealMs[this.revealMs.length - 1] - this.revealMs[0] : null;
    }
  },
  methods: {
    answerComprehension(answer) {
      if (this.comprehensionFirst === null) this.comprehensionFirst = answer;
      if (answer !== COMPREHENSION_ANSWER) this.comprehensionErrors += 1;
      this.comprehension = answer;
    },
    gather() {
      this.revealMs.push(Date.now() - this.screenStart);
      this.shown += 1;
    },
    trialInfo(screen) {
      const { condition, info, vehicle } = this.condition;
      return { screen, condition, info, vehicle, batch: BATCH, design_version: DESIGN_VERSION };
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
html, body { background: #fff; color: var(--ink); } /* a light page whatever the participant's system theme */

/* the box with the kind of patches: grey for the wild patches, green for the test field */
.patch-card { border: 2px solid var(--box-line); background: var(--box-fill); border-radius: 8px; padding: 12px 14px;
  max-width: 380px; margin: 16px auto; text-align: center; }
.patch-card.obs { --box-fill: #F3F3F3; --box-line: #8C8C8C; }
.patch-card.int { --box-fill: #EAF0E8; --box-line: #5C7457; }
.patch-card p { margin: 6px 0; font-size: 15px; }
.patch-card .way { font-weight: 900; }
.patch-card.reminder { max-width: 560px; }
.patch-card.reminder p { font-size: 16px; margin: 2px 0; }
.patch-picture { width: 100%; max-width: 300px; display: block; margin: 0 auto; }
.icon-legend { font-size: 13px !important; color: var(--muted); }
.icon-legend span { margin-left: 10px; white-space: nowrap; }
.low-patches { width: 100%; max-width: 340px; display: block; margin: 8px auto; }
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
.patches .bar-wrap { height: 92px; display: flex; align-items: flex-end; justify-content: center; }
.patches .bar { width: 26px; background: #4F7A4A; border-radius: 2px 2px 0 0; }
.patches td.ralo { height: 36px; vertical-align: middle; }
.patches td.with-ralo { background: #FBE6CF; }
.patches td.pending { background: #F7F7F7; border-left: 2px solid #fff; border-right: 2px solid #fff; }
.patches .none { color: var(--muted); }

.lead { margin-top: 24px; font-weight: 600; }
/* Questions and their response options sit in a framed box, apart from the evidence or report above them. */
.question-box { border: 2px solid var(--ink); border-radius: 8px; background: #F7F7FA; padding: 6px 24px 10px; margin: 28px auto 16px; max-width: 760px; text-align: center; }
.question-box .context { margin: 14px 0 4px; }
.q { font-weight: 600; font-size: 18px; margin: 14px 0 8px; }
.q-mark { font-weight: 700; margin-right: 4px; }
.question-box .multiple-choice { margin-bottom: 8px; }
.rating { margin: 24px 0; }
.question-box .rating { margin: 14px 0 18px; }
.question-box .rating + .rating { border-top: 1px solid var(--rule); padding-top: 12px; }
.multiple-choice .options { text-align: left; display: inline-block; }
.multiple-choice label { display: block; margin: 6px 0; cursor: pointer; }
.retry { color: #A33; }
.rating.unset .vue-slider-dot, .rating.unset .vue-slider-dot *, .rating.unset .vue-slider-process { visibility: hidden; }
.rating .vue-slider-rail { cursor: pointer; }
.debrief summary { cursor: pointer; font-weight: 600; }
</style>
