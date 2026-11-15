<template>
  <Experiment title="Colonists of Xelifan-3">
    <!-- A study link without a valid ?condition= never starts the study (see README). -->
    <Screen v-if="!list" title="This study cannot start">
      <p>The study link is incomplete. Please return the study on Prolific and contact
        <a :href="'mailto:' + $magpie.contactEmail">{{ $magpie.contactEmail }}</a>;
        you will not be penalised.</p>
    </Screen>

    <template v-else>
      <!-- Screen 1. Welcome and consent -->
      <Screen>
        <p>
          Welcome, and thank you for your interest in this study! In this short study, you will read a fictitious
          story about colonists on a distant planet and then twelve sentences that colonists said. For each sentence,
          please judge whether the colonist contradicts themselves, that is, says two things that cannot both be
          true. Whether what the colonist says is true does not matter; please pay attention to small differences in
          wording. There are no right or wrong answers: we are interested in your own judgement.
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
        <p>The study takes about four minutes.</p>
        <button @click="$magpie.nextScreen()">I agree, start the study</button>
      </Screen>

      <!-- Screen 2. Cover story of Experiment 1, with one sentence that introduces the plants of items B–D -->
      <Screen>
        <p>
          You are one of a group of colonists who have just settled on a distant planet called Xelifan-3. To survive
          there, the colony depends on a plant called <strong>xeliherb</strong>, which grows only on Xelifan-3. The colonists have not
          yet started to cultivate <strong>xeliherb</strong>, so for now they gather it where it grows wild, in patches scattered
          across the plains near the settlement. Some patches give a high yield of <strong>xeliherb</strong> and others a low yield,
          and the colonists would like to gather as much as possible. Another wild plant, ralocrop, grows in some of
          the patches. Other wild plants grow on the plains too, among them dolberry, mirathorn, tasselroot,
          vennagrass, quellbean and brindlevine.
        </p>
        <CoverPicture />
        <button @click="$magpie.nextScreen()">Continue</button>
      </Screen>

      <!-- Screens 3–14. One sentence per screen, in the order drawn for this participant -->
      <Screen v-for="(s, k) in sentences" :key="s.item">
        <Slide>
          <p class="quote">A colonist says: “{{ s.text }}”</p>
          <Rating v-model="ratings[s.item]" question="Does the colonist contradict themselves?"
                  left="not at all" right="completely" />
          <button v-if="ratings[s.item] !== null" @click="save({ rating: ratings[s.item] })">Continue</button>
          <Record :data="{ screen: 'sentence', list: list.condition, position: k + 1, item: s.item, type: s.type,
                           text: s.text, previous_type: k > 0 ? sentences[k - 1].type : null,
                           first_critical: s.item === firstCritical }" />
        </Slide>
      </Screen>

      <!-- Screen 15. Thanks and debriefing; the button submits the data -->
      <Screen>
        <h3>The experiment is finished. Thank you for taking part!</h3>
        <details class="debrief">
          <summary>About this study (click to read more)</summary>
          <p>
            The planet, the colony and its plants were invented for this study, so that nobody knew anything about
            them beforehand.
          </p>
          <p>
            When two things often occur together, people tend to conclude that one causes the other. But occurring
            together does not show this on its own: the second could affect the first, or both could depend on a
            third thing. For example, ice-cream sales and sunburn go together because both depend on sunny weather,
            not because ice cream causes sunburn.
          </p>
          <p>
            In this study, you rated twelve sentences. Some of them really say two things that cannot both be true.
            Others only seem to, because the first part suggests something that the second part then takes back:
            “I ate some of the cake” suggests that I did not eat all of it, but “I ate some of the cake, in fact all of
            it” is not a contradiction. Four of the sentences were about the plants of the planet, in different
            wordings. We want to know whether a sentence saying that two things occur together also says that one
            causes the other, or only suggests it, in which case the speaker can take it back without contradicting
            themselves.
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
import lists from '../trials/lists.csv';
import critical from '../trials/critical.csv';
import fillers from '../trials/fillers.csv';
import CoverPicture from './components/CoverPicture.vue';
import Rating from './components/Rating.vue';

// Each list runs as its own Prolific study whose link names the list: ?condition=list1|list2.
// A list assigns the association or the causal version to each of the four critical items (Latin square).
const requested = new URL(window.location).searchParams.get('condition');
const list = lists.find((l) => l.condition === requested) || null;

// The twelve sentences of this list: four critical items in their assigned version, and the eight fillers.
function listSentences(l) {
  const crit = critical.map((c) => ({ item: c.item, type: l[c.item], text: c[l[c.item]] }));
  return [...crit, ...fillers.map((f) => ({ item: f.item, type: f.type, text: f.text }))];
}

const isCritical = (s) => s.type === 'association' || s.type === 'causal';

// The order is random per participant, with three constraints: the first two sentences are fillers of
// different types, two critical items are never adjacent, and the last sentence is a filler.
function orderOk(o) {
  if (isCritical(o[0]) || isCritical(o[1]) || o[0].type === o[1].type) return false;
  if (isCritical(o[o.length - 1])) return false;
  return o.every((s, i) => i === 0 || !(isCritical(s) && isCritical(o[i - 1])));
}

function drawOrder(sentences) {
  for (;;) {
    const o = _.shuffle(sentences);
    if (orderOk(o)) return o;
  }
}

export default {
  name: 'App',
  components: { CoverPicture, Rating },
  data() {
    const sentences = list ? drawOrder(listSentences(list)) : [];
    return {
      list,
      sentences,
      ratings: Object.fromEntries(sentences.map((s) => [s.item, null]))
    };
  },
  computed: {
    firstCritical() { // the first critical item, which no earlier critical item can have affected
      const s = this.sentences.find(isCritical);
      return s ? s.item : null;
    }
  },
  methods: {
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

.scene { width: 100%; height: auto; display: block; margin: 12px 0 4px; }
.legend { display: flex; gap: 20px; font-size: 14px; align-items: center; margin-bottom: 8px; }
.legend span { display: inline-flex; align-items: center; gap: 6px; }
.icon { vertical-align: middle; }

.quote { font-size: 17px; margin: 24px 0; }
.rating { margin: 24px 0; }
.rating.unset .vue-slider-dot, .rating.unset .vue-slider-dot *, .rating.unset .vue-slider-process { visibility: hidden; }
.rating .vue-slider-rail { cursor: pointer; }
.debrief summary { cursor: pointer; font-weight: 600; }
</style>
