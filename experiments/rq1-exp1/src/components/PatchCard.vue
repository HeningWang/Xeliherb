<template>
  <!-- The kind of patches of the participant's group: scattered wild patches on the plains (observational)
       or identical beds in a fenced test field (interventional). Ralocrop grows in five patches and four patches
       give a high yield; in the picture the two are unrelated (two high-yield patches with ralocrop, two without),
       so it shows what the patches look like without giving evidence about ralocrop. -->
  <!-- On the critical screen (caption), the card is a verbal reminder without the picture, so that the picture
       cannot mix with the evidence below it. -->
  <div v-if="caption" class="patch-card reminder" :class="info">
    <p v-html="reminder" />
  </div>
  <div v-else class="patch-card" :class="info">
    <p class="way">{{ info === 'obs' ? 'Wild patches' : 'Test field' }}</p>
    <div v-html="picture" />
    <p class="icon-legend" v-html="legend" />
    <p>{{ text }}</p>
  </div>
</template>

<script>
import { xeliYield, ralocrop, icon } from './plants';

const RALO_AT = [0, 2, 3, 6, 9];
const HIGH_AT = [0, 1, 3, 5];
const WILD = [ // x, y, size, soil
  [5, 6, 30, '#E5E5E5'], [46, 14, 34, '#D1D1D1'], [91, 4, 27, '#ECECEC'], [128, 12, 32, '#C8C8C8'], [167, 3, 26, '#DCDCDC'],
  [11, 50, 34, '#C8C8C8'], [55, 56, 28, '#ECECEC'], [91, 46, 33, '#DCDCDC'], [135, 54, 28, '#E5E5E5'], [166, 44, 29, '#D1D1D1']
];
const FENCE = '<rect x="5" y="5" width="186" height="82" rx="2" fill="none" stroke="#666666" stroke-width="2"/>' +
  [5, 51.5, 98, 144.5, 191].map((fx) =>
    `<rect x="${fx - 2}" y="2" width="4" height="8" fill="#666666"/><rect x="${fx - 2}" y="82" width="4" height="8" fill="#666666"/>`).join('');

const COVER_TEXT = {
  obs: 'Xeliherb grows wild on the plains. The colonists mark these patches on a map, together with any wild ralocrop.',
  int: 'To learn how xeliherb grows best, the colonists grow it in a test field, and in some patches they also plant ralocrop.'
};
const REMINDER = {
  obs: 'Remember: these are <strong>wild patches</strong>. Nobody planted the ralocrop; it grew there by itself.',
  int: 'Remember: this is the <strong>test field</strong>. The colonists chose five of the ten patches and planted ralocrop in them.'
};

function picture(info) {
  const wild = info === 'obs';
  const cells = Array.from({ length: 10 }, (_, k) => {
    const [x, y, size, soil] = wild ? WILD[k] : [17 + (k % 5) * 34, 16 + Math.floor(k / 5) * 34, 27, '#DADADA'];
    const ralo = RALO_AT.includes(k);
    const plant = ralo ? ralocrop(x + size * 0.04, y + size * 0.12, 0.62) : '';
    const xeli = xeliYield(x + size * (ralo ? 0.62 : 0.5), y + size * 0.85, HIGH_AT.includes(k));
    const rx = wild ? size * 0.38 : 3; // wild patches are rounded blobs, field beds are square
    return `<rect x="${x}" y="${y}" width="${size}" height="${size}" rx="${rx}" fill="${soil}" stroke="#B3B3B3"/>${plant}${xeli}`;
  }).join('');
  const label = wild ? 'ten wild patches on the plains' : 'ten patches in a fenced test field';
  return `<svg viewBox="0 0 196 92" class="patch-picture" role="img" aria-label="${label}">${wild ? '' : FENCE}${cells}</svg>`;
}

export default {
  name: 'PatchCard',
  props: {
    info: { type: String, required: true }, // 'obs' or 'int'
    caption: { type: Boolean, default: false } // the critical screen shows a verbal reminder instead of the card
  },
  computed: {
    picture() { return picture(this.info); },
    reminder() { return REMINDER[this.info]; },
    text() { return COVER_TEXT[this.info]; },
    legend() {
      return `${icon(xeliYield(9, 19, true), 18)} high yield of xeliherb ` +
        `<span>${icon(xeliYield(7, 18, false), 14)} low yield of xeliherb</span> ` +
        `<span>${icon(ralocrop(2, 0, 0.75), 14)} ralocrop</span>`;
    }
  }
};
</script>
