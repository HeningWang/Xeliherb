<template>
  <!-- The plains with wild xeliherb and ralocrop scattered at random, without patch boundaries, so that
       the picture shows no comparison between patches. A fixed seed gives every participant the same picture,
       and a plant is redrawn when it would overlap another one. -->
  <div>
    <div v-html="svg" />
    <div class="legend">
      <span><XeliIcon /> xeliherb</span>
      <span><RaloIcon /> ralocrop</span>
    </div>
  </div>
</template>

<script>
import XeliIcon from './XeliIcon.vue';
import RaloIcon from './RaloIcon.vue';

const LINE = '#2B2A30';

function rng(seed) { // mulberry32
  let a = seed + 0x9e3779b9;
  return () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const xeliherb = (x, y) => [-4.5, 0, 4.5].map((dx) =>
  `<path d="M${x},${y} q${dx},-8 ${dx * 1.4},-15" stroke="#4F7A4A" stroke-width="1.6" fill="none" stroke-linecap="round"/>`).join('');

const ralocrop = (x, y) =>
  `<line x1="${x}" y1="${y}" x2="${x}" y2="${y - 15}" stroke="${LINE}" stroke-width="1.4"/>` +
  `<circle cx="${x}" cy="${y - 20}" r="6.5" fill="#E07A1F" stroke="${LINE}" stroke-width="1.4"/>`;

function picture() {
  const r = rng(99);
  const boxes = [];
  const place = (n, x0, xw, y0, yh, height) => {
    const out = [];
    for (let tries = 0; out.length < n && tries < 5000; tries++) {
      const x = x0 + r() * xw;
      const y = y0 + r() * yh;
      const box = [x - 9, y - height - 2, x + 9, y + 2];
      if (boxes.some((b) => box[0] < b[2] && b[0] < box[2] && box[1] < b[3] && b[1] < box[3])) continue;
      boxes.push(box);
      out.push([x, y]);
    }
    return out;
  };
  const ralos = place(6, 60, 840, 80, 130, 27).map(([x, y]) => ralocrop(x, y));
  const herbs = place(70, 20, 920, 60, 150, 16).map(([x, y]) => xeliherb(x, y));
  return `<svg viewBox="0 0 960 230" class="scene" role="img" aria-label="The plains with wild xeliherb and ralocrop">
    <rect x="0.75" y="0.75" width="958.5" height="228" rx="6" fill="#fff" stroke="${LINE}" stroke-width="1.2"/>
    <path d="M0,50 Q240,30 480,48 T960,40" stroke="#B5B2A8" fill="none" stroke-width="1.2"/>
    ${herbs.join('')}${ralos.join('')}</svg>`;
}

export default {
  name: 'CoverPicture',
  components: { XeliIcon, RaloIcon },
  data: () => ({ svg: picture() })
};
</script>
