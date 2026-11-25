<template>
  <!-- A compact stand-in for the 100 low-yield patches of the prediction question: six patches, an ellipsis
       and the count, without a map or a fence, so that the row says nothing about wild patches or a test field. -->
  <div v-html="svg" />
</template>

<script>
import { xeliYield } from './plants';

const cells = Array.from({ length: 6 }, (_, k) => {
  const x = 2 + k * 25;
  return `<rect x="${x}" y="2" width="20" height="20" rx="4" fill="#E3D5B5" stroke="#C4B48F"/>${xeliYield(x + 10, 19, false)}`;
}).join('');
const dots = [160, 168, 176].map((cx) => `<circle cx="${cx}" cy="12" r="1.8" fill="#6b6875"/>`).join('');
const label = '<text x="190" y="16" font-size="11" font-weight="700" font-family="sans-serif" fill="#2B2A30">100 patches</text>';

export default {
  name: 'LowPatches',
  data: () => ({
    svg: `<svg viewBox="0 0 256 24" class="low-patches" role="img" aria-label="100 patches with a low xeliherb yield and no ralocrop">${cells}${dots}${label}</svg>`
  })
};
</script>
