<template>
  <svg class="icon" width="16" height="22" viewBox="-8 -18 16 20" aria-label="xeliherb">
    <path v-for="dx in [-4.5, 0, 4.5]" :key="dx" :d="`M0,0 q${dx},-8 ${dx * 1.4},-15`"
          stroke="#4F7A4A" stroke-width="1.6" fill="none" stroke-linecap="round" />
  </svg>
</template>
