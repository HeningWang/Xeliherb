<template>
  <!-- The fields group's ten harvests regrouped after the last patch: the patches with ralocrop on one side and
       those without on the other, each side ordered from the largest to the smallest harvest. -->
  <div class="table-block sorted">
    <h4>The same patches, with and without ralocrop</h4>
    <div class="groups">
      <div v-for="g in groups" :key="g.label" :class="['group', { 'with-ralo': g.ralocrop }]">
        <div class="group-label">{{ g.label }} <RaloIcon v-if="g.ralocrop" /></div>
        <div class="bars">
          <div v-for="(p, k) in g.patches" :key="k" class="col">
            <div class="bar-wrap"><div class="bar" :style="{ height: barHeight(p.kg) + 'px' }" /></div>
            <div class="kg">{{ p.kg }} kg</div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import RaloIcon from './RaloIcon.vue';

const MAX_KG = 50;
const BAR_PX = 90; // the same scale as the table of patches

export default {
  name: 'SortedPatches',
  components: { RaloIcon },
  props: {
    patches: { type: Array, required: true }
  },
  computed: {
    groups() {
      const byKg = [...this.patches].sort((a, b) => b.kg - a.kg);
      return [
        { label: 'With ralocrop', ralocrop: true, patches: byKg.filter((p) => p.ralocrop) },
        { label: 'Without ralocrop', ralocrop: false, patches: byKg.filter((p) => !p.ralocrop) }
      ];
    }
  },
  methods: {
    barHeight: (kg) => Math.round(kg / MAX_KG * BAR_PX)
  }
};
</script>
