<template>
  <!-- The patch table of the own-observation and data-reported groups: all ten patch columns from the start, filled in the order of gathering.
       Each harvest is a bar proportional to its weight above the weight in kg; ralocrop is its symbol or a dash,
       and the columns of patches with ralocrop are tinted so that the two kinds of patch stand apart. -->
  <div class="table-block patches">
    <table>
      <thead>
        <tr><th></th><th v-for="(p, k) in patches" :key="k">Patch {{ k + 1 }}</th></tr>
      </thead>
      <tbody>
        <tr>
          <td>Harvest of xeliherb <XeliIcon /></td>
          <td v-for="(p, k) in patches" :key="k" :class="cellClass(p, k, 'harvest')">
            <template v-if="k < shown">
              <div class="bar-wrap"><div class="bar" :style="{ height: barHeight(p.kg) + 'px' }" /></div>
              <div>{{ p.kg }} kg</div>
            </template>
          </td>
        </tr>
        <tr>
          <td>{{ info === 'int' ? 'Ralocrop planted' : 'Ralocrop' }} <RaloIcon /></td>
          <td v-for="(p, k) in patches" :key="k" :class="cellClass(p, k, 'ralo')">
            <template v-if="k < shown">
              <RaloIcon v-if="p.ralocrop" /><span v-else class="none">–</span>
            </template>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
import XeliIcon from './XeliIcon.vue';
import RaloIcon from './RaloIcon.vue';

const MAX_KG = 50;
const BAR_PX = 90; // bar height of a 50 kg harvest

export default {
  name: 'PatchTable',
  components: { XeliIcon, RaloIcon },
  props: {
    patches: { type: Array, required: true }, // in the order of gathering
    info: { type: String, required: true }, // 'obs' or 'int'
    shown: { type: Number, required: true }
  },
  methods: {
    barHeight: (kg) => Math.round(kg / MAX_KG * BAR_PX),
    cellClass(p, k, row) {
      if (k >= this.shown) return 'pending';
      return p.ralocrop ? `${row} with-ralo` : row;
    }
  }
};
</script>
