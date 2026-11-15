<template>
  <!-- The fields group's raw evidence: all ten patch columns from the start, filled in the order of gathering.
       Each harvest is a bar proportional to its weight above the weight in kg; ralocrop is its symbol or a dash. -->
  <div class="table-block patches">
    <table>
      <thead>
        <tr><th></th><th v-for="(p, k) in patches" :key="k">Patch {{ k + 1 }}</th></tr>
      </thead>
      <tbody>
        <tr>
          <td>Harvest of xeliherb <XeliIcon /></td>
          <td v-for="(p, k) in patches" :key="k" :class="k < shown ? 'harvest' : 'pending'">
            <template v-if="k < shown">
              <div class="bar-wrap"><div class="bar" :style="{ height: Math.round(p.kg / 50 * 56) + 'px' }" /></div>
              <div>{{ p.kg }} kg</div>
            </template>
          </td>
        </tr>
        <tr>
          <td>Ralocrop <RaloIcon /></td>
          <td v-for="(p, k) in patches" :key="k" :class="k < shown ? 'ralo' : 'pending'">
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

export default {
  name: 'PatchTable',
  components: { XeliIcon, RaloIcon },
  props: {
    patches: { type: Array, required: true }, // in the order of gathering
    shown: { type: Number, required: true }
  }
};
</script>
