<template>
  <!-- The ten patches counted by harvest (high/low) and ralocrop: the colonist's note, and the
       summary the fields group sees after the last patch. Counts are computed from trials/patches.csv. -->
  <div class="table-block">
    <h4>{{ title }}</h4>
    <table>
      <thead>
        <tr>
          <th>Harvest of xeliherb <XeliIcon /></th>
          <th>Without ralocrop</th>
          <th>With ralocrop <RaloIcon /></th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="row in rows" :key="row.label">
          <td>{{ row.label }}</td>
          <td>{{ count(row.without) }}</td>
          <td>{{ count(row.with) }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
import XeliIcon from './XeliIcon.vue';
import RaloIcon from './RaloIcon.vue';

const HIGH_KG = 30; // high-yield patches weigh 44–49 kg, low-yield ones 10–15 kg

export default {
  name: 'CrossTable',
  components: { XeliIcon, RaloIcon },
  props: {
    title: { type: String, required: true },
    patches: { type: Array, required: true }
  },
  computed: {
    rows() {
      return [['High', (p) => p.kg >= HIGH_KG], ['Low', (p) => p.kg < HIGH_KG]].map(([name, inRow]) => {
        const ps = this.patches.filter(inRow);
        const kg = ps.map((p) => p.kg);
        return {
          label: `${name} (${Math.min(...kg)}–${Math.max(...kg)} kg)`,
          without: ps.filter((p) => !p.ralocrop).length,
          with: ps.filter((p) => p.ralocrop).length
        };
      });
    }
  },
  methods: {
    count: (n) => `${n} ${n === 1 ? 'patch' : 'patches'}`
  }
};
</script>
