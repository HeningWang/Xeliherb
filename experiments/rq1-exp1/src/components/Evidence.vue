<template>
  <!-- What the participant learns about the patches. withIntro=false gives the repeat on Screens 4–6,
       where the fields group sees all patches and the summary. -->
  <div>
    <template v-if="condition.evidence === 'fields'">
      <p v-if="withIntro">
        At harvest time, you walk across the plains and gather xeliherb from one patch after another.
        For each patch, you weigh the harvest and note whether ralocrop grows there.
        Click the button to gather the next patch.
      </p>
      <PatchTable :patches="patches" :shown="shown" />
      <CrossTable v-if="shown === patches.length" title="Summary of the patches" :patches="patches" />
    </template>

    <template v-else>
      <p v-if="withIntro">
        At harvest time, another colonist gathered <strong>xeliherb</strong> from patches across the plains, weighed each harvest and noted
        whether ralocrop grew in each. Afterwards, the colonist sends you this report:
      </p>
      <div v-if="condition.evidence === 'note'" class="report note">
        <CrossTable title="Report on the xeliherb patches" :patches="patches" />
      </div>
      <div v-else class="report">
        <h4>Report on the xeliherb patches</h4>
        <p class="sentence">{{ condition.report }}</p>
      </div>
    </template>
  </div>
</template>

<script>
import PatchTable from './PatchTable.vue';
import CrossTable from './CrossTable.vue';

export default {
  name: 'Evidence',
  components: { PatchTable, CrossTable },
  props: {
    condition: { type: Object, required: true },
    patches: { type: Array, required: true },
    shown: { type: Number, default: 10 },
    withIntro: { type: Boolean, default: false }
  }
};
</script>
