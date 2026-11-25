<template>
  <!-- What the participant learns about the ten patches, by vehicle: gathering them one by one (own),
       the colonist's report with the patch table and summary (data), or the colonist's sentence (verbal). -->
  <div>
    <template v-if="condition.vehicle === 'own'">
      <p>
        At harvest time, you gather xeliherb from one patch after another. For each patch, you weigh the harvest
        and note whether ralocrop grows there. Click the button to gather the next patch.
      </p>
      <PatchTable :patches="patches" :shown="shown" :info="condition.info" />
      <CrossTable v-if="shown === patches.length" title="Summary of the patches" :patches="patches" :info="condition.info" />
    </template>

    <template v-else>
      <p>
        At harvest time, another colonist gathered xeliherb from the ten patches, weighed each harvest and noted
        whether ralocrop grew there. Afterwards, the colonist sends you this report:
      </p>
      <div v-if="condition.vehicle === 'data'" class="report">
        <h4>Report on the xeliherb patches</h4>
        <PatchTable :patches="patches" :shown="patches.length" :info="condition.info" />
        <CrossTable title="Summary of the patches" :patches="patches" :info="condition.info" />
      </div>
      <div v-else class="report note">
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
    shown: { type: Number, default: 10 }
  }
};
</script>
