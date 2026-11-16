<template>
  <!-- The colonist's report and what the researchers found, in boxes of the same layout and type size,
       so that the ground truth is as visible as the sentence it is judged against. -->
  <div>
    <p>
      At harvest time, another colonist gathered <strong>xeliherb</strong> from patches across the plains, weighed each harvest and
      noted whether ralocrop grew in each. Afterwards, the colonist sends you this report:
    </p>
    <div class="report">
      <h4>Report on the xeliherb patches</h4>
      <p class="sentence">{{ condition.sentence }}</p>
    </div>
    <p>
      Some years later, the colony's researchers studied the plains. The colonist had observed correctly: ralocrop
      does grow more often in high-yield patches. The researchers also found out why the two plants occur together:
    </p>
    <div class="report">
      <h4>What the researchers found</h4>
      <p class="sentence">{{ findings[condition.truth] }}</p>
    </div>
  </div>
</template>

<script>
export default {
  name: 'ReportAndFinding',
  props: {
    condition: { type: Object, required: true }
  },
  data: () => ({
    findings: {
      H: 'Fertile soil makes both plants grow better, but neither plant affects the other’s growth.',
      G: 'Ralocrop makes xeliherb grow better.'
    }
  })
};
</script>
