<template>
  <!-- magpie's SliderInput (0–100) without a visible starting handle: the handle and its value appear
       only once the participant clicks on the line, so no position is suggested. -->
  <div class="rating" :class="{ unset: value === null }" @click="answered">
    <p>{{ question }}</p>
    <SliderInput :left="left" :right="right" :initial="initial" :tooltip="true"
                 @update:response="update" />
  </div>
</template>

<script>
const INITIAL = 50; // the hidden starting position

export default {
  name: 'Rating',
  props: {
    question: { type: String, required: true },
    value: { type: Number, default: null },
    left: { type: String, default: 'very unlikely' },
    right: { type: String, default: 'very likely' }
  },
  data: () => ({ initial: INITIAL, current: INITIAL }),
  methods: {
    update(v) {
      this.current = v;
      this.$emit('input', v);
    },
    // SliderInput reports only changes, so a first click exactly on the hidden starting position
    // would not count as an answer. Any click or drag that ends on the slider counts.
    answered(e) {
      if (e.target.closest('.vue-slider')) this.$nextTick(() => this.$emit('input', this.current));
    }
  }
};
</script>
