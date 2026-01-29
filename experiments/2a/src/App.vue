<template>
  <Experiment title="Decision-making experiment">
    <InstructionScreen :title="'Welcome'">
      In this short experiment you will first read a description about a
      fictitious context in which you are supposed to make an decision based on
      some information you received from others. Please read the context
      information very carefully and make your decision deliberately.
      <p></p>
      After your choice, we will ask you to explain your choice in a few words.
    </InstructionScreen>

    <template v-for="(trial, i) of items">
      <Screen :key="i">
        <Slide>
          <h2><strong>Context</strong></h2>

          <p>You are leading a group of colonists to a far away planet called Xelifan-3. The information you received from Mission Control when you departed months ago is this:</p>

          <div style="background-color:#FF9999"><strong>Information from Mission Control:</strong> <br> To survive on Xelifan-3, you need constant supply of the seeds of a plant called xeliherb, which grows exclusively on Xelifan-3.</div>

          <p>Prior to arrival on Xeliherb-3 your Science Team has been sent ahead to explore the planet. You have since lost contact with the Science Team, but you did receive one short report. Here is what the Science Team reported:</p>

          <div style="background-color:#AAAAFF"><strong>Information from Science Team:</strong> <br> A high yield of xeliherb is associated with the presence of ralocrop.
          </div>

          <p>
            Ralocrop is another plant, which is not exclusive to Xelifan-3.
            It can be found throughout the galaxy and is well-known.
            It is also well-known that the cultivation of ralocrop is costly (water, energy resources).
          </p>

          <p><strong>Question:</strong> {{ trial.taskQuestion }}</p>
          <ForcedChoiceInput
            :response.sync="$magpie.measurements.response"
            :options="['both xeliherb and ralocrop', 'only xeliherb']"
            @update:response="$magpie.saveAndNextScreen()"
          />
          <Record
            :data="{
              trialNR: i,
              itemNr: trial.itemNr,
              itemName: trial.itemName,
              condition: trial.condition
            }"
          />
        </Slide>
      </Screen>

      <Screen :key="i">
        <Slide>
          <p>
            Please give a short explanation for why you chose to cultivate
            {{ getPreviousResponse() }}!
          </p>
          <!-- <p style="color: grey"> -->
          <!--   (You need to enter at least 20 characters of text to proceed.) -->
          <!-- </p> -->
          <TextareaInput :response.sync="$magpie.measurements.justification" />
          <button
            v-if="
              $magpie.measurements.justification
            "
            @click="$magpie.saveAndNextScreen()"
          >
            Submit
          </button>
          <Record
            :data="{
                   trialNR: i+1,
                   itemNr: trial.itemNr,
                   itemName: trial.itemName,
                   condition: trial.condition
            }"
          />
        </Slide>
      </Screen>

      <Screen :key="i">
        <Slide>
          <p>
            Due to atmospheric conditions and technical problems you are about to lose contact to an outpost on the planet.
            You have only very little time to send a short message to the colonists in this outpost before all contact breaks off for potentially a long time.
            The colonists in the outpost know about xeliherb and its importance. They know the costs associated with cultivation of ralocrop.
            What they do not know is what your Science Team reported.
            <strong>You cannot forward the original report. </strong>
            <strong>Please type what you recall from the Science Team's report into the text box, so that the other colonists can make a decision like you did on their own!</strong>
          </p>
          <!-- <p style="color: grey"> -->
          <!--   (You need to enter at least 20 characters of text to proceed.) -->
          <!-- </p> -->
          <TextareaInput :response.sync="$magpie.measurements.reproduction" />
          <button
            v-if="
              $magpie.measurements.reproduction
            "
            @click="$magpie.saveAndNextScreen()"
          >
            Submit
          </button>
          <Record
            :data="{
                   trialNR: i+2,
                   itemNr: trial.itemNr,
                   itemName: trial.itemName,
                   condition: trial.condition
            }"
          />
        </Slide>
      </Screen>
    </template>

    <PostTestScreen />

    <SubmitResultsScreen />

  </Experiment>
</template>

<script>
import items from '../trials/items.csv';
import _ from 'lodash';

console.log("Hi, I'm Pilot 02a!")

export default {
  name: 'App',
  data() {
    // return { items: _.shuffle(items).slice(0, 1) };
    return { items: items.slice(0, 1) };
  },
  computed: {
    // Expose lodash to template code
    _() {
      return _;
    }
  },
  methods: {
    getPreviousResponse: function () {
      return this.$magpie.trialData[1][0].response;
    }
  }
};
</script>
