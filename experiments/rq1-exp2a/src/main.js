import Vue from 'vue';
import VueMagpie from 'magpie-base';
import App from './App.vue';
import magpieConfig from './magpie.config.js';

Vue.config.productionTip = false;

// Load magpie components
Vue.use(VueMagpie, magpieConfig);

// start app
new Vue({
  render: (h) => h(App)
}).$mount('#app');
