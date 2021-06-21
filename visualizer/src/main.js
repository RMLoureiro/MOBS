import { createApp } from "vue";
import App from './App.vue';
import router from "./router";
import store from "./store/index";


//import Vuetify from "vuetify/lib";
//import { Ripple } from "vuetify/lib/directives";
//import "vuetify/src/stylus/app.styl";


window.setSpeed = function(x) {
    window.speed = x;
    console.log("Playback speed set to "+x);
}

const app = createApp(App);

app.use(store);

app.use(router);

app.mount('#app');

