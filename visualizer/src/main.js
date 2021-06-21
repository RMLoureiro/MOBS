import { createApp } from "vue";
import App from './App.vue';
import router from "./router";
import store from "./store/index";

window.speed = 3;

window.setSpeed = function(x) {
    window.speed = x;
    console.log("Playback speed set to "+x);
}

const app = createApp(App);

app.use(store);

app.use(router);

app.mount('#app');

