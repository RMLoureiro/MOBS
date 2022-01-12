<template>
  <div id="app">
    <app-header />
    <router-view />
  </div>
</template>

<script>
import appHeader from './components/Header.vue'

export default {
  name: 'layout',
  components: {
    appHeader
  }
}
</script>

<style>
  @import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
  html,
  #app {
      min-height: 100vh;
      font-family: "Roboto", sans-serif;
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
      text-align: center;
      font-size: 18px;
  }
  a {
      font-weight: bold;
      color: #000000;
      text-decoration: none;
  }
  a:hover {
      text-decoration: underline;
      color: #3700B3;
  }
  a.router-link-exact-active {
      color: #6200EE;
  }
  input[type='text'] {
      font-family: "Roboto", sans-serif;
      font-size: 18px;
  }
  input[type='text']:focus {
      border:1px solid #03DAC6;
      box-shadow: 0 0 10px #018786;
  }
  input[type='checkbox'] {
      background-color: #6200EE;
      transform: scale(1.5);
  }
  html {
      background-color: #eeeeee;
      background-attachment: fixed;
  }
</style>
