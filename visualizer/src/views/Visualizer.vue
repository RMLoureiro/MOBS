<template>
    <world-map></world-map>
</template>

<script>
import WorldMap from "../components/WorldMap.vue";

export default {
  name: "app",
  components: {
    WorldMap
  }
};
</script>

<style>
  * {
    text-transform: none !important;
  }

  ::-webkit-scrollbar {
    display: none;
  }
</style>
