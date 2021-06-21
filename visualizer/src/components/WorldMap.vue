<template>
  <div class="outer">
    <div class="top">
      <canvas id="mapCanvas" ref="mapCanvas"></canvas>
    </div>
    <div class="bottom">
      <p><button>Upload JSON Output</button></p>
      <p>Timestamp: {{timestamp}}</p>
      <timestamp-slider v-model="step" :isRunning="isRunning" :maxStep="maxStep" :minStep="0"></timestamp-slider>
      <button v-if="!isRunning" v-bind:onclick="run">Play</button>
      <button v-if="isRunning" v-bind:onclick="stop">Pause</button>
    </div>
  </div>
</template>

<script>
import Manager from "@/js/Manager";
import TimestampSlider from '../components/TimestampSlider.vue';

let manager;

export default {
  name: "worldMap",
  data: function() {
    return {
      ctx: null,
      step: 0,
      maxStep: 100,
      timestamp: 0,
      isRunning: false,
      reader: new FileReader(),
      snackbarVisible: false,
      loadSuccess: true,
    };
  },
  mounted: function() {
    this.ctx = this.$refs.mapCanvas.getContext("2d");
    this.resizeCanvas();
    manager = new Manager(this.ctx);
    this.maxStep = Math.max(manager.timestamps.length - 1,1);
    manager.updateTimeStep(this.step);
    manager.setLoadCallback(() => (this.step = 0));
    manager.run();
    this.reader.onload = event => {
      const dynamicData = JSON.parse(event.target.result);
      const success = manager.loadDynamicData(dynamicData);
      if (success) {
        this.maxStep = manager.timestamps.length - 1;
      }
      this.showLoadStatus(success);
    };
  },
  watch: {
    step: function(val) {
      manager.updateTimeStep(val);
      this.timestamp = manager.getTimestamp();
    }
  },
  methods: {
    run: function() {
      this.isRunning = true;
    },
    stop: function() {
      this.isRunning = false;
    },
    resizeCanvas: function() {
      this.ctx.canvas.width = document.body.clientWidth;
      this.ctx.canvas.height = (document.body.clientHeight)*0.75;
    },
    updateFile: function(file) {
      console.clear();
      if (file === null) return;
      if (file.type !== "application/json") {
        this.showLoadStatus(false);
        return;
      }
      this.reader.readAsText(file);
    },
    showLoadStatus: function(success) {
      this.loadSuccess = success;
      this.snackbarVisible = true;
    }
  },
  components: {
    TimestampSlider
  }
};
</script>

<style>

.outer {
  height: 100%;
  width: 100%;
}

</style>
