<template>
  <div class="outer">
    <div class="top">
      <canvas id="mapCanvas" ref="mapCanvas"></canvas>
    </div>
    <div class="bottom">
      <p><button>Upload JSON Output</button></p>
      <p>Timestamp: {{timestamp}}</p>
      <div class="my-slider-wrapper" style="width:60%;margin:auto;text-align:center;display:flex;">
        <button v-bind:onclick="decrementStep"> - </button>
        <div class="my-slider" style="width:90%;margin:auto;">
          <Slider v-model="step" :min="0" :max="maxStep" @update="$emit('input', $event)" :tooltips="tooltip" />
        </div>
        <button v-bind:onclick="incrementStep"> + </button>
      </div>
      <button v-if="!isRunning" v-bind:onclick="run">Play</button>
      <button v-if="isRunning" v-bind:onclick="stop">Pause</button>
    </div>
  </div>
</template>

<script>
import Manager from "@/js/Manager";
import Slider from '@vueform/slider'

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
      tooltip:false
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
    setInterval(() => {
      if (!this.isRunning) return;
      this.incrementStep();
    }, 50);
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
    incrementStep: function() {
      this.step = this.step === this.maxStep ? this.step : this.step + window.speed;
    },
    decrementStep: function() {
      this.step = this.step === 0 ? this.step : this.step - window.speed;
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
    Slider
  }
};
</script>

<style src="@vueform/slider/themes/default.css">

.outer {
  height: 100%;
  width: 100%;
}

</style>
