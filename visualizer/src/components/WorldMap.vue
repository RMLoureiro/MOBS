<template>
  <div>
    <canvas id="mapCanvas" ref="mapCanvas"></canvas>
  </div>
</template>

<script>
import Manager from "@/js/Manager";

let manager;

export default {
  name: "worldMap",
  data: function() {
    return {
      ctx: null,
      step: 0,
      maxStep: 0,
      timestamp: 0,
      isRunning: false,
      reader: new FileReader(),
      snackbarVisible: false,
      loadSuccess: true
    };
  },
  mounted: function() {
    this.ctx = this.$refs.mapCanvas.getContext("2d");
    this.resizeCanvas();
    manager = new Manager(this.ctx);
    this.maxStep = manager.timestamps.length - 1;
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
    // onResize: function() {
    //   if (this.ctx === null) return;
    //   this.resizeCanvas();
    // },
    resizeCanvas: function() {
      this.ctx.canvas.width = document.body.clientWidth;
      this.ctx.canvas.height = document.body.clientHeight;
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
    
  }
};
</script>

<style>
#mapCanvas {
  position: fixed;
  left: 0;
  top: 0;
  z-index: 0;
}
</style>
