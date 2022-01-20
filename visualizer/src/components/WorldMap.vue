<template>
  <div class="outer" @click="updateData">
    <div class="top">
      <div class="l">
        <div class="ltop">
        <canvas id="mapCanvas" ref="mapCanvas"></canvas>
        </div>
        <p></p>
        <div class="lbottom">
          <p><input type="file" @change="updateFile"/></p>
          <p>Timestamp: {{timestamp}}</p>
          <timestamp-slider v-model="step" :isRunning="isRunning" :maxStep="maxStep" :minStep="0"></timestamp-slider>
          <div style="text-align:center; margin:auto; width:75%;">
              <button v-if="!isRunning" v-bind:onclick="run" class="play-btn-ts">Play</button>
              <button v-if="isRunning" v-bind:onclick="stop" class="play-btn-ts">Pause</button>
              Playback Speed: <input type="number" @change="updateSpeed" :value="speed" :disabled="isRunning" min="1" max="999">
          </div>
        </div>
      </div>
      <div class="r">
        <hr>
        <p>
          <node-data :node="selectedNode"></node-data>
        </p>
        <hr>
        <p>
          <link-data :link="selectedLink"></link-data>
        </p>
        <hr>
      </div>
    </div>
  </div>
</template>

<script>
import Manager from "@/js/Manager";
import TimestampSlider from '../components/TimestampSlider.vue';
import NodeData from '../components/NodeData.vue';
import LinkData from '../components/LinkData.vue';

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
      selectedNode:null,
      selectedLink:null,
      speed:0
    };
  },
  mounted: function() {
    this.ctx = this.$refs.mapCanvas.getContext("2d");
    this.resizeCanvas();
    manager = new Manager(this.ctx);
    this.maxStep = 1;
    this.maxStep = Math.max(manager.timestamps.length - 1,1);
    // if(manager.timestamps.length > 0) {
    //   this.maxStep = manager.timestamps[manager.timestamps.length];
    // }
    
    manager.updateTimeStep(this.step);
    manager.setLoadCallback(() => (this.step = 0));
    manager.run();
    this.speed = window.speed;
    this.reader.onload = event => {
      const dynamicData = JSON.parse(event.target.result);
      const success = manager.loadDynamicData(dynamicData);
      if (success) {
        this.maxStep = manager.timestamps.length - 1;
        // this.maxStep = 1;
        // if(manager.timestamps.length > 0) {
        //   this.maxStep = manager.timestamps[manager.timestamps.length-1];
        // }
        console.log(manager.timestamps);
      }
      this.showLoadStatus(success);
    };
  },
  watch: {
    step: function(val) {
      manager.updateTimeStep(val);
      this.timestamp    = manager.getTimestamp();
      this.selectedNode = manager.getSelectedNode();
      this.selectedLink = manager.getSelectedLink();
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
      this.ctx.canvas.width = (document.body.clientWidth)*0.75;
      this.ctx.canvas.height = (document.body.clientHeight)*0.75;
    },
    updateFile: function(file) {
      console.clear();
      if (file === null) return;
      if(file.target.files.length === 0) return;
      let f = file.target.files[0];
      if (f.type !== "application/json") {
        this.showLoadStatus(false);
        return;
      }
      this.reader.readAsText(f);
    },
    showLoadStatus: function(success) {
      this.loadSuccess = success;
      this.snackbarVisible = true;
    },
    updateData: function() {
      this.selectedNode = manager.getSelectedNode();
      this.selectedLink = manager.getSelectedLink();
    },
    updateSpeed: function(e) {
      this.speed = parseFloat(e.target.value)
      window.speed = this.speed;
    }
  },
  components: {
    TimestampSlider,
    NodeData,
    LinkData
  }
};
</script>

<style>

.outer {
  height: 100%;
  width: 100%;
}

.ltop {
    height: 75vh;
}

.l {
  float: left;
}

.r {
  margin-left: 75vw;
}

.play-btn-ts {
    color: white;
    padding: 5px 10px;
    border-radius: 4px;
    /*text-shadow: 0 1px 1px rgba(0, 0, 0, 0.2);*/
    background-color: #3700B3;
    font-family: "Roboto", sans-serif;
    font-size: 16px;
    margin: auto;
}

.play-btn-ts {
    background-color: #6200EE;
}

</style>
