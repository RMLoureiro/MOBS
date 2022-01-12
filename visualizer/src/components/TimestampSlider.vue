<template>
  <div>
    <div class="my-slider-wrapper" style="width:75%;margin:auto;display:flex;">
    <button v-bind:onclick="decrementStep" class="ts-ctrl-btn"> - </button>
    <div class="my-slider" style="width:80%;margin:auto;">
      <Slider v-model="value" :min="minStep" :max="maxStep" @update="$emit('input', newValue);" :tooltips="tooltip" />
    </div>
    <button v-bind:onclick="incrementStep" class="ts-ctrl-btn"> + </button>
  </div>
  </div>
</template>

<script>
  import Slider from '@vueform/slider'

  export default {
    name: "timestampSlider",
    props: {
        modelValue: {
          type: Number,
          required: true
        },
        minStep: {
          type: Number,
          required: true
        },
        maxStep: {
          type: Number,
          required: true
        },
        isRunning: {
          type: Boolean,
          required: true
        }
    },
    data() {
      return {
        value:0,
        tooltip:false
      }
    },
    watch: {
      modelValue: function(newValue) {
        this.value = newValue;
      },
      value: function(newValue) {
        this.$emit("update:modelValue", newValue);
      }
    },
    components: {
      Slider,
    },
    methods: {
        incrementStep: function() {
          let newValue = this.modelValue === this.maxStep ? this.modelValue : this.modelValue + window.speed;
          this.$emit("update:modelValue", newValue);
        },
        decrementStep: function() {
          let newValue = this.modelValue === this.minStep ? this.modelValue : this.modelValue - window.speed;
          this.$emit("update:modelValue", newValue);
        }
    },
    mounted: function() {
        setInterval(() => {
            if (!this.isRunning) return;
            this.incrementStep();
        }, 50);
    }
  }
</script>

<style src="@vueform/slider/themes/default.css">
</style>
<style>
    .ts-ctrl-btn {
        color: white;
        height: 32px;
        width: 32px;
        font-family: "Roboto", sans-serif;
        font-size: 18px;
        background-color: #6200EE;
        border-radius: 50%;
    }

    .ts-ctrl-btn:hover {
        background-color: #3700B3;
    }
</style>
