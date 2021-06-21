<template>
  <div>
    <div class="my-slider-wrapper" style="width:90%;margin:auto;display:flex;">
    <button v-bind:onclick="decrementStep"> - </button>
    <div class="my-slider" style="width:80%;margin:auto;">
      <Slider v-model="value" :min="minStep" :max="maxStep" @update="$emit('input', newValue);" :tooltips="tooltip" />
    </div>
    <button v-bind:onclick="incrementStep"> + </button>
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

<style src="@vueform/slider/themes/default.css"></style>