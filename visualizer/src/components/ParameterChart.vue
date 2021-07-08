<template>
  <div style="height:50%;width: 100%;margin:auto;text-align:center;">
    Select one or more statistics to display in the graph:
    <vue3-chart-js
        ref = chartRef
        :type="'line'"
        :data="chart.data"
        @before-render="beforeRenderLogic"
    ></vue3-chart-js>
  </div>
</template>

<script>
import { ref } from 'vue';
import Vue3ChartJs from '@j-t-mcc/vue3-chartjs';

export default {
  components: {
    Vue3ChartJs,
  },
  props: {
    data: Object
  },
  setup (props) {
    const chartRef = ref(null);

    const chart = {
      data: props.data
    }

    const beforeRenderLogic = (event) => {
      //...
      //if(a === b) {
      //  event.preventDefault()
      //}   
    }  

    const updateChart = () => {
      chartRef.value.update();
    }

    return {
      chart,
      beforeRenderLogic,
      chartRef,
      updateChart
    }
  },
}
</script>