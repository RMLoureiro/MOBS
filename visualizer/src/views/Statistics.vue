<template>
    <section class="statistics">
        <parameter-chart :data="data" ref="chartRef"></parameter-chart>
        <p>
            Varying Parameter:
                <select v-model="view_parameter" @change="computeGraphData">
                    <option v-for="label in range_parameter_labels" v-bind:value="label" v-bind:key="label">
                        {{label}}
                    </option>
                </select>
        </p>
        <div v-for="param in other_parameters" v-bind:key="param.label">
            <p>
                {{param.label}}:
                    <select v-model="param.selected" @change="computeGraphData">
                        <option v-for="v in param.values" v-bind:value="v" v-bind:key="v">
                            {{v}}
                        </option>
                    </select>
            </p>
        </div>
    </section>
</template>

<script>
    import { mapActions, mapMutations, mapState } from "vuex";
    import ParameterChart from "../components/ParameterChart.vue";

    function randomColor(numOfSteps, step) {
        // This function generates vibrant, "evenly spaced" colours (i.e. no clustering). This is ideal for creating easily distinguishable vibrant markers in Google Maps and other apps.
        // Adam Cole, 2011-Sept-14
        // HSV to RBG adapted from: http://mjijackson.com/2008/02/rgb-to-hsl-and-rgb-to-hsv-color-model-conversion-algorithms-in-javascript
        var r, g, b;
        var h = step / numOfSteps;
        var i = ~~(h * 6);
        var f = h * 6 - i;
        var q = 1 - f;
        switch(i % 6){
            case 0: r = 1; g = f; b = 0; break;
            case 1: r = q; g = 1; b = 0; break;
            case 2: r = 0; g = 1; b = f; break;
            case 3: r = 0; g = q; b = 1; break;
            case 4: r = f; g = 0; b = 1; break;
            case 5: r = 1; g = 0; b = q; break;
        }
        var c = "#" + ("00" + (~ ~(r * 255)).toString(16)).slice(-2) + ("00" + (~ ~(g * 255)).toString(16)).slice(-2) + ("00" + (~ ~(b * 255)).toString(16)).slice(-2);
        return (c);
    }

    export default {
        data() {
            return {
                data: {
                    labels: [1,2,3,4,5],
                    datasets: [
                        {
                            label:"Parameter",
                            data:[1,2,3,4,5],
                            borderColor:'#234c75',
                            backgroundColor:'#2f659c'
                        }
                    ]
                },
                parameter_labels:[],
                statistic_labels:[],
                per_statistic_data:[],
                is_range_parameter:[],
                range_parameter_labels:[],
                view_parameter:"",
                other_parameters:[]
            };
        },
        components: {
            ParameterChart
        },
        mounted() {
            this.computeGraphInOut();

            Object.keys(this.parameters[0]).forEach(key => {
                this.parameter_labels.push(key);
                let num_equal_params = (this.parameters.filter(x => x[key] === this.parameters[0][key])).length;
                let res = num_equal_params != this.parameters.length;
                this.is_range_parameter.push(res);
                if(res) {
                    this.range_parameter_labels.push(key);
                    this.view_parameter = key;
                }
            });

            Object.keys(this.outputs[0]).forEach(key => {
                this.statistic_labels.push(key);
            });

            let i = 0;
            this.outputs.forEach(out_stats => {
                let key_index = 0;
                Object.keys(out_stats).forEach(key =>{
                    let v = out_stats[key];
                    if(i === 0) {
                        let json_obj = {
                            min:{value:v, parameters:this.parameters[i]},
                            max:{value:v, parameters:this.parameters[i]}
                        };
                        this.per_statistic_data.push(json_obj);
                    }
                    else {
                        let json_obj = this.per_statistic_data[key_index];
                        if(v < json_obj.min.value) {
                            json_obj.min.value = v;
                            json_obj.min.parameters = this.parameters[i];
                        }
                        if(v > json_obj.max.value) {
                            json_obj.max.value = v;
                            json_obj.max.parameters = this.parameters[i];
                        }
                        this.per_statistic_data[key] = json_obj;
                    }
                    key_index++;
                });
                i++;
            });

            this.computeGraphData();
        },
        computed: {
            ...mapState(["parameters","outputs"]),
        },
        methods: {
            ...mapMutations(["computeGraphInOut"]),
            otherParameters: function() {
                this.other_parameters = [];
                let i = 0;
                this.parameters.forEach(params => {
                    let numKey = 0;
                    Object.keys(params).forEach(key => {
                        if(key != this.view_parameter) {
                            if(i==0) {
                                this.other_parameters.push({label:key,values:[],selected:0});
                            }
                            this.other_parameters[numKey].values.push(params[key]);
                            numKey++;
                        }
                    });
                    i++;
                });
                this.other_parameters.forEach(elem => {
                    let unique_values = [...new Set(elem.values)];
                    elem.values = unique_values.sort();
                    elem.selected = elem.values[0];
                });
            },
            computeGraphData: function() {
                let labels = [];
                this.parameters.forEach(comb => {
                    labels.push(comb[this.view_parameter]);
                });
                labels = [...new Set(labels)].sort();
                this.data.labels = labels;
                this.otherParameters();
                let stat_data = [];
                let param_index = 0;
                let label_index = 0;
                this.parameters.forEach(comb => {
                    let flag = true;
                    if(comb[this.view_parameter]==labels[label_index]) {
                        this.other_parameters.forEach(other => {
                            if(comb[other.label] != other.selected) {
                                flag = false;
                            }
                        });
                    }
                    else {
                        flag = false;
                    }
                    if(flag) {
                        stat_data.push(this.outputs[param_index]);
                        label_index++;
                    }
                    param_index++;
                });
                let stat_index = 0;
                let stat_dataset = [];
                stat_data.forEach(stat => {
                    let tC = Object.keys(stat).length;
                    let c = 0;
                    Object.keys(stat).forEach(key => {
                        if(stat_index == 0) {
                            let d = [];
                            d.push(stat[key]);
                            let color = randomColor(tC,c);
                            let data = {label:key, data:d, borderColor:color, backgroundColor:color, hidden:true};
                            stat_dataset.push(data);
                        }
                        else {
                            stat_dataset.forEach(s => {
                                if(s.label == key) {
                                    s.data.push(stat[key]);
                                }
                            });
                        }
                        c++;
                    });
                    stat_index++;
                });
                this.data.datasets = stat_dataset;
                this.$refs["chartRef"].updateChart();
            }
        }
    }

</script>

<style>

.statistics {
    width: 100vw;
    height: 100vh;
    margin: auto;
}

</style>