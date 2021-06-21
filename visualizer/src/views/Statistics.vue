<template>
    <section class="statistics">
        <parameter-chart :data="data"></parameter-chart>
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

            
            this.otherParameters();
            this.computeGraphData();
            // TODO : set data (labels and dataset)
            // TODO : call for Graph update
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
                // TODO
                console.log("Hello world!");
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