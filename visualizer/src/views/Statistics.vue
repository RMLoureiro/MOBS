<template>
    <section class="statistics">
        <button @click="displayGeneral">General Statistics</button>
        <button @click="displayPerNode">Per Node Statistics</button>
        <div v-if="generalStatistics" class="scontainer">
            <div class="ll">
                <parameter-chart :data="data" ref="chartRef"></parameter-chart>
                <p>
                    Varying Parameter (X Axis):
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
            </div>
            <div class="rr">
                <h3>Optimal Parameters</h3>
                <div v-for="stat in per_statistic_data" v-bind:key="stat.label">
                    <h4>{{stat.label}}</h4>
                    <p>
                        Minimum Value: {{stat.min.value}} <button @click="openModal(stat.min)">Show Parameters</button>
                    </p>
                    <p>
                        Maximum Value: {{stat.max.value}} <button @click="openModal(stat.max)">Show Parameters</button>
                    </p>
                </div>
            </div>
        </div>
        <div v-if="perNodeStatistics" class="scontainer">
            <parameter-chart :data="perNodeData" ref="chartRef"></parameter-chart>
            <p>X Axis = Node IDs</p>
            <p>
                Result of:
                    <select v-model="parameterFile" @change="computePerNodeData">
                        <option v-for="v in parameter_files" v-bind:value="v" v-bind:key="v">
                            {{v}}
                        </option>
                    </select>
            </p>
        </div>

        <param-modal v-if="showModal" @close="showModal = false" v-bind:data="modalData"></param-modal>
    </section>
</template>

<script>
    import { mapActions, mapMutations, mapState } from "vuex";
    import ParameterChart from "../components/ParameterChart.vue";
    import ParamModal from '../components/ParamModal.vue';

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
                perNodeData: {
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
                parameter_files:[],
                view_parameter:"",
                other_parameters:[],
                showModal:false,
                modalData:null,
                generalStatistics:true,
                perNodeStatistics:false,
                parameterFile:""
            };
        },
        components: {
            ParameterChart,
            ParamModal
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
                    if(this.view_parameter == "") {
                        this.view_parameter = key;
                    }
                }
            });

            Object.keys(this.outputs[0].stats).forEach(key => {
                this.statistic_labels.push(key);
            });

            let i = 0;
            this.outputs.forEach(out_stats => {
                let key_index = 0;
                Object.keys(out_stats.stats).forEach(key =>{
                    let v = out_stats.stats[key];
                    if(i === 0) {
                        let json_obj = {
                            label:key,
                            min:{value:v, parameters:this.parameters[i], filename:this.outputs[i].filename},
                            max:{value:v, parameters:this.parameters[i], filename:this.outputs[i].filename}
                        };
                        this.per_statistic_data.push(json_obj);
                    }
                    else {
                        let json_obj = this.per_statistic_data[key_index];
                        if(v < json_obj.min.value) {
                            json_obj.min.value = v;
                            json_obj.min.parameters = this.parameters[i];
                            json_obj.min.filename = this.outputs[i].filename;
                        }
                        if(v > json_obj.max.value) {
                            json_obj.max.value = v;
                            json_obj.max.parameters = this.parameters[i];
                            json_obj.max.filename = this.outputs[i].filename;
                        }
                        this.per_statistic_data[key] = json_obj;
                    }
                    key_index++;
                });
                i++;
            });

            this.otherParameters();
            this.computeGraphData();
            this.computePerNodeData();
        },
        computed: {
            ...mapState(["parameters","outputs","per_node_outputs"]),
        },
        watch: {
            view_parameter: function() {
                this.otherParameters();
                this.computeGraphData();
            }
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
                //labels = [...new Set(labels)].sort();
                this.data.labels = labels;
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
                        stat_data.push(this.outputs[param_index].stats);
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
                stat_dataset[stat_dataset.length-1].hidden = false;
                this.data.datasets = stat_dataset;
                this.$refs["chartRef"].updateChart();
            },
            computePerNodeData : function() {
                let out_index = 0;
                for(let i = 0; i < this.parameter_files.length; i++) {
                    if(this.parameter_files[i] == this.parameterFile) {
                        out_index = i;
                    }
                }
                let stats_obj = this.per_node_outputs[out_index].stats;
                let tC = Object.keys(stats_obj).length;
                let c = 0;
                let stat_dataset = [];
                Object.keys(stats_obj).forEach(key => {
                    let labels = [];
                    let values = [];
                    for(let i = 0; i < stats_obj[key].length; i++) {
                        labels.push(i+1);
                        values.push(stats_obj[key][i]);
                    }
                    this.perNodeData.labels = labels;
                    let color = randomColor(tC,c);
                    let data = {label:key, data:values, borderColor:color, backgroundColor:color, hidden:true};
                    stat_dataset.push(data);
                    c++;
                });
                stat_dataset[stat_dataset.length-1].hidden = false;
                this.perNodeData.datasets = stat_dataset;
                this.$refs["chartRef"].updateChart();
            },
            openModal: function(data) {
                this.modalData = data;
                this.showModal = true;
            },
            displayGeneral : function() {
                this.generalStatistics = true;
                this.perNodeStatistics = false;
            },
            displayPerNode : function() {
                this.parameter_files = [];
                // Store all parameter file names
                this.outputs.forEach(out => {
                    this.parameter_files.push(out.filename.replace("output", "parameters"));
                });
                this.parameterFile = this.parameter_files[0];
                this.generalStatistics = false;
                this.perNodeStatistics = true;
                
            }
        }
    }

</script>

<style scoped>

    .statistics {
        width: 100vw;
        height: 100vh;
        margin: auto;
    }

    ::-webkit-scrollbar {
        display: none;
    }

    .ll {
        float: left;
        width: 75%;
    }

    .rr {
        margin-left: 75%;
        padding-top: 5%;
    }

    .scontainer {
        width: 80%;
        margin: auto;
    }

</style>