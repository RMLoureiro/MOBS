<template>
    <section class="parameters">
        <div class="content">
            <form class="container" @submit.prevent="runSim">
                <div class="left">
                    <div class="global-parameters">
                        <h3>General Parameters</h3>
                        <parameter
                            v-for="(param,index) in gParams"
                            :key="index"
                            :parameter="param"
                        ></parameter>
                    </div>
                    <div class="network-parameters">
                        <h3>Network Parameters</h3>
                        <parameter
                            v-for="(param,index) in nParams"
                            :key="index"
                            :parameter="param"
                        ></parameter>
                    </div>
                </div>
                <div class="right">
                    <div class="protocol-parameters">
                        <h3>Protocol Parameters</h3>
                        <range-parameter
                            v-for="(param,index) in pParams"
                            :key="index"
                            :parameter="param"
                            :ref="el => pComponents.push(el)"
                        ></range-parameter>
                    </div>
                    <div class="btn-wrapper-params">
                        <input type="button" value="Store as Default Parameters" @click="storeAsDefault" class="my-btn">
                        <input type="submit" value="Run Simulation" :disabled="running" class="my-btn">
                        <span style="margin:auto;">
                            <progress :max="maxProgress" :value="progress"></progress>
                        </span>
                    </div>
                </div>
            </form>
        </div>
    </section>
</template>



<script>
    import { mapActions, mapMutations, mapState } from "vuex";
	import Parameter from "../components/Parameter";
    import RangeParameter from '../components/RangeParameter.vue';

    // https://stackoverflow.com/a/15310051
    function getCombinations(args) {
        let res = []
        let maxLength = args.length-1;
        function helper(arr, i) {
            let length = args[i].length;
            for (let j=0; j<length; j++) {
                let a = arr.slice(0);
                a.push(args[i][j]);
                if (i==maxLength)
                    res.push(a);
                else
                    helper(a, i+1);
            }
        }
        helper([], 0);
        return res;
    }

    function parseCombinations(c, labels) {
        let res = [];

        for(let i = 0; i < c.length; i++) {
            let params = [];
            for(let j = 0; j < c[i].length; j++) {
                params.push({label:labels[j], value:c[i][j]});
            }
            res.push(params);
        }

        return res;
    }



    export default {
        data() {
            return {
                gParams:"",
                nParams:"",
                pParams:"",
                pComponents:[],
                progress:0,
                maxProgress:100,
                currentCombination:0,
                parsedCombs:[],
                running:false
            };
        },
        components: {
            Parameter,
            RangeParameter,
        },
        created() {
            this.getParameters();
            this.gParams  = JSON.parse(JSON.stringify(this.generalParameters));
            this.nParams  = JSON.parse(JSON.stringify(this.networkParameters));
            this.pParams  = JSON.parse(JSON.stringify(this.protocolParameters));
        },
        computed: {
            ...mapState(["generalParameters","networkParameters","protocolParameters"]),
        },
        methods: {
            ...mapMutations(["getParameters","clearInputFiles","setCombinations","storeParamsDefault"]),
            ...mapActions(["produce"]),
            setItemRef: function(el) {
                if(el) {
                    this.pComponents.push(el);
                }
            },
            runCombination : async function(x) {
                if(x < this.parsedCombs.length) {
                    let promise = this.produce([this.gParams, this.nParams, this.parsedCombs[x], x]);
                    promise.then((value) => {
                        if (value == "ok") {
                            this.progress += 1;
                            this.currentCombination += 1;
                            this.runCombination(this.currentCombination);
                        }
                        else
                            console.log("error");
                    });
                }
                else {
                    this.running = false;
                }
            },
            runSim: function() {
                this.clearInputFiles();
                this.progress = 0;

                this.gParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    if(stringVal === "on") { stringVal = false; }
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });
                this.nParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });


                // [a,b,c,d]
                // where each element is itself a [] containing possible values
                // for that parameter, according to its {min,max,step} definitions
                let pParamValues = [];

                let labels = [];

                this.pComponents.forEach(element => {
                    pParamValues.push(element.getValues());
                    labels.push(element.getLabel());
                });

                let combinations = getCombinations(pParamValues);
                this.parsedCombs = parseCombinations(combinations, labels);
                this.maxProgress = this.parsedCombs.length;
                this.currentCombination = 0;
                this.running = true;

                this.runCombination(this.currentCombination);

                return false;
            },
            storeAsDefault: function() {
                this.gParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });
                this.nParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });

                let pParamValues = [];
                let labels = [];

                this.pComponents.forEach(element => {
                    pParamValues.push(element.getValues());
                    labels.push(element.getLabel());
                });

                let combinations = getCombinations(pParamValues);
                this.parsedCombs = parseCombinations(combinations, labels);
                this.storeParamsDefault([this.gParams, this.nParams, this.parsedCombs[0]]);
            }
        },
        beforeUpdate() {
            this.pComponents = [];
        }
    }

</script>

<style scoped>
    .container {
        width: 80vw;
        text-align: center;
        margin: auto;
        margin-top: 0;
    }

    .left {
        width: 35vw;
        float: left;
    }

    .right {
        margin-left: 45vw;
        width: 35vw;
    }

    .global-parameters {
        padding-top: 10px;
        margin-top: 0;
    }

    .protocol-parameters {
        padding-top: 10px;
        margin-top: 0;
    }

    ::-webkit-scrollbar {
        display: none;
    }

    .my-btn {
        color: white;
        padding: 5px 10px;
        border-radius: 4px;
        /*text-shadow: 0 1px 1px rgba(0, 0, 0, 0.2);*/
        background-color: #3700B3;
        font-family: "Roboto", sans-serif;
        font-size: 16px;
        margin: auto;
    }

    .my-btn:hover {
        background-color: #6200EE;
    }

    .btn-wrapper-params {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-between;
        text-align: center;
        width: 90%;
        margin: auto;
    }

</style>
