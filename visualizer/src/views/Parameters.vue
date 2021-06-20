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
                    <input type="submit" value="Run Simulation">
                </div>
            </form>
        </div>
    </section>
</template>



<script>
    import { mapMutations, mapState } from "vuex";
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
                pComponents:[]
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
            ...mapMutations(["getParameters","setParameters","clearInputFiles"]),
            setItemRef: function(el) {
                if(el) {
                    this.pComponents.push(el);
                }
            },
            runSim: function() {
                this.clearInputFiles();

                this.gParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
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
                let parsedCombinations = parseCombinations(combinations, labels);

                let totalExecutions = parsedCombinations.length;
                let execution = 1;

                parsedCombinations.forEach(combination => {
                    this.setParameters([this.gParams, this.nParams, combination]);
                    console.log("Running execution "+execution+" out of "+totalExecutions);
                    execution++;
                });

                return false;
            }
        },
        beforeUpdate() {
            this.pComponents = [];
        }
    }

</script>

<style>
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
</style>