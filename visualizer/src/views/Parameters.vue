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
                        <parameter
                            v-for="(param,index) in pParams"
                            :key="index"
                            :parameter="param"
                        ></parameter>
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

    export default {
        data() {
            return {
                gParams:"",
                nParams:"",
                pParams:""
            };
        },
        components: {
            Parameter,
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
            ...mapMutations(["getParameters","setParameters"]),
            runSim: function() {
                this.gParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });
                this.nParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });
                this.pParams.forEach(element => {
                    let stringVal = document.getElementById(element.label).value;
                    element.value = JSON.stringify(JSON.parse(stringVal));
                });
                this.setParameters([this.gParams, this.nParams, this.pParams]);
                return false;
            }
        },
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