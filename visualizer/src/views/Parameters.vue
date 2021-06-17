<template>
    <section class="parameters">
        <div class="content">
            <form class="container">
                <div class="left">
                    <h3>General Parameters</h3>
                    <div class="global-parameters">
                        <parameter
                            v-for="(param,index) in gParams"
                            :key="index"
                            :parameter="param"
                        ></parameter>
                    </div>
                    <h3>Network Parameters</h3>
                    <div class="network-parameters">
                        <parameter
                            v-for="(param,index) in nParams"
                            :key="index"
                            :parameter="param"
                        ></parameter>
                    </div>
                </div>
                <div class="right">
                    <h3>Protocol Parameters</h3>
                    <div class="protocol-parameters">
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
            this.gParams  = this.generalParameters;
            this.nParams  = this.networkParameters;
            this.pParams  = this.protocolParameters;
        },
        computed: {
            ...mapState(["generalParameters","networkParameters","protocolParameters"]),
        },
        methods: {
            ...mapMutations(["getParameters"]),
        },
    }

</script>

<style>
    .container {
        width: 80vw;
        text-align: center;
        margin: auto;
    }

    .left {
        width: 35vw;
        float: left;
    }

    .right {
        margin-left: 45vw;
        width: 35vw;
    }
</style>