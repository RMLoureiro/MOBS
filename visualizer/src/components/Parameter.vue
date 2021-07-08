<template>
    
    <section class="parameter">
        <div class="div-parameter">
            <p class="param-label">
                {{parameter.label}}:
                <span class="param-value">
                    <input type="text" 
                        v-bind:value="parameter.value"
                        v-bind:id="parameter.label"
                        v-if="this.parameter.value != 'true' && this.parameter.value != 'false'"
                    >
                    <input type="checkbox"
                        v-bind:id="parameter.label"
                        v-model="this.checkboxval"
                        true-value="true"
                        false-value="false"
                        v-if="this.parameter.value == 'true' || this.parameter.value == 'false'"
                    >
                </span>
            </p>
        </div>
    </section>

</template>

<script>

    export default {
        name:"parameter",
        props: {
            parameter: Object
        },
        data() {
            return {
                checkboxval:false
            }
        },
        mounted() {
            this.checkboxval = this.parameter.value == 'true';
        },
        watch: {
            checkboxval: function(newValue) {
                console.log(newValue);
                console.log(document.getElementById(this.parameter.label).value);
                console.log(document.getElementById(this.parameter.label).checked);
                document.getElementById(this.parameter.label).value = newValue;
            }
        }
    }

</script>

<style>
    .param-label {
        text-align: left;
    }

    .param-value {
        float: right;
    }
</style>