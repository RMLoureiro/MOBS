<template>
    
    <section class="range-parameter">
        <div class="div-range-parameter">
            <p class="param-label">
                {{parameter.label}}:
                <span class="param-value">
                    <input type="text" v-bind:value="parameter.value" v-bind:id="parameter.label" v-if="!isRange">
                </span>
                <span class="range-data">
                    <ul>
                        <li>
                            Ranged Parameter: <input type="checkbox" v-bind:checked="isRange" v-on:click="isRange = !isRange">
                        </li>
                        <li v-if="isRange">
                            Min: <input type="text" v-model="min">
                        </li>
                        <li v-if="isRange">
                            Max: <input type="text" v-model="max">
                        </li>
                        <li v-if="isRange">
                            Step: <input type="text" v-model="step">
                        </li>
                    </ul>
                </span>
            </p>
        </div>
    </section>

</template>

<script>

    export default {
        name:"range-parameter",
        props: {
            parameter: Object
        },
        data() {
            return {
                isRange:false,
                min:0,
                max:0,
                step:0
            };
        },
        methods: {
            getValues() {
                if(!this.isRange) {
                    return [document.getElementById(this.parameter.label).value];
                } 
                else {
                    this.step = parseFloat(this.step);
                    this.max  = parseFloat(this.max);
                    this.min  = parseFloat(this.min);
                    if(this.step == 0 || this.min > this.max) {
                        return [this.min];
                    }
                    let ret = [];
                    let i = this.min;
                    while(i <= this.max) {
                        ret.push(i);
                        i += this.step;
                    }
                    return ret;
                }
            },
            getLabel() {
                return this.parameter.label;
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

    .range-data {
        text-align: right;
    }
</style>