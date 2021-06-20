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
                            Min: <input type="text" v-bind:value="min">
                        </li>
                        <li v-if="isRange">
                            Max: <input type="text" v-bind:value="max">
                        </li>
                        <li v-if="isRange">
                            Step: <input type="text" v-bind:value="step">
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