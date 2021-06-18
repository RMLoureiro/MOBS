<template>
    
    <section class="parameter">
        <div class="div-parameter">
            <p class="param-label">{{parameter.label}}:<span class="param-value"><input type="text" v-bind:value="parameter.value" v-bind:id="parameter.label"></span></p>
        </div>
    </section>

</template>

<script>

    export default {
        name:"parameter",
        props: {
            parameter: Object
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