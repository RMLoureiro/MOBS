<template>
    <div>
        <div v-if="hasSelection() == true">
            <div v-if="isNode() == true" class="fos">
                <h1>Node {{selected.target.id}}</h1>
                <p class="pwrap">
                    <span class="sp">Region: {{selected.target.region}}</span>
                    <span class="sp">HashPower: {{selected.target.hPower}}</span>
                    <span class="sp">Stake: {{selected.target.stake}}</span>
                    <span class="sp">Links: {{selected.target.links}}</span>
                </p>
            </div>
            <div v-if="isLink() == true" class="fos">
                <h1>Link from {{selected.target.from.id}} to {{selected.target.to.id}}</h1>
            </div>
        </div>

    </div>
</template>


<script>
    
    export default {
        name:"nodedata",
        props: {
            selected: Object
        },
        methods: {
            hasSelection: function() {
                if(this.selected != null) {
                    return this.selected.target !== null;
                }
                return false;
            },
            isNode: function() {
                return this.selected.type == "node";
            },
            isLink: function() {
                return this.selected.type == "link";
            }
        }
    }


</script>

<style>

</style>