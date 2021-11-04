<template>
    <div>
        <div v-if="hasSelection() == true">
            <div v-if="isNode() == true" class="fos">
                <h3>Node {{selected.target.id}}</h3>
                <div class="pwrap">
                    <span class="sp">Region: <input type="number" id="region" v-bind:value="selected.target.region"></span>
                    <span class="sp">HashPower: <input type="number" id="hpower" v-bind:value="selected.target.hPower"></span>
                    <span class="sp">Stake: <input type="number" id="stake" v-bind:value="selected.target.stake"></span>
                    <span class="sp">Links: {{selected.target.links}}</span>
                    <br>
                    <span class="sp">IsMalicious: <input type="checkbox" id="isbad" v-bind:checked="selected.target.malicious.isbad"></span>
                    <span class="sp">BecomeMaliciousTimestamp: <input type="number" id="becomebadts" v-bind:value="selected.target.malicious.start"></span>
                    <span class="sp">OfflineFrom: <input type="number" id="offstart" v-bind:value="selected.target.offline.from"></span>
                    <span class="sp">OfflineUntil: <input type="number" id="offend" v-bind:value="selected.target.offline.to"></span>
                </div>
                <button @click="handleUpdateClick">Update</button>
            </div>
            <div v-if="isLink() == true" class="fos">
                <h3>Link from {{selected.target.from.id}} to {{selected.target.to.id}}</h3>
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
            },
            handleUpdateClick : function() {
                let region = parseInt(document.getElementById("region").value);
                let hPower = parseInt(document.getElementById("hpower").value);
                let stake  = parseInt(document.getElementById("stake").value);
                let isMalicious = document.getElementById("isbad").checked;
                let badTs  = parseInt(document.getElementById("becomebadts").value);
                let offS   = parseInt(document.getElementById("offstart").value);
                let offE   = parseInt(document.getElementById("offend").value);
                this.$parent.updateNode({id:this.selected.target.id, region:region, hPower:hPower, stake:stake, offline:{from:offS,to:offE}, malicious:{isbad:isMalicious,start:badTs}});
            }
        }
    }


</script>

<style scoped>

    h3 {
        margin: 0;
    }

    .pwrap {
        width: 100%;
        text-align: center;
        padding-bottom: 5px;
    }

    .sp {
        padding-left: 10px;
        padding-right: 10px;
    }

</style>