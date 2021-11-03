<template>
    <div>
        <div v-if="hasSelection() == true">
            <h1>Placeholder Text</h1>
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
            }
        }
    }


</script>