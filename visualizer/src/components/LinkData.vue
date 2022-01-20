<template>
    <div>
        <div v-if="hasLink() == true">
            <h3>Link from {{link.from}} to {{link.to}}</h3>
            <div style="text-align:left;">
                <h4>Messages in Transit:</h4>
                <div class="link-data-class">
                    <ul>
                        <li v-for="m in link.content" :key="m">
                            <pre>{{m}}</pre>
                        </li>
                    </ul>
                </div>
            </div>
        </div>
        <div v-if="!hasLink()">
            Click on a link to display its information.
        </div>
    </div>
</template>


<script>
    
    export default {
        name:"linkdata",
        props: {
            link: Object
        },
        methods: {
            hasLink: function() {
                return this.link !== null;
            }
        }
    }


</script>

<style>

    .link-data-class {
        position: relative;
        overflow-y: scroll;
        height: 30vh;
        width: 100%;
    }

</style>