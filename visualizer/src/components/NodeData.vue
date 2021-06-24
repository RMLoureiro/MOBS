<template>
    <div>
        <div v-if="hasNode() == true">
            <h3>Node {{node.id}}</h3>
            <p>Region: {{node.region}}</p>
            <div style="text-align:left;">
                <h4>Current Statistics</h4>
                <ul>
                    <li>Chain Head:</li>
                        <ul>
                            <li>ID : {{node.currentStatistics.chainHead.id}}</li>
                            <li>Creator : {{node.currentStatistics.chainHead.creator}}</li>
                        </ul>
                    <li>Currently in Committee: {{node.currentStatistics.inCommittee}}</li>
                    <li>Currently a Proposer: {{node.currentStatistics.proposer}}</li>
                </ul>
                <h4>Global Statistics</h4>
                <ul>
                    <li>Total Committee Participations: {{node.globalStatistics.committeeParticipations}}</li>
                    <li>Proposals Created: {{node.globalStatistics.proposalsCreated}}</li>
                    <li>Ended with {{node.globalStatistics.totalNumberOfBlocks}} blocks in its chain</li>
                </ul>
            </div>
        </div>
        <div v-if="!hasNode()">
            Click on a node to display its information.
        </div>
    </div>
</template>


<script>
    
    export default {
        name:"nodedata",
        props: {
            node: Object
        },
        methods: {
            hasNode: function() {
                return this.node !== null;
            }
        }
    }


</script>