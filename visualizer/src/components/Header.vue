<template>
    <header class="header">
        <nav class="nav">
            <h3 class="nav__link">
                <span class="header-link"><router-link to="/"><img src="notepad.png" class="img-icon"> Parameters</router-link></span>
            </h3>
            <h3 class="nav__link">
                <span class="header-link"><router-link to="/topology"><img src="top_graph.png" class="img-icon"> Topology</router-link></span>
            </h3>
            <h3 class="nav__link">
                <span class="header-link"><router-link to="/visualizer"><img src="globe.png" class="img-icon"> Visualizer</router-link></span>
            </h3>
            <h3 class="nav__link">
                <span class="header-link"><router-link to="/statistics"><img src="stat_graph.png" class="img-icon"> Statistics</router-link></span>
            </h3>
        </nav>
    </header>
</template>
<script>
    export default {
        name: "app-header",
    };
</script>
<style>
    .header {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-between;
        text-align: center;
        width: 100%;
        border-bottom: 1px solid black;
    }
    .nav {
        display: flex;
        flex-wrap: wrap;
        width: 75%;
        justify-content: space-between;
        margin: auto;
    }
    .header-link {
        font-family: "Roboto", sans-serif;
        color: #000000;
    }
    .header-icon {
        background-color: black;
    }
    .img-icon {
        height: 20px;
        width: 20px;
    }
</style>
