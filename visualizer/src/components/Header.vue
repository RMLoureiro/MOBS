<template>
    <header class="header">
        <nav class="nav">
            <h2 class="nav__link">
                <span class="header-link"><router-link to="/">Parameters</router-link></span>
            </h2>
            <h2 class="nav__link">
                <span class="header-link"><router-link to="/topology">Topology</router-link></span>
            </h2>
            <h2 class="nav__link">
                <span class="header-link"><router-link to="/visualizer">Visualizer</router-link></span>
            </h2>
            <h2 class="nav__link">
                <span class="header-link"><router-link to="/statistics">Statistics</router-link></span>
            </h2>
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
</style>
