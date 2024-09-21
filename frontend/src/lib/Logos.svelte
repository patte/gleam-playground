<script lang="ts">
  import svelteLogo from "$assets/svelte.svg";
  import viteLogo from "/vite.svg";
  import gleamLogo from "/lucy.svg";
  import tailwindLogo from "/tailwind.svg";
</script>

<div class="flex justify-center gap-9">
  <a href="https://gleam.run" target="_blank" rel="noreferrer">
    <img src={gleamLogo} class="logo gleam" alt="Gleam Logo (Lucy)" />
  </a>
  <a href="https://vitejs.dev" target="_blank" rel="noreferrer">
    <img src={viteLogo} class="logo vite" alt="Vite Logo" />
  </a>
  <a href="https://tailwindcss.com" target="_blank" rel="noreferrer">
    <img src={tailwindLogo} class="logo tailwind" alt="Tailwind CSS Logo" />
  </a>
  <a href="https://svelte.dev" target="_blank" rel="noreferrer">
    <img src={svelteLogo} class="logo svelte" alt="Svelte Logo" />
  </a>
</div>

<style>
  .logo {
    height: 6em;
    padding: 1.5em;
    will-change: filter;
    transition: filter 300ms;
  }
  .logo.gleam:hover {
    filter: drop-shadow(0 0 2em #ffaff3);
  }
  .logo.vite:hover {
    filter: drop-shadow(0 0 2em #646cffaa);
  }
  .logo.tailwind:hover {
    filter: drop-shadow(0 0 2em #06b6d4aa);
  }
  .logo.svelte:hover {
    filter: drop-shadow(0 0 2em #ff3e00aa);
  }
</style>
