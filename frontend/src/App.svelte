<script>
  import "./app.css";
  import Logos from "./lib/Logos.svelte";
  import Counter from "./lib/Counter.svelte";
  import Chat from "Chat.svelte";
</script>

<div class="flex flex-col justify-between h-dvh px-2 md:px-0">
  <header>
    <h1 class="text-2xl">&nbsp;</h1>
  </header>
  <main class="flex-grow">
    <Chat class="h-full" />
  </main>
  <footer class="h-30">
    <Logos />
  </footer>
</div>
