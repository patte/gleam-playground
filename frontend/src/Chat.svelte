<script lang="ts">
  import ChatMessage from "$lib/ChatMessage.svelte";
  import { Button } from "$lib/components/ui/button";
  import * as Card from "$lib/components/ui/card/index.js";
  import { Input } from "$lib/components/ui/input/index.js";
  import { onMount } from "svelte";

  let messages = Array.from({ length: 100 }, (_, i) => ({
    author: "Gleam",
    body: `Hello, world ${i}!`,
  }));

  let input = "";

  function handleSubmit(e: SubmitEvent) {
    scrollToBottom();
    messages = [...messages, { author: "Gleam", body: input }];
    input = "";
  }

  function scrollToBottom() {
    const scrollable = document.querySelector(".overflow-y-auto");
    if (!scrollable) {
      throw new Error("Scrollable element not found");
    }
    scrollable.scrollTop = scrollable.scrollHeight;
  }

  onMount(scrollToBottom);
</script>

<Card.Root class="{$$props.class} flex flex-col w-full max-w-2xl mx-auto">
  <Card.Header>
    <Card.Title class="flex items-center justify-between">
      <span>Chat</span>
    </Card.Title>
  </Card.Header>
  <Card.Content class="flex-grow flex flex-col justify-end">
    <div class="overflow-y-auto" style="height: calc(100vh - 270px);">
      {#each messages as message}
        <ChatMessage
          class="no-overflow-anchoring"
          author={message.author}
          body={message.body}
        />
      {/each}
      <div class="anchor" />
    </div>
    <form class="flex space-x-2" on:submit|preventDefault={handleSubmit}>
      <Input bind:value={input} placeholder="Write a message…" class="flex-1" />
      <Button type="submit" disabled={input.trim().length === 0}>Send</Button>
    </form>
  </Card.Content>
</Card.Root>

<style>
  .anchor {
    height: 1px;
    overflow-anchor: auto;
  }
</style>
