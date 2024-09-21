<script lang="ts">
  import ChatMessage from "$lib/ChatMessage.svelte";
  import { Button } from "$lib/components/ui/button";
  import * as Card from "$lib/components/ui/card/index.js";
  import { Input } from "$lib/components/ui/input/index.js";
  import { onMount, onDestroy } from "svelte";
  import { writable } from "svelte/store";

  type Message = {
    createdAt: Date;
    author: string;
    text: string;
  };

  type MessageWithDelay = Message & { delay?: number | undefined };

  let messages = [] as MessageWithDelay[];
  let input = "";
  let avgDelay = 0;

  function handleSubmit(e: SubmitEvent) {
    sendMessage(input);
    scrollToBottom();
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

  const connected = writable(false);

  const onSocketOpen = () => connected.set(true);
  const onSocketClose = () => connected.set(false);
  const onMessage = (event: MessageEvent) => {
    let parsedMessage: Message | undefined;
    try {
      parsedMessage = JSON.parse(event.data);
    } catch (e) {}

    if (!parsedMessage) {
      messages = [
        ...messages,
        { createdAt: new Date(), author: "Parse Error", text: event.data },
      ];
      return;
    }

    let delay = 0;
    let createdAt: Date | undefined = new Date(parsedMessage.createdAt);
    if (!isNaN(createdAt.getTime())) {
      delay = new Date().getTime() - createdAt.getTime();
    }
    messages = [...messages, { ...parsedMessage, delay, createdAt }];

    // avg delay over last x messages
    avgDelay = messages
      .slice(-200)
      .reduce((acc, msg) => acc + (msg.delay || 0), 0);
    avgDelay /= Math.min(messages.length, 200);
  };

  function terminateSocket(socket: WebSocket) {
    socket.close();
    socket.removeEventListener("open", onSocketOpen);
    socket.removeEventListener("close", onSocketClose);
    socket.removeEventListener("message", onMessage);
    socket.removeEventListener("close", reconnect);
    socket.removeEventListener("error", reconnect);
  }

  function connect() {
    const socket = new WebSocket("ws://127.0.0.1:3000/ws");
    socket.addEventListener("open", onSocketOpen);
    socket.addEventListener("close", onSocketClose);
    socket.addEventListener("message", onMessage);
    return socket;
  }

  let socket = connect();

  const reconnect = () => {
    console.log("Reconnecting...");
    terminateSocket(socket);
    socket = connect();
    socket.addEventListener("close", reconnect);
    socket.addEventListener("error", reconnect);
  };

  socket.addEventListener("close", reconnect);
  socket.addEventListener("error", reconnect);

  onDestroy(() => {
    terminateSocket(socket);
  });

  const sendMessage = (message: string) => {
    if (socket.readyState <= 1) {
      socket.send(
        JSON.stringify({
          author: "Me",
          createdAt: new Date().toISOString(),
          text: message,
        })
      );
    }
  };

  let interval: number = 500;
  let autoSendInterval: number | undefined;
  function startSending() {
    autoSendInterval = setInterval(() => {
      sendMessage(input || "Hello");
      //setTimeout(scrollToBottom, 1);
    }, interval);
  }
  function stopSending() {
    clearInterval(autoSendInterval);
    autoSendInterval = undefined;
  }
</script>

<Card.Root class="{$$props.class} min-h-64  w-full max-w-2xl mx-auto">
  <Card.Header>
    <Card.Title class="flex items-center justify-between">
      <div class="space-x-2">
        <span>Chat</span>
        {#if $connected}
          <span class="text-sm text-green-500">Connected</span>
        {:else}
          <span class="text-sm text-red-500">Disconnected</span>
        {/if}
      </div>
      <span class="text-sm text-gray-500">
        {messages.length}
      </span>
      <span class="text-sm text-gray-500 w-[130px] text-right space-x-2">
        {#if avgDelay > 0}
          <span>
            {(1000 / avgDelay).toFixed(0)}mps
          </span>
          <span>{avgDelay.toFixed(2)}ms</span>
        {/if}
      </span>
    </Card.Title>
  </Card.Header>
  <Card.Content class=" flex flex-col justify-end">
    <div
      class="overflow-y-auto"
      style="height: max(250px, calc(100vh - 270px));"
    >
      {#each messages as message}
        <ChatMessage
          class="no-overflow-anchoring"
          author={message.author}
          body={message.text}
          delay={message.delay}
        />
      {/each}
      <div class="anchor" />
    </div>
    <form class="flex space-x-2" on:submit|preventDefault={handleSubmit}>
      <Input bind:value={input} placeholder="Write a message…" class="flex-1" />
      <Button type="submit" disabled={input.trim().length === 0}>Send</Button>
      <Button
        type="button"
        on:click={!!autoSendInterval
          ? stopSending
          : () => {
              scrollToBottom();
              startSending();
            }}
        class="bg-blue-500 hover:bg-blue-700"
      >
        {!!autoSendInterval ? "Stop" : "Start"}
      </Button>
      <Input
        type="number"
        bind:value={interval}
        on:blur={() => {
          if (autoSendInterval) {
            stopSending();
            startSending();
          }
        }}
        on:keypress={(e) => {
          if (e.key === "Enter") {
            e.preventDefault();
            if (autoSendInterval) {
              stopSending();
              startSending();
            }
          }
        }}
        min="0"
        max="10000"
        step="1"
        class="w-18 text-right"
      />
    </form>
  </Card.Content>
</Card.Root>

<style>
  .anchor {
    height: 1px;
    overflow-anchor: auto;
  }
</style>
