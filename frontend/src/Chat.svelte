<script lang="ts">
  import ChatMessage from "$lib/ChatMessage.svelte";
  import { Button } from "$lib/components/ui/button";
  import * as Card from "$lib/components/ui/card/index.js";
  import { Input } from "$lib/components/ui/input/index.js";
  import {
    message_to_string,
    message_from_string,
    type Message$ as MessageSharedType,
    ChatMessage as ChatMessageSharedType,
    RoomUpdate as RoomUpdateSharedType,
  } from "$generated/shared/shared.mjs";
  import { onMount, onDestroy } from "svelte";
  import { writable } from "svelte/store";
  import { Ok } from "$generated/prelude.mjs";
  import { Slider } from "$lib/components/ui/slider/index.js";
  import { Badge } from "$lib/components/ui/badge/index.js";
  import Icon from "svelte-awesome";
  import usersIcon from "svelte-awesome/icons/users";
  import envelopeOIcon from "svelte-awesome/icons/envelopeO";

  type ChatMessage = Omit<
    ChatMessageSharedType,
    "withFields" | "created_at"
  > & {
    created_at: Date;
  };

  type ChatMessageWithDelay = ChatMessage & { delay?: number | undefined };

  let chatMessages = [] as ChatMessageWithDelay[];
  let input = "";
  let avgDelay = 0;
  let numParticipants = 0;

  let meName = `User ${Math.floor(Math.random() * 1000)}`;

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
    let result = message_from_string(event.data);

    // validate message
    if (!(result instanceof Ok)) {
      chatMessages = [
        ...chatMessages,
        { created_at: new Date(), author: "Parse Error", text: event.data },
      ];
      return;
    }

    const parsedMessage = result["0"] as MessageSharedType;

    if (parsedMessage instanceof RoomUpdateSharedType) {
      let roomUpdate = parsedMessage as RoomUpdateSharedType;
      numParticipants = roomUpdate.num_participants;
    } else if (parsedMessage instanceof ChatMessageSharedType) {
      const chatMessage = parsedMessage as ChatMessageSharedType;

      let delay = 0;
      let created_at: Date | undefined = new Date(chatMessage.created_at);
      if (!isNaN(created_at.getTime())) {
        delay = new Date().getTime() - created_at.getTime();
      }

      // append to local messages
      chatMessages = [
        ...chatMessages,
        {
          ...chatMessage,
          delay,
          created_at,
          author: chatMessage.author === meName ? "Me" : chatMessage.author,
        },
      ];

      // avg delay over last x messages
      avgDelay = chatMessages
        .slice(-200)
        .reduce((acc, msg) => acc + (msg.delay || 0), 0);
      avgDelay /= Math.min(chatMessages.length, 200);
    }
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
    const url = import.meta.env.DEV
      ? "ws://127.0.0.1:3000/ws" // force 3000 to enable access from vite dev server
      : `wss://${window.location.host}/ws`;
    const socket = new WebSocket(url);
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
      const chatMessage = new ChatMessageSharedType(
        message,
        meName,
        new Date().toISOString()
      );
      const encoded = message_to_string(chatMessage);
      socket.send(encoded);
    }
  };

  let interval: number = 250;
  let autoSendInterval: number | undefined;
  function startSending() {
    autoSendInterval = setInterval(() => {
      sendMessage(input || "Hello 👋");
    }, interval);
    setTimeout(scrollToBottom, 1);
  }
  function stopSending() {
    clearInterval(autoSendInterval);
    autoSendInterval = undefined;
  }
</script>

<Card.Root class="{$$props.class} min-h-64  w-full max-w-2xl mx-auto">
  <Card.Header>
    <Card.Title class="flex items-center justify-between">
      <div class="space-x-1">
        <span>Chat</span>
        {#if $connected}
          <div
            class="w-3 h-3 rounded-full inline-block align-baseline bg-green-500"
          />
        {:else}
          <div
            class="w-3 h-3 rounded-full inline-block align-baseline bg-red-500"
          />
        {/if}
      </div>
      <div class="whitespace-nowrap">
        <Badge variant="outline" class="text-gray-200">
          <Icon scale={0.75} data={usersIcon} class="mr-1 text-gray-200" />
          {numParticipants}
        </Badge>
        <Badge variant="outline" class="text-gray-200">
          <Icon scale={0.75} data={envelopeOIcon} class="mr-1 text-gray-200" />
          {chatMessages.length}
        </Badge>
      </div>
      <span
        class="text-sm text-gray-500 font-mono w-[50px] text-right space-x-2"
      >
        {#if avgDelay > 0}
          <!--
          <span>
            {(1000 / avgDelay).toFixed(0)}mps
          </span>
          -->
          <span>{avgDelay.toFixed(2)}ms</span>
        {/if}
      </span>
    </Card.Title>
  </Card.Header>
  <Card.Content class=" flex flex-col justify-end">
    <div
      class="overflow-y-auto"
      style="height: max(100px, calc(100svh - 310px));"
    >
      {#each chatMessages as message}
        <ChatMessage
          class="no-overflow-anchoring"
          author={message.author}
          body={message.text}
          delay={message.delay}
        />
      {/each}
      <div class="anchor" />
    </div>
    <div class="space-y-2">
      <div class="flex space-x-2 justify-end">
        <Slider
          value={[interval]}
          onValueChange={(e) => {
            interval = e[0];
            if (autoSendInterval) {
              stopSending();
              startSending();
            }
          }}
          min={0}
          max={500}
          step={1}
          class="max-w-[150px]"
        />
        <p style="line-height: 37px;" class="w-[60px] font-mono text-right">
          <span style="vertical-align: middle;">{interval}ms</span>
        </p>
        <!--
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
          max="1000"
          step="1"
          class="w-18 text-right"
        />
        -->
        <Button
          type="button"
          on:click={!!autoSendInterval
            ? stopSending
            : () => {
                scrollToBottom();
                startSending();
              }}
          class="bg-blue-500 hover:bg-blue-700 w-[60px]"
          disabled={!autoSendInterval && !$connected}
        >
          {!!autoSendInterval ? "Stop" : "Start"}
        </Button>
      </div>
      <form
        class="flex space-x-2 flex-grow"
        on:submit|preventDefault={handleSubmit}
      >
        <Input
          bind:value={input}
          placeholder="Write a message…"
          class="flex-1 text-base"
        />
        <Button
          type="submit"
          disabled={input.trim().length === 0}
          class="w-[60px]">Send</Button
        >
      </form>
    </div>
  </Card.Content>
</Card.Root>

<style>
  .anchor {
    height: 1px;
    overflow-anchor: auto;
  }
</style>
