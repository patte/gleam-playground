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
  import { onMount, onDestroy, tick } from "svelte";
  import { writable } from "svelte/store";
  import { Ok } from "$generated/prelude.mjs";
  import { Slider } from "$lib/components/ui/slider/index.js";
  import { Badge } from "$lib/components/ui/badge/index.js";
  import Icon from "svelte-awesome";
  import usersIcon from "svelte-awesome/icons/users";
  import envelopeOIcon from "svelte-awesome/icons/envelopeO";
  import arrowUpIcon from "svelte-awesome/icons/arrowUp";
  import arrowDownIcon from "svelte-awesome/icons/arrowDown";
  import { LinkedChart } from "svelte-tiny-linked-charts";

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
  let numSent = 0;

  let meName = `User ${Math.floor(Math.random() * 1000)}`;

  let chartData = {} as Record<string, number>;
  let hoveredValue: number | undefined = undefined;

  function handleSubmit(e: SubmitEvent) {
    scrollToBottom();
    sendMessage(input);
    input = "";
  }

  function scrollToBottom() {
    const scrollable = document.querySelector(".overflow-y-auto");
    if (!scrollable) {
      throw new Error("Scrollable element not found");
    }
    scrollable.scrollTop = scrollable.scrollHeight;
  }

  function isScrolledToBottom() {
    const scrollable = document.querySelector(".overflow-y-auto");
    if (!scrollable) {
      return false;
    }
    // 2px tolerance
    return (
      scrollable.scrollHeight -
        scrollable.clientHeight -
        scrollable.scrollTop <=
      2
    );
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

      const isScrolledToBottomBefore = isScrolledToBottom();

      let delay: number | undefined = undefined;
      let created_at: Date | undefined = new Date(chatMessage.created_at);
      if (
        // as clocks are not synced, only my messages can be measured
        chatMessage.author === meName &&
        created_at &&
        !isNaN(created_at.getTime())
      ) {
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

      let chatMessagesWithDelay = chatMessages.filter(
        (m) => m.delay !== undefined
      );

      // update chart data
      chartData = chatMessagesWithDelay.slice(-50).reduce(
        (acc, msg) => {
          const date = msg.created_at.toISOString();
          acc[date] = msg.delay!;
          return acc;
        },
        {} as Record<string, number>
      );

      // avg delay over last x messages
      avgDelay = chatMessagesWithDelay
        .slice(-200)
        .reduce((acc, msg) => acc + (msg.delay || 0), 0);
      avgDelay /= Math.min(chatMessagesWithDelay.length, 200);

      // scroll to bottom if was before or not many messages
      if (isScrolledToBottomBefore || chatMessages.length <= 12) {
        tick().then(() => {
          scrollToBottom();
        });
      }
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
      numSent++;
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
    if (!input.length) {
      input = "Hello 👋";
    }
    autoSendInterval = setInterval(() => {
      sendMessage(input || "Hello 👋");
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
          <Icon scale={0.75} data={arrowUpIcon} class="mr-1 text-gray-200" />
          {numSent}
          <Icon scale={0.75} data={envelopeOIcon} class="mx-1 text-gray-200" />
          {chatMessages.length}
          <Icon scale={0.75} data={arrowDownIcon} class="ml-1 text-gray-200" />
        </Badge>
      </div>
      <div class="flex justify-end space-x-2">
        <LinkedChart
          class="translate-y-[-4px]"
          type="line"
          height={21}
          width={50}
          lineColor={"#6b7280"}
          fill={"#ffffff"}
          barMinWidth={1}
          data={chartData}
          dispatchEvents
          on:hover={(e) => {
            const value = e.detail.value;
            if (value !== undefined) {
              hoveredValue = value;
            }
          }}
          on:blur={() => {
            hoveredValue = undefined;
          }}
        />
        <span
          class="text-sm text-gray-500 font-mono min-w-[50px] text-right space-x-2"
        >
          {#if avgDelay > 0}
            <span
              >{hoveredValue !== undefined
                ? hoveredValue
                : avgDelay.toFixed(2)}ms</span
            >
          {/if}
        </span>
      </div>
    </Card.Title>
  </Card.Header>
  <Card.Content class=" flex flex-col justify-end">
    <div
      class="overflow-y-auto"
      style="height: max(100px, calc(100svh - 340px));"
    >
      {#each chatMessages as message}
        <ChatMessage
          class=" even:bg-[#0f1422] px-2 py-1 rounded-sm"
          author={message.author}
          body={message.text}
          delay={message.delay}
        />
      {/each}
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
          class="max-w-[189px]"
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
    <p class="text-xs text-gray-500 pt-2">
      Hint: send long running calculations like <a
        href="#"
        class="underline"
        on:click={() => {
          input = "200!";
          // focus input
          document.querySelector("input")?.focus();
        }}>200!</a
      >
      or
      <a
        href="#"
        class="underline"
        on:click={() => {
          input = "30000!";
          // focus input
          document.querySelector("input")?.focus();
        }}>30000!</a
      >
      or
      <a
        href="#"
        class="underline"
        on:click={() => {
          input = "60000!";
          // focus input
          document.querySelector("input")?.focus();
        }}>60000!</a
      >
    </p>
  </Card.Content>
</Card.Root>
