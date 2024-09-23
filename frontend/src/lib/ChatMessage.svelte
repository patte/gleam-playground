<script lang="ts">
  import { Button } from "$lib/components/ui/button";
  export let body = "Hello, world!";
  export let author = "Gleam";
  export let delay: number | undefined;
</script>

<div class="{$$props.class} mb-4">
  <div class="flex items-start space-x-2">
    <!--<Avatar>
        <AvatarFallback>{author[0]}</AvatarFallback>
      </Avatar>-->
    <div class="flex-1">
      <p class="font-semibold">{author}</p>
      <p class="wrap-overflow text-sm">{body}</p>
    </div>
    <div class="self-end">
      <span class="text-xs text-gray-500 font-mono"
        >{delay !== undefined ? `${delay}ms` : ""}</span
      >
    </div>
  </div>
</div>
