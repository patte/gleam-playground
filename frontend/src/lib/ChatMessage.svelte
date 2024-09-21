<script lang="ts">
  import { Button } from "$lib/components/ui/button";
  export let body = "Hello, world!";
  export let author = "Gleam";
</script>

<div class="{$$props.class} mb-4">
  <div class="flex items-start space-x-2">
    <!--<Avatar>
        <AvatarFallback>{author[0]}</AvatarFallback>
      </Avatar>-->
    <div class="flex-1">
      <p class="font-semibold">{author}</p>
      <p>{body}</p>
    </div>
    <Button variant="ghost" size="icon" class="text-muted-foreground">
      <!--<Trash2 className="w-4 h-4" />-->
    </Button>
  </div>
</div>
