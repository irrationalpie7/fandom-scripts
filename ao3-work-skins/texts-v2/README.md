# Texting workskin

I'm planning to write an auto-formatter for this workskin, but if you want it sooner you can check out texting.css and texting.html

## Input for the auto-formatter

### Header

```
# Chat display name

```

There's a few different types of chat. Types "group" and "group-all" will display as "**Group chat: Chat display name**" when there's no css applied (creator style off). "private" and "private-all" will display as "**Texts with: Chat display name**".

You can use any number of "#" with no spaces to signal the header. If you want multiple text chains, use a divider of "---" with an empty line before and after. If a section between "---"s doesn't start with a texting header, it will be ignored by the formatter.

If you have two chats with the same display name, and you want the second to have the same metadata as the first, just skip the metadata on the second chat.

### Metadata

Technically optional, but usually you'll want to specify at least the point-of-view character.

```
meta:
pov: char1
type: group-all
char1: Character Display Name
char2: Other Display Name

```

POV: POV defaults to none (as if the point-of-view character is lurking in the chat). The value of pov can be either a characters' display name, or an alias ("pov: Character Display Name" or "pov: char1")

Type: Type defaults to group-all if there are more than two characters or aliases in the conversation, private-all otherwise.

- group-all (show all character names)
- group (hides pov name visually)
- private-all (shows all character names)
- private (hides pov and non-pov name(s) visually)

Aliases: Lines other than "`type`" and "`pov`" are aliases, so that you can write "char1: Message" instead of having to type out "Character Display Name: Message" every time. This is also how you can override character color -- "char1: pink: Character Display Name". The word "`info`" is reserved and can't be used as an alias.

Colors: The built-in options are red, orange, yellow, green, blue, lightblue, purple, pink, and gray. You can use a non-supported option, but it will show the default color as a fallback until you edit your css to support the new color.

### Texts

Some example texts. New-lines are mostly ignored, but a blank line is required between separate messages. If the next line after the blank one doesn't have a character name specified, it will be treated as a double-text (same sender as previous message).

```
char1: Message 1

Double-text from char 1

char2:
Message from char 2

```

You can use the character's display name instead of an alias like "char1" if you prefer.

Replies: as a shorthand, you can label a message with a label in square brackets (e.g., [my-label]). Then later, if another character wants to quote that message, they can use the label [reply-my-label] for the new message and it will automatically quote the first message. "reply-" is required, but "my-label" can be any set of letters, numbers, or - as long as it's the same for the reply and the message that quotes it.

```
char1[1]: Message 1

char2[reply-1]: I'm replying to Message 1

[reply-1-1]: I'm char2 and I'm replying to my own previous message as a double-text

```

Info: `info` is a special reserved word that can't be used as an alias. Use "info: **Today** 6:15 AM" or "**Character 2** changed the chat name to **Stop arguing, nimwits**" to have a centered info line in the chat

Images can be represented as `![a 1-2 sentence description of what the image represents](https://example.com/image-link)`. Please please include a description! See this [resource on writing helpful alt text](https://accessibility.huit.harvard.edu/describe-content-images). If you don't have a working link for the image, the preview can show a placeholder instead.

### Odds and ends

Within a text message or info, use $alias or @alias to include the character's display name (@alias will include the @ and bold the name).

### Limitations

- Message text is required unless there's an image
- A reply to a previous message can't also send an image
- A reply to an image without text will just say "_[Image]_" as the quoted text
- A message can include at most one image
- Message text shows below the image, if there is an image.
- If something is falsely being recognized as a character (e.g., "Oh no :(" being interpreted as character "Oh no" sends message "("), add the actual character name at the beginning of the line ("char1: oh no :(")
- For the most part, new-lines are ignored. If you want a particular new-line to be respected, use "`<br>`" at the end of the line
- If in the metadata you wish to specify an alias for a character with a ":" in their display name, either specify the color, or use the format "ivan:: Ivan: the Bold"
- Asterisks are treated as formatting (single for italics, double for bold). To write an asterisk instead, use "`\*`"
