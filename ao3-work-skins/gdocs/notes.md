Characteristics:

- "highlight" text
- comments (by multiple characters?)
- comment replies?

Plain-text encoding:

- highlighted text: [[highlighted text]]

  - must be followed by at least one comment
  - cannot overlap or be inside another highlight
  - can be multiple in a paragraph, but:
    - order on mobile will be full paragraph + comment thread + comment thread, so use wisely
  - can span multiple paragraphs, but:
    - on mobile, any comments related to those paragraphs will show up after the last paragraph of the group
    - any additional comment threads within that set of paragraphs will also only show up after the final paragraph of the set

- comment: {{NAME: comment}}
  - must be immediately after a highlight (]]) or another comment (}})
  - can optionally have some modifiers like {MODIFIER{NAME: comment}}. Valid modifiers:
    - {Q} - quote the highlighted text
    - {Q:text} - partial quote ("text")
    - {T} - align top of first comment with the top of the paragraph
      - this works best if the comment is much shorter than the paragraph and the highlighted text is near the beginning of the paragraph
    - {B} - align bottom of last comment with the bottom of the paragraph
      - this works best if the comment is much shorter than the paragraph and the highlighted text is near the end of the paragraph
    - {C} - (this is the default) align middle of paragraph with middle of comment thread

<p>Blah blah paragraph [[HIGHLIGHTED_TEXT]]{{NAME: COMMENT}}{{NAME: REPLY}}</p>
