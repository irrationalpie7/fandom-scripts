program:
# Note: in a list of expressions, the last expression cannot have a semicolon
# Also, comments aren't allowed to be indented.
# Also also, the way you use this is with "custom columns" of type "column built from other columns", where you put this entire thing in the "template" field
# Or you can add this under "preferences" "advanced" "template functions" with a template name like "summary_function", at which point the value you'll put in the column "template" will be "program:summary_function()"
# The advantage of that is that you can actually read the code and debug it and such there
# Defaults:
    default_max_characters = 201;
    omission=" (...)";

# Compute max_length, allowing override by column #max
# (but ignore #max if it's 0 or negative)
    max_length = $#max;
    if !max_length || max_length <=# 0 then
        max_length = default_max_characters
    fi;
    max_before_omission=max_length - strlen(omission);

## The regex for sentence boundaries is honestly pretty terrifying
# the first part excludes specific patterns like Mr./Mrs. (note that Mrs is separate
# because it's 3 chars instead of 2
# the next part (boundary) has some punctuation that might be the end of a sentence
# the next part (after) requires a following space, and then either a quote or a capital letter.
# ---> Note: noticed Mar 3 2023 that the "capital letter" part doesn't work because
#      calibre automatically makes matching case insensitive
# edge case: this cannot handle initials or abbreviations followed be a new capitalized word
# that is not the start of a new sentence
    sentence_punctuation = ":…;.?!—";
    extended_punctuation = strcat(sentence_punctuation,',"“”',"'");
    boundaries_find0 = strcat('(?<!Mr|Dr|Ms|Fr|Pr|Br|Sr|St)(?<!Mrs)(?P<boundary>([', sentence_punctuation, "]+))(?P<after> [A-Z'",'"“”])');
# If two or more punctuation symbols happen in a row, call it a sentence boundary
    boundaries_find1 = strcat("(?P<boundary>([",extended_punctuation,"][",extended_punctuation,"]+))(?P<after> )");
    boundaries_replace="\g<boundary>@@@@@\g<after>";
    sent_split="@@@@@ ";
# For some god-forsaken reason, the interpretation of \n is conditional
# such that we need to use "\\n" essentially everywhere but here:
    par_split="\n";

## clean the comment field into a summary
# remove pathological whitespace, including non-breaking spaces
    summary=re($comment,"(\s|\xa0|\x20)+"," ");
# convert the ends of paragraphs or break tags to newlines
    summary=re(summary," *(</p>|<br />|<br/>|<br>) *","\\n");
# bye bye other tags
    summary=re(summary,"<[^<>]+>","");
# remove HTML &;s as needed
    summary=re(summary,"&amp;","&");
    summary=re(summary,"&gt;",">");
    summary=re(summary,"&lt;","<");
# bye bye any consecutive white space characters (in case removing tags added some back)
    summary=re(summary," +"," ");
# bye bye to any white space near a \n
    summary=re(summary," *\\n *","\\n");
# bye bye to any consecutive \n
    summary=re(summary,"(\\n)+\\n","\\n");
# bye bye trailing \n or spaces
    summary=re(summary,"(\\n| )+$","");
# bye bye any leading \n or spaces (yes this exists wtf)
    summary=re(summary,"^(\\n| )+","");
# let's not treat . . . as multiple sentences
    summary=re(summary," ?([.] )+[.]","...");
    summary=re(summary,"…","...");

## extract paragraphs (up to ten of them because idk how to do better)
    sub_summary="";
    next=-1;
    for i in '0,1,2,3,4,5,6,7,8,9' separator ',':
        if next <# 0 then
            cur_paragraph=list_item(summary, i, par_split);
            blah="blah";
            if strlen(sub_summary) + strlen(cur_paragraph) + strlen(par_split) <=# max_before_omission then
                sub_summary = strcat(sub_summary, cur_paragraph, par_split)
            else
                next = i
            fi
        fi
    rof;
# make sure there's exactly one trailing \n:
    sub_summary=re(sub_summary,"(\\n| )+$","\\n");
# if the summary would otherwise be empty, remove the newline:
    if next ==# 0 then
        sub_summary = ""
    fi;
# if we went through the for without changing next, it should be 10,
# since the for went from 0 to 9
    if next <# 0 then
        next = 10
    fi;
    next_par=list_item(summary, next, par_split);

## manipulate next_par to find sentence boundaries
    next_par=re(next_par,boundaries_find0,boundaries_replace);
    next_par=re(next_par,boundaries_find1,boundaries_replace);
    debug_next_par=next_par;
    next=-1;
# hard-code 10 because idk how to do better
    for i in '0,1,2,3,4,5,6,7,8,9' separator ',':
        if next <# 0 then
            cur_sentence = list_item(next_par, i, sent_split);
            if strlen(sub_summary) + strlen(cur_sentence) <=# max_before_omission then
                sub_summary = strcat(sub_summary, cur_sentence, " ")
            else
                next = i
            fi
        fi
    rof;

# if sub_summary=="" give it a pity sentence (sent0) even if that puts it over max_before_omission
    if strlen(sub_summary) ==# 0 then
        sub_summary = list_item(next_par, 0, sent_split)
    fi;

# clean off any trailing spaces/new lines
    sub_summary=re(sub_summary,"(\\n| )*$","");
# concatenate the omission string to sub_summary
    sub_summary=strcat(sub_summary,omission);

## if summary is too long, use sub_summary instead
    final = summary;
    if strlen(final) ># max_length then
        final = sub_summary
    fi;

# Note: this has a bunch of \\n (instead of \n) because that's what generate cover wants
    final