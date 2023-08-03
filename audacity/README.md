# Miscellaneous audacity plug-ins/macros

The files here are various audacity plug-ins/macros I've worked on over time. I've organized them here for ease of me finding them again, but **please be aware that some of them are broken/confusing.** I'll add description here as I clean them up/make certain they work.

## Add Labels Below/Add Mono Below

These related plug-ins (`add-labels-below.ny` and `add-mono-below.ny`) add a label track or mono track directly below the track that currently has focus, then attempt to reset the focus to the original track. I originally posted about "Add Labels Below" in this audacity forum thread: [Why Are New Tracks Always Added to the End? You Should be Able to Add Them After the Selected Track](https://forum.audacityteam.org/viewtopic.php?f=69&t=105172&start=20#p450455):

Inspired by trackmove.ny, I made a nyquist plug-in that:
1. identifies the current track (call it track "a")
2. adds a label track and places it directly below that track
3. moves focus back to track "a".

I'm not sure if there are any differences to how plug-ins apply based on audacity version, but I'm running version 3.0.2 on linux, installed from the ppa.

A few notes:
1. The plug-in picks "current track" based on the track that has focus--in particular, this means that if you:
   1. select some audio on track 1
   2. manually hit the down arrow to move focus to track 2
   3. run this plug-in
   then the plug-in will
   1. Place the new label track directly below track 2
   2. and then select the portion of track *2* matching the range of track *1* that you originally had selected
2. The plug-in doesn't try to do any sort of special handling if your selection spans multiple track--the plug-in will still:
   1. pick the one track that has focus (if you selected with the mouse from top to bottom, that'll be the last track of the selection, if you selected from bottom to top, it'll be the top track of the selection)
   2. make a label track directly below that one track
   3. and then, because it only knows about that one track, put the focus back there, de-selecting the other tracks.

I don't understand nyquist well enough to address these limitations, and this is sufficient for my purposes, but if anyone else does want to address this, go for it! It is lines 47-50 that place the focus back on the original track, and the function get-focus-track that picks which track to put the label track directly below.

----

To make a similar plug-in that adds any other kind of track, you can change:
1. Line 4:
```
;name "Add Labels Below"
```
This is the line that sets the display name you'll see when you want to apply the plug-in in audacity (or use it as a step in a macro).
2. Line 40:
```
    (aud-do "NewLabelTrack:")
```
This is the line that makes a new label track. You can replace "NewLabelTrack:" with any of the other scripting ids under "Tracks: Add New" in the [scripting reference](https://manual.audacityteam.org/man/scripting_reference.html).
3. The file name, so you'll be able to recognize it in the [enable/disable add-ons menu](https://manual.audacityteam.org/man/manage_effects_generators_and_analyzers.html) after installation


## Add Label Plus

This plug-in provides more control for adding a label with arbitrary text/location as part of a macro. If I recall correctly, it works but the controls are a bit confusing.

TODO: flesh out description

## generate.sh

Something to do with generating parametrized macros. I think it's generating parametrized user presets and printing them to the command line, to be manually copied directly into audacity's config file, as well as a set of parametrized macros that reference those user presets and can be copied into audacity's macro folder. 

I did get it to work eventually, but I totally destroyed an audacity project that I opened while testing a previous version of the script, so, y'know, use extreme caution. (It turns out audacity has opinions about macros that have the same name after spaces have been removed from the file name, and that opinion is **no**).

## Extend selection

Allows changing the number of tracks selected, relative to the current focused/selected track.

## Select tracks plus

Like extend selection, but has more (and more confusing) controls. TODO: document.

## Label clicks.txt

This can be imported as a macro. It's intended to help with processing audio that has been labeled with claps or clicks to mark bloopers. You can navigate between the generated region labels to decide if the threshold has correctly identified a click, and then remove it (with sync-lock tracks on), to remove these clicks from the audio. Then you can remove the region label track entirely, and only the point label track will remain, so you can navigate quickly between the labeled regions to address whatever bloopers remain.

This macro depends on the "Add Labels Below" plugin above and "point" and "region" user presets for "Label Sounds..." that can identify such clicks.

It is an evolution of the process described in [this tutorial](https://docs.google.com/document/d/1SREMyxGa4Ufr8G0tPRRNRerVNYmGaXLxgLCyzH6JfYU/edit?usp=sharing). TODO: update.

## Process clicks.txt

This is an incomplete macro (as in, it doesn't quite work right yet), designed to run after the "label clicks" macro. It's supposed to move all labeled audio regions to a track just below the labeled track, and mute the new track. Thus, the clicks get silenced without having to be manually removed, but the audio is still around/aligned in case there's a false-positive where some loud speaking got labeled as a click.