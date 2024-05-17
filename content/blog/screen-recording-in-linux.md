+++
title = "Screen Recording in Linux"
author = ["Shuvrojit Biswas"]
date = 2023-05-08
lastmod = 2023-05-08T00:48:40+06:00
tags = ["wfrecorder", "tools", "productivity", "linux"]
categories = ["linux"]
draft = false
weight = 1001
+++

I use sway as my wm on top of arch linux. Sway is a wayland compositor. For recording my screen, I use wf-recorder. wf-recorder can use most of the popular codecs like gif, h264, h265, vp8, vp9, avi and a lot more. It also supports hardware/gpu accelaration using vaapi. Archwiki has a really good documentation on vaapi.

When using first time without any codec specification it will default to `libx264` codec. The recording video quality is bad and colors doesn't seem right. It looks blurry. `libxvid` also has the same quality.

{{< figure src="/ox-hugo/badtrim.gif" >}}

`libx265` is a good codec. Cpu usage is a little bit high though. `libx264rgb` has better video quality and all the colors. But using it for a long time kills sway and sends me back to the login window. dmesg shows GPU hang error. I searched for a solution, couldn't find any. Maybe it's a problem with my intel gpu.

You can use vp9 with `libvpx-vp9`. It has a high cpu usage. You will need to specify a device. This one has good video quality too. Another one is `h264_vaapi`, low cpu usage but doesn't have good colors. quality can be adjusted with pixel format and codec parameter. These both have low file size.

If size doesn't matter to you then use `ffv1` or `huffyuv`. These both have high quality video and high file size. You can reduce the file size just by a little using codec param context.

{{< figure src="/ox-hugo/goodtrim.gif" >}}

For audio recording, you can combine multiple sources. wf-recorder has a good documentation with examples [here](https://github.com/ammen99/wf-recorder/wiki).

```shell
# List all sources
pactl list sources short

# List all the codecs on your system
ffmpeg -codecs


# List all the pixel formats on your system
ffmpeg -pix_fmts

# List all the encoders on your system
ffmpeg -encoders

wf-recorder --codec=ffv1 -p context=0 -f output.avi
wf-recorder -c libvpx-vp9 -f output.mkv
wf-recorder -c h264_vaapi -p qp=0 --pixel-format=rgbaf32le -d /dev/dri/renderD128 -f output.mp4
wf-recorder -c gif -f output.gif
```

My wf config looks like this. I use it with [this](https://github.com/ammen99/wf-recorder/wiki/Usage-with-Sway).

```shell
#!/bin/sh


pactl load-module module-null-sink sink_name=Combined
pactl load-module module-loopback sink=Combined source=alsa_output.pci-0000_00_1f.3.analog-stereo.monitor
pactl load-module module-loopback sink=Combined source=bluez_output.E8_07_BF_1A_2A_2F.1.monitor
pactl load-module module-loopback sink=Combined source=alsa_input.pci-0000_00_1f.3.analog-stereo

# wf-recorder -f "~/Desktop/screencasts/$(date +"%Y_%m_%d_%I_%M_%p").mp4" --audio=Combined.monitor -c libx264rgb flac -p "--preset=faster"

## vapi good quality low cpu usage.
wf-recorder -f ~/Desktop/screencasts/"$(date +"%Y_%m_%d_%I_%M_%p").mp4" --audio=Combined.monitor -c h264_vaapi -p qp=1 --pixel-format=rgbaf32le -d /dev/dri/renderD128

## vapi best/better quality low cpu usage. gpu hang
# wf-recorder -f ~/Desktop/screencasts/"$(date +"%Y_%m_%d_%I_%M_%p").mp4" --audio=Combined.monitor -c libx264rgb


## vapi best/better quality low cpu usage.
# wf-recorder -f ~/Desktop/screencasts/"$(date +"%Y_%m_%d_%I_%M_%p").mp4" --audio=Combined.monitor -c ffv1 -p --context=0
```

There's kahoo, which has good color quality with gif. And there's obs studio.
