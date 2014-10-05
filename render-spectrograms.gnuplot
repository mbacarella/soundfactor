reset
set title "a.mp3 spectrogram"
set xlabel "time"
set ylabel "peak-frequency"
set grid
set term x11 0
plot "/home/mbac/music/satisfaction/a.mp3.spectrogram" title ""

reset
set title "b.mp3 spectrogram"
set xlabel "time"
set ylabel "peak-frequency"
set grid
set term x11 1
plot "/home/mbac/music/satisfaction/b.mp3.spectrogram" title ""

reset
set title "c.mp3 spectrogram"
set xlabel "time"
set ylabel "peak-frequency"
set grid
set term x11 2
plot "/home/mbac/music/satisfaction/c.mp3.spectrogram" title ""

reset
set title "not-a.mp3 spectrogram"
set xlabel "time"
set ylabel "peak-frequency"
set grid
set term x11 3
plot "/home/mbac/music/satisfaction/not-a.mp3.spectrogram" title ""