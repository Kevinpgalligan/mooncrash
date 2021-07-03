### Description
A physics simulation to demonstrate why the Moon doesn't crash into Earth. There's a [sample video](https://kevingal.com/static/video/mooncrash/sample.mp4) on my website.

![picture of moon and Earth](https://github.com/Kevinpgalligan/mooncrash/blob/master/pic.png)

### Setup
You'll need to install a Common Lisp implementation and quicklisp. Clone this repo to your quicklisp `local-projects` folder. Then, from your REPL, after following the manual installation steps for [trivial-gamekit](https://github.com/borodust/trivial-gamekit) (the only dependency), run the following commands:

```lisp
CL-USER> (ql:quickload 'mooncrash)
CL-USER> (in-package mooncrash)
MOONCRASH> (gamekit:start 'spacesim)
```

Aaaand with that, you should be off to the races! You can launch a moon from anywhere on the screen by clicking and dragging your mouse. Let me know if any of that doesn't work.
