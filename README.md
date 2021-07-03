### Description
A physics simulation to demonstrate why the Moon doesn't crash into Earth.

![video demonstration, flinging the moon around like a mad yolk](https://github.com/Kevinpgalligan/mooncrash/blob/master/sample.mp4)

### Setup
You'll need to install a Common Lisp implementation and quicklisp. Clone this repo to your quicklisp `local-projects` folder. Then, from your REPL, after following the manual installation steps for [trivial-gamekit](https://github.com/borodust/trivial-gamekit) (the only dependency), run the following commands:

```lisp
CL-USER> (ql:quickload 'mooncrash)
CL-USER> (in-package mooncrash)
MOONCRASH> (gamekit:start 'spacesim)
```

Aaaand with that, you should be off to the races! Let me know if any of that doesn't make sense or it doesn't work.
