# hsh
A small shell written in haskell.

## Example usage
```
➜  hsh git:(main) ✗ cabal run
Welcome to the Haskell SHell!
/home/leonv/Git/hsh > ls
app  dist-newstyle  hsh.cabal
/home/leonv/Git/hsh > sleep 5 &     
pid: 26960

[ .. wait 5s .. ]

/home/leonv/Git/hsh > jobs
26960: sleep 5
[sleep 5] exited with code 0
/home/leonv/Git/hsh > cd ..
/home/leonv/Git > cd hsh
/home/leonv/Git/hsh > cabal run    
Welcome to the Haskell SHell!
/home/leonv/Git/hsh >
```
## Some TODOs
- [ ] quotes ("") should group a string on the commandline, parser needs to be adjusted for this
- [ ] arrow keys are not implemented yet, neither left/right for moving nor up/down for history, this also needs adjusting of the parser
- [ ] stdin and stdout should be reroutable with >> and <<
- [ ] handle unix signals (ctrl + c)
- [ ] command 'nuke <pid>' that kills a process
