default:
  @just --choose

repl *args:
  rlwrap just run repl {{args}}

run *args:
  cabal run aether -- {{args}}

runw *args:
  nodemon -e .hs,.cpp -w bin -w src -w cpp --exec 'clear && just run {{args}}'

test *args:
  cabal test {{args}}

testw *args:
  nodemon -e .hs -w src -w specs --exec 'clear && just test {{args}}'

build:
  nix build

clean:
  cabal clean
