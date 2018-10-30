all: repetitive2 repetitive precedence simple

# HAPPY= cabal new-run happy:happy --
# HAPPY=./dist-newstyle/build/x86_64-linux/ghc-8.4.3/happy-1.20.0/build/happy/happy
HAPPY=happy-az

repetitive2 : parsers/Repetitive2.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=Repetitive2.info \
    --lr0 --action --goto --lookaheads \
    parsers/Repetitive2.y


repetitive : parsers/Repetitive.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=Repetitive.info \
    --lr0 --action --goto --lookaheads \
    parsers/Repetitive.y

precedence : parsers/ExprPrecedence.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=ExprPrecedence.info \
    --lr0 --action --goto --lookaheads \
    parsers/ExprPrecedence.y

simple : parsers/ExprSimple.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=ExprSimple.info \
    --lr0 --action --goto --lookaheads \
    parsers/ExprSimple.y

.PHONY : templates
templates : happy-templates/IncrementalTemplate-ghc-debug

# happy-templates/HappyTemplate-incremental-ghc-debug: happy-templates/GenericTemplate.hs
happy-templates/IncrementalTemplate-ghc-debug: happy-templates/GenericTemplate.hs
	ghc -cpp -E -DHAPPY_ARRAY -DHAPPY_GHC -DHAPPY_DEBUG -DHAPPY_INCR  happy-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@

  # ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),

orig : parsers/ExprSimpleOrig.y
	$(HAPPY) --ghc --array --debug  --info=ExprSimpleOrig.info parsers/ExprSimpleOrig.y
