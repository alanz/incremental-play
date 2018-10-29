all: repetitive2 repetitive precedence simple

# HAPPY= cabal new-run happy:happy --
# HAPPY=./dist-newstyle/build/x86_64-linux/ghc-8.4.3/happy-1.20.0/build/happy/happy
HAPPY=happy-az

repetitive2 : app/Repetitive2.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=Repetitive2.info \
    --lr0 --action --goto --lookaheads \
    app/Repetitive2.y


repetitive : app/Repetitive.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=Repetitive.info \
    --lr0 --action --goto --lookaheads \
    app/Repetitive.y

precedence : app/ExprPrecedence.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=ExprPrecedence.info \
    --lr0 --action --goto --lookaheads \
    app/ExprPrecedence.y

simple : app/ExprSimple.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=ExprSimple.info \
    --lr0 --action --goto --lookaheads \
    app/ExprSimple.y

.PHONY : templates
templates : happy-templates/IncrementalTemplate-ghc-debug

# happy-templates/HappyTemplate-incremental-ghc-debug: happy-templates/GenericTemplate.hs
happy-templates/IncrementalTemplate-ghc-debug: happy-templates/GenericTemplate.hs
	ghc -cpp -E -DHAPPY_ARRAY -DHAPPY_GHC -DHAPPY_DEBUG -DHAPPY_INCR  happy-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@

  # ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),

orig : app/ExprSimpleOrig.y
	$(HAPPY) --ghc --array --debug  --info=ExprSimpleOrig.info app/ExprSimpleOrig.y
