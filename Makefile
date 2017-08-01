
all : app/Repetitive.y happy-templates/HappyTemplate-incremental-ghc-debug
	happy --ghc --incremental --debug --template=./happy-templates --info=Repetitive.info \
    --lr0 --action --goto --lookaheads \
    app/Repetitive.y

precedence : app/ExprPrecedence.y happy-templates/HappyTemplate-incremental-ghc-debug
	happy --ghc --incremental --debug --template=./happy-templates --info=ExprPrecedence.info \
    --lr0 --action --goto --lookaheads \
    app/ExprPrecedence.y

simple : app/ExprSimple.y happy-templates/HappyTemplate-incremental-ghc-debug
	happy --ghc --incremental --debug --template=./happy-templates --info=ExprSimple.info \
    --lr0 --action --goto --lookaheads \
    app/ExprSimple.y

happy-templates/HappyTemplate-incremental-ghc-debug: happy-templates/GenericTemplate.hs
	ghc -cpp -E -DHAPPY_ARRAY -DHAPPY_GHC -DHAPPY_DEBUG -DHAPPY_INCR  happy-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@

  # ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),

orig : app/ExprSimpleOrig.y
	happy --ghc --array --debug  --info=ExprSimpleOrig.info app/ExprSimpleOrig.y
