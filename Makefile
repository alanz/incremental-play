
all : app/Simple.y happy-templates/HappyTemplate-incremental-ghc-debug
	# happy --ghc --array --debug --template=./happy-templates --info=Simple.info app/Simple.y
	# happy --ghc --incremental --debug --template=./happy-templates --info=Simple.info app/Simple.y
	happy --ghc --incremental --debug --template=./happy-templates --info=ExprSimple.info \
    --lr0 --action --goto --lookaheads \
    app/ExprSimple.y

happy-templates/HappyTemplate-incremental-ghc-debug: happy-templates/GenericTemplate.hs
	ghc -cpp -E -DHAPPY_ARRAY -DHAPPY_GHC -DHAPPY_DEBUG -DHAPPY_INCR  happy-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@

  # ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),
