
all : app/Simple.y happy-templates/HappyTemplate-arrays-ghc-debug
	happy --ghc --array --debug --template=./happy-templates --info=Simple.info app/Simple.y
	# happy --ghc --array --debug  --info=Simple.info app/Simple.y

happy-templates/HappyTemplate-arrays-ghc-debug: happy-templates/GenericTemplate.hs
	ghc -cpp -E -DHAPPY_ARRAY -DHAPPY_GHC -DHAPPY_DEBUG -DHAPPY_INCR  happy-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@

  # ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),
