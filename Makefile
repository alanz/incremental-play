all: repetitive2 repetitive precedence simple lexer

# HAPPY= cabal new-run happy:happy --
# HAPPY=./dist-newstyle/build/x86_64-linux/ghc-8.4.3/happy-1.20.0/build/happy/happy
HAPPY=happy-az
ALEX=alex

#-- ------------------------------------------

lexer : parsers/PaperExample.x templates
	$(ALEX) --ghc --debug \
    --template=./alex-templates \
    -o generated-parsers/PaperExample.hs \
    parsers/PaperExample.x

    # --info \

#-- ------------------------------------------

repetitive2 : parsers/Repetitive2.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=Repetitive2.info \
    --lr0 --action --goto --lookaheads \
    -o generated-parsers/Repetitive2.hs \
    parsers/Repetitive2.y


repetitive : parsers/Repetitive.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=Repetitive.info \
    --lr0 --action --goto --lookaheads \
    -o generated-parsers/Repetitive.hs \
    parsers/Repetitive.y

precedence : parsers/ExprPrecedence.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=ExprPrecedence.info \
    --lr0 --action --goto --lookaheads \
    -o generated-parsers/ExprPrecedence.hs \
    parsers/ExprPrecedence.y

simple : parsers/ExprSimple.y templates
	$(HAPPY) --ghc --incremental --debug --template=./happy-templates --info=ExprSimple.info \
    --lr0 --action --goto --lookaheads \
    -o generated-parsers/ExprSimple.hs \
    parsers/ExprSimple.y

#-- ------------------------------------------
#-- ------------------------------------------
.PHONY : templates
templates : happy-templates/IncrementalTemplate-ghc-debug \
            alex-templates/AlexTemplate-ghc-debug

# happy-templates/HappyTemplate-incremental-ghc-debug: happy-templates/GenericTemplate.hs
happy-templates/IncrementalTemplate-ghc-debug: happy-templates/GenericTemplate.hs
	ghc -cpp -E -DHAPPY_ARRAY -DHAPPY_GHC -DHAPPY_DEBUG -DHAPPY_INCR  happy-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@

  # ("HappyTemplate-arrays-ghc-debug"     , ["-DHAPPY_ARRAY","-DHAPPY_GHC","-DHAPPY_DEBUG"]),

#-- ------------------------------------------

alex-templates/AlexTemplate-ghc-debug: alex-templates/GenericTemplate.hs
	ghc -cpp -E -DALEX_ARRAY -DALEX_GHC -DALEX_DEBUG -DALEX_INCR  alex-templates/GenericTemplate.hs -o $@
	sed -i -E "s/^# ([0-9]+ \".*\").*/{-# LINE \1 #-}/" $@


#-- ------------------------------------------
orig : parsers/ExprSimpleOrig.y
	$(HAPPY) --ghc --array --debug  --info=ExprSimpleOrig.info parsers/ExprSimpleOrig.y
