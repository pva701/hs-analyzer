time find -L ~/github/cardano-sl -name '*.hs' | grep -v ".stack-work" | tr '\n' ' ' | xargs stack exec -- hs-analyzer | grep "inspection" | wc -l
