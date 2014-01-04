sudo apt-get update
sudo apt-get install -y python-software-properties
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-1.18 cabal-install-1.16 ghc-7.6.3 ghc-head ghc-7.4.2
cabal-1.18 update
