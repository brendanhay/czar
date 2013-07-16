Vagrant.configure('2') do |config|
  config.vm.box_url = 'http://zxaos.github.io/vagrantbox-raringServer64/raringServer64.box'
  config.vm.box     = 'raring-server'

  config.ssh.forward_agent = true

  config.vm.provision :shell, :inline => $bootstrap

  config.vm.provider(:virtualbox) do |vb|
    vb.customize ["modifyvm", :id, "--memory", "4096"]
  end

  config.vm.synced_folder ".", "/home/vagrant/czar"
end

$bootstrap = <<-BASH

sudo apt-get update
sudo apt-get install build-essential zlib1g-dev git-core ghc cabal-install -y

echo 'export PATH=~/.cabal/bin:$PATH' > ~/.bashrc

cabal update
cabal install cabal-install cabal-dev cabal-meta hlint

BASH
