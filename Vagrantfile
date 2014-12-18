# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "hashicorp/precise64"
  config.vm.provision "shell", path: "manifests/precise64.sh"
  config.vm.synced_folder ".", "/erlangio"
end
