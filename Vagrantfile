# -*- mode: ruby -*-

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/bionic64"
  config.vm.provision "shell", inline: <<-SHELL
    set -eou pipefail
    apt-get update
    apt-get install -y aspell zopfli python3 python3-setuptools texinfo python3-pip emacs nodejs npm
    curl --location --remote-name --silent \
      https://github.com/jgm/pandoc/releases/download/2.9.2.1/pandoc-2.9.2.1-1-amd64.deb
    dpkg --install pandoc-2.9.2.1-1-amd64.deb
    pip3 install panflute
    cd /vagrant
    npm install
  SHELL
end
