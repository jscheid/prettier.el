# -*- mode: ruby -*-

# This file is not part of GNU Emacs.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Run a command inside the Vagrant VM in directory /vagrant. See
# HACKING.md

Vagrant.configure('2') do |config|
  config.vm.box = 'ubuntu/bionic64'
  config.vm.provision 'shell', privileged: true, inline: <<-SHELL
    set -eou pipefail
    add-apt-repository ppa:kelleyk/emacs
    apt-get update
    curl --location --silent https://deb.nodesource.com/setup_16.x | sudo -E bash -
    apt-get install -y aspell python3 python3-setuptools texinfo python3-pip emacs27 nodejs gnutls-bin ruby
    curl --location --remote-name --silent \
      https://github.com/jgm/pandoc/releases/download/2.11.1.1/pandoc-2.11.1.1-1-amd64.deb
    dpkg --install pandoc-2.11.1.1-1-amd64.deb
    pip3 install panflute>=2.0.5
    npm install -g yarn@1.22.18
    cd /vagrant
    yarn
    cd test-stable && yarn && cd ..
    cd test-v2 && yarn && cd ..
  SHELL
end

ENV['VAGRANT_DEFAULT_PROVIDER'] = 'virtualbox'
