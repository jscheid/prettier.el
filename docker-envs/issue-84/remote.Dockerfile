#
# This Dockerfile creates the "remote" container: a simple container
# running Ubuntu with SSH access for a user named "test", accessible
# as defined in file `ssh-config'. The server has Prettier installed
# and contains a badly formatted JavaScript file in
# `/home/ubuntu/test.js'.
#

FROM ubuntu:18.04

RUN \
    # Install everything except for Node.js
    apt-get update -y && \
    apt-get install -y curl openssh-server && \

    # Ubuntu 18.04's Node.js is too old for latest Prettier needs, grab a new one
    bash -c 'set -o pipefail && curl -fsSL https://deb.nodesource.com/setup_16.x | bash -' && \
    apt-get install -y nodejs && npm install --global prettier && \

    # Enable SSH server
    service ssh start && \

    # Create `test' user for SSH access
    useradd -rm -d /home/ubuntu -s /bin/bash -g root -G sudo -u 1000 test

# Setup `test' user's SSH authorization
USER test
RUN mkdir --mode=700 /home/ubuntu/.ssh
COPY --chown=test --chmod=600 id_rsa.pub /home/ubuntu/.ssh/authorized_keys

# Add the sample file
COPY --chown=test test.js /home/ubuntu/

# Run sshd as root on port 2001
USER root
EXPOSE 2001
CMD ["/usr/sbin/sshd", "-p", "2001", "-D"]
