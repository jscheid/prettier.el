#
# This Dockerfile creates the "emacs" container. It has Emacs 28
# installed as well as prettier.el and the root user has passwordless
# access to the SSH host `remote' configured. It contains a file
# `/root/test.el' which, when run, remotely formats
# `/home/ubuntu/test.js' on `remote'.
#

FROM silex/emacs:28

# Install Prettier
RUN emacs --batch --eval '(progn \
    (package-initialize) \
    (add-to-list (quote package-archives) (cons "melpa" "http://melpa.org/packages/") t) \
    (package-refresh-contents) \
    (package-install (quote prettier)))'

# Add SSH config
ADD ssh-config /root/.ssh/config
ADD id_rsa /root/.ssh/id_rsa
ADD id_rsa /root/id_rsa
ADD id_rsa /id_rsa

# Patch up SSH config for use inside the container
RUN sed -i.bak -e 's,localhost,remote,' -e 's,id_rsa,/root/.ssh/id_rsa,' /root/.ssh/config

# Add test script
ADD test.el /root

# Replace default command
CMD ["sh", "-c", "/root/test.el"]
