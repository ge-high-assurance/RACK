#!/bin/bash

# Exit if anything goes wrong

set -eo pipefail

# Disable interactive configuration steps

export DEBIAN_FRONTEND=noninteractive
export DEBCONF_NONINTERACTIVE_SEEN=true

# Avoid https://bugs.launchpad.net/ubuntu/+source/linux/+bug/1766857
# if we're building a Hyper-V VM.  Also, we needed to install
# linux-cloud-tools-virtual to let Packer communicate with the Hyper-V
# VM, but we don't want it installed in the VirtualBox VM.

if [ "${PACKER_BUILDER_TYPE}" == "hyperv-iso" ]; then
    mkdir /usr/libexec/hypervkvpd
    cd /usr/libexec/hypervkvpd
    ln -s /usr/sbin/hv_get_dhcp_info .
    ln -s /usr/sbin/hv_get_dns_info .
elif [ "${PACKER_BUILDER_TYPE}" == "virtualbox-iso" ]; then
    apt-get remove -yqq linux-cloud-tools-common linux-cloud-tools-virtual
    apt-get autoremove -yqq # currently not needed, but just in case
fi

# Upgrade all packages

apt-get -o Acquire::Check-Valid-Until=false -o Acquire::Check-Date=false update -yqq
apt-get upgrade -yqq

# Clean apt cache and temporary files

apt-get clean -y
rm -rf /tmp/files /var/lib/apt/lists/*

# Execute this part of the script only if we have a virtual hard disk

if [ "${PACKER_BUILDER_TYPE}" != "docker" ]; then

    # Zero out the rest of the free space using dd, then delete the written file

    dd if=/dev/zero of=/EMPTY bs=1M || true
    rm -f /EMPTY

    # Add `sync` so Packer doesn't quit too early, before the large file is deleted

    sync

fi
