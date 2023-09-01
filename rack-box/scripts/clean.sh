#!/bin/bash

# Exit if anything goes wrong
echo 'STARTING CLEAN SCRIPT'
set -eo pipefail

# Disable interactive configuration steps

export DEBIAN_FRONTEND=noninteractive
export DEBCONF_NONINTERACTIVE_SEEN=true

# Avoid https://bugs.launchpad.net/ubuntu/+source/linux/+bug/1766857
# if we're building a Hyper-V VM.  Also, we needed to install
# linux-cloud-tools-virtual to let Packer communicate with the Hyper-V
# VM, but we don't want it installed in the VirtualBox VM.

echo 'SETTING HYPERV'
if [ "${PACKER_BUILDER_TYPE}" == "hyperv-iso" ]; then
    echo 'CDEBUG: IS HYPERV ISO'
    mkdir /usr/libexec/hypervkvpd
    echo 'CDEBUG: MADE DIR'
    cd /usr/libexec/hypervkvpd
    echo 'CDEBUG: LINKING DIR'
    ln -s /usr/sbin/hv_get_dhcp_info .
    echo 'CDEBUG: LINKING DIR2'
    ln -s /usr/sbin/hv_get_dns_info .
elif [ "${PACKER_BUILDER_TYPE}" == "virtualbox-iso" ]; then
    echo 'CDEBUG: IS VIRTUAL ISO'
    apt-get remove -yqq linux-cloud-tools-common linux-cloud-tools-virtual
    echo 'CDEBUG: REMOVED THINGS'
    apt-get autoremove -yqq # currently not needed, but just in case
fi

# Upgrade all packages
echo 'CDEBUG: UPGRADING PACKAGES'
apt-get -o Acquire::Check-Valid-Until=false -o Acquire::Check-Date=false update -yqq
echo 'CDEBUG: UPGRADING UPDATES'
apt-get upgrade -yqq

# Clean apt cache and temporary files
echo 'CDEBUG: UPGRADING APT-GET'
apt-get clean -y
echo 'CDEBUG: APT-GET CLEANED'
rm -rf /tmp/files /var/lib/apt/lists/*

# Execute this part of the script only if we have a virtual hard disk
echo 'CDEBUG: CHECKING DOCKER'
if [ "${PACKER_BUILDER_TYPE}" != "docker" ]; then

    # Zero out the rest of the free space using dd, then delete the written file
    echo 'CDEBUG: NOT DOCKER'
    dd if=/dev/zero of=/EMPTY bs=1M || true
    echo 'CDEBUG: DD CMD DONE'
    rm -f /EMPTY

    # Add `sync` so Packer doesn't quit too early, before the large file is deleted
    echo 'CDEBUG: SYNCING'
    sync

fi
