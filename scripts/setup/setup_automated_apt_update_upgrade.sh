#!/bin/bash

# Needs to run as root!
# Don't forget to have cron started in wsl, e.g., create task that does
# wsl --user root --exec /usr/sbin/service cron start


SCRIPT="/root/apt_update_upgrade.sh"

cat > "$SCRIPT" <<EOF
#!/bin/bash
apt update && apt upgrade --yes
EOF

chmod u+x "$SCRIPT"

cat > /etc/cron.d/apt_update_upgrade <<EOF
0 18 * * 0 root /root/apt_update_upgrade.sh
EOF

