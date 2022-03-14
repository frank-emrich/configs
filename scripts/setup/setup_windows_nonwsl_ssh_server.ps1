# Install the OpenSSH Client
#Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0

# Install the OpenSSH Server
#Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0

$packages = Get-WindowsCapability -Online | Where-Object Name -like 'OpenSSH*' | select -exp Name

foreach ($package in $packages)
{
    Add-WindowsCapability -Online -Name $package
}

# Make it autostart
Set-Service -Name sshd -StartupType 'Automatic'

# Generate key pair in Windows (!) user folder under .ssh
ssh-keygen -t ed25519 -N '""'


$OurKeyPath = join-path -path $HOME -childpath ".ssh\id_ed25519.pub"
$OurKey = Get-Content $OurKeyPath
$LPCPubKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnLs2R6OaWOJu1q0+Vdp5FfXwh8W/cVvsHm93UXLE1gdrTw+dWin7h3TQoopsS+8265FEblQ3nPNIGZQPfWHrNuzlpNSa1Xobr1o6m+KJ5Ko3bI0wzn5u4pgEstdQ3k3IYIihmxIjrfnO9LBSDC5CUtYID+Hgex2a2SQaF3bNgM3xFlKThEDO5HiToT7iyq4JSay1VnJzcAVeoxgvzhbzA4HuF9j3ZH8XhyyvSbUPPlihkto5c5JRd5htsjgkEXbdd9SoUHMIzM4ax06b3ZCUk/nXJpNl5oEFzV52k9OFhZlS5LAJfekqgtiAnjrkJDNeq4b9cSCLqVCCn38KjV7XVfRCYpfbRAkgmZHFCrrNWpQO3NYZjDK1wlOq7lG68p6/s6cH6ShC3J34gv0im+NLxA4DyD/In65lIAvCKFNZXJQ0GzfqvANAsvl/eOQshctXcXklSZ9QRWagJPACcuDmsvfidfS7YPZnhycjZJML5YRV+DWkzxE8nfFFjkL7xUORdTnjuxqzIiCnZfTlq3GWWogJDyREE/2cuTPIvA2GADNL1H4PxE599toA6xTj1exkpelBS0b46FD5x8D8Qm8E899W9LyLWIo7Bhdit1cAyY2tpHOUiepk1UNnFJtYn1m8ERkVGLeZJCNF/fW+heYfyrnnW+kAKCLDvcjp1uJHXhw== frank@frank-lpc"

write-host $OurKey

$OutPath = join-path -path $env:ProgramData -childpath "ssh\administrators_authorized_keys"
write-host $OutPath
Add-Content -Path $OutPath -Value $LPCPubKey

# We must set the permissions of administrators_authorized_keys, otherwise authentication fails silently!
icacls.exe $OutPath /inheritance:r /grant "Administrators:F" /grant "SYSTEM:F"


# disable password login
$SSHDConfigPath = join-path -path $env:ProgramData -childpath "ssh\sshd_config"
Add-Content -Path $SSHDConfigPath -Value "PasswordAuthentication no"



Start-Service sshd

# Disable firewall rule so that only localhost traffic is allowed
$Rule = Get-NetFirewallRule | Where-Object DisplayName -like 'OpenSSH SSH Server*' 
Disable-NetFirewallRule -InputObject $Rule


# install our public key in WSL
# note that this doesn't help for using the Windows SSH server as a jump host!
$WSLPath = wsl.exe --exec wslpath $OurKeyPath
$BashCmd = ( '"cat ' + $WSLPath + ' >> ~/.ssh/authorized_keys ; chmod 644  ~/.ssh/authorized_keys"')
wsl.exe --exec bash -c $BashCmd
