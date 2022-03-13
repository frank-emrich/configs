# Start SSH and cron in WSL at logon

# https://docs.microsoft.com/en-us/powershell/module/scheduledtasks/new-scheduledtasktrigger
$Trigger = New-ScheduledTaskTrigger -AtLogon

# https://docs.microsoft.com/en-us/powershell/module/scheduledtasks/new-scheduledtaskaction
# If you want to update a specific distro, you can add "--distribution <DistributionName>"
$Actions = @(
    New-ScheduledTaskAction -Execute "wsl" -Argument "--user root --exec /usr/sbin/service ssh start"
    New-ScheduledTaskAction -Execute "wsl" -Argument "--user root --exec /usr/sbin/service cron start"
)

# https://docs.microsoft.com/en-us/powershell/module/scheduledtasks/register-scheduledtask
Register-ScheduledTask -TaskName "Start WSL services" -Trigger $Trigger -Action $Actions
