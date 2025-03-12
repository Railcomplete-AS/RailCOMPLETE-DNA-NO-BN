PowerShell -Command "&{ gh issue list --limit 100 --state all | foreach-object {$_ -replace(\"\t\",\";\")} | Out-File -FilePath \"..\NO-BN_issues.csv\"}"
pause