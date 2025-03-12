
gh search prs --author=@me --json createdAt --template '{{range .}}{{timefmt "2006/1/1" .createdAt | tablerow}}{{end}}'
PowerShell -Command "&{ gh issue list --limit 100 --state all | foreach-object {$_ -replace(\"\t\",\";\")} | Out-File -FilePath \"..\NO-BN_issues.csv\"}"
pause