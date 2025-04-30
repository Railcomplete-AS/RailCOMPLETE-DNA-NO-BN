@echo OFF
rem this script creates a csv file with information about issues from the current repo
rem in the parent folder of the repo.
rem It requires the Github cli and git for windows to be installed.
rem The script can be configured to select issues based on state or number and
rem to include issue contents or not.

set REPO=RailCOMPLETE-DNA-NO-BN

rem If No the report includes %LIMIT% issues counted from the newest.
rem else it includes issues from %FROMNUMBER% to %TONUMBER%. This option takes a long time.
set USEEXPLICITRANGE=NO

set LIMIT=100
set FROMNUMBER=7000
set TONUMBER=7100

rem STATE must be [open|closed|all]
set STATE=open

set INCLUDECONTENTS=NO

rem FIELDS must be an ordered comma separated list without spaces.
rem possible fields are: assignees, author, body, closed, closedAt, 
rem comments, createdAt, id, isPinned, labels, milestone, number, 
rem projectCards, projectItems, reactionGroups, state, stateReason,
rem title, updatedAt, url

if %INCLUDECONTENTS%==NO (
	set FIELDS=number,state,closedAt,createdAt,title
) else (
	set FIELDS=number,state,closedAt,createdAt,title,body
)

echo Querying github for issues without
echo Fields = %FIELDS%
echo State = %STATE%

if %USEEXPLICITRANGE%==NO (
	echo Limit = %LIMIT%
	Powershell -NoProfile -ExecutionPolicy Bypass -Command "& {gh issue list --limit %LIMIT% --state %STATE% --json %FIELDS% | ConvertFrom-Json | ForEach-Object { $_ } | Select-Object -property %FIELDS%| Export-Csv -Path ../%DATE%_Github_issues_%REPO%_%LIMIT%_Latest.csv -NoTypeInformation}"
	echo Wrote report to file %DATE%_Github_issues_%REPO%_%LIMIT%_Latest.csv
) else (
	echo Range = %FROMNUMBER% - %TONUMBER%
	echo This may take a while...
	Powershell -NoProfile -ExecutionPolicy Bypass -Command  "& {gh issue list --limit 100000 --state %STATE% --json %FIELDS% --jq '[.[]|select(.number >= %FROMNUMBER% and .number < %TONUMBER%)]'| ConvertFrom-Json | ForEach-Object { $_ } | Select-Object -property %FIELDS%| Export-Csv -Path ../%DATE%_Github_issues_%REPO%_%FROMNUMBER%-%TONUMBER%.csv -NoTypeInformation}"
	echo Wrote report to file %FILENAME%
)
pause