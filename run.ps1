[CmdletBinding()]
Param(
    [Parameter(Position=0,mandatory=$true)]
    [string] $fileName
)

cat $fileName | dotnet run > "$($fileName).out"