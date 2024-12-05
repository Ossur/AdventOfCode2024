#!/bin/env fish

if test 2 -gt (count $argv)
	echo "USAGE: ./a_new_day [DAY_NUMBER] [DAY_NAME]"
	exit
end

mkdir Day$argv[1]

set fs_file "Day$argv[1]/$argv[2].fs"

echo "module Day$argv[1]" > $fs_file
cat ./NewDayTemplate.fsx >> $fs_file

# there is a sed function to do this more elegantly I'm sure
echo "
  <ItemGroup>
    <Compile Include=\"./$fs_file\" />
  </ItemGroup>
" >> ./AdventOfCode2024.fsproj

vim ./AdventOfCode2024.fsproj Day$argv[1]/TestInput.txt Day$argv[1]/RealInput.txt

dotnet watch run $argv[1]
