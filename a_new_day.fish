#!/bin/env fish

if test 2 -gt (count $argv)
	echo "USAGE: ./a_new_day [DAY_NUMBER] [DAY_NAME]"
	exit
end

mkdir Day$argv[1]

set fs_file "Day$argv[1]/$argv[2].fs"

echo "module Day$argv[1]

open System.Text.RegularExpressions
open System.IO

let ( testData, realData ) = 
    let readFile filename = 
        Path.Combine(__SOURCE_DIRECTORY__, filename) |> System.IO.File.ReadAllLines 
    ( readFile \"TestInput.txt\" , readFile \"RealInput.txt\" ) 
" > $fs_file

echo "
  <ItemGroup>
    <Compile Include=\"./$fs_file\" />
  </ItemGroup>
" >> ./AdventOfCode2024.fsproj

vim ./AdventOfCode2024.fsproj Day$argv[1]/TestInput.txt Day$argv[1]/RealInput.txt

dotnet watch run $argv[1]



