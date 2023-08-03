#!/bin/bash

args=("$@")

if [ "$#" -eq 0 ]; then
    args=(1 3 5 10 13 17)
    echo "No args specified, using defaults."
fi

rm -rf "out"
mkdir "out"

echo "Generating custom presets with thresholds ${args[@]}"

cp ~/.audacity-data/pluginsettings.cfg "tmp-config.txt"
# Note--if you change this to point to the orig, also don't delete it at the end
tmpSettingsFile="tmp-config.txt"

# [pluginsettings/base64\:RWZmZWN0X055cXVpc3RfU3RldmUgRGF1bHRvbl9MYWJlbCBTb3VuZHM\=/private/UserPresets/region]
pluginPrefix=$(cat "${tmpSettingsFile}" | grep "private/UserPresets/region]" | sed -E 's/(.*)region.*/\1/g')
pluginPostfix=$(cat "${tmpSettingsFile}" | grep "private/UserPresets/region]" | sed -E 's/(.*)region(.*)/\2/g')
pluginId=$(echo "${pluginPrefix}" | sed -E 's/.*base64..((\w)+).*/\1/g')
echo

# The lines that are related to this plugin:
startLine=$(cat "${tmpSettingsFile}" | grep -nh "${pluginId}" | cut -f1 -d: | head -1)
endLine=$(cat "${tmpSettingsFile}" | grep -nh "${pluginId}" | cut -f1 -d: | tail -1)
endLine=$((endLine+1))
echo "Found the following presets in the config file, on lines ${startLine} to ${endLine}:"
tail -n "+$startLine" "${tmpSettingsFile}" | head -n "$((endLine-startLine+1))" | grep --color "UserPreset.*\|$"
echo
echo Add or replace lines at your own risk!

# The sources of truth:
regionLineNum=$(cat "${tmpSettingsFile}" | grep -nh "private/UserPresets/region]" | cut -f1 -d:)
regionLineNum=$((regionLineNum+1))
regionLine=$(cat "${tmpSettingsFile}" | head -"${regionLineNum}" | tail -1)
pointLineNum=$(cat "${tmpSettingsFile}" | grep -nh "private/UserPresets/point]" | cut -f1 -d:)
pointLineNum=$((pointLineNum+1))
pointLine=$(cat ${tmpSettingsFile} | head -"${pointLineNum}" | tail -1)

for i in "${args[@]}"
do
   re='^[0-9]+(\.[0-9]+)?$'
   if ! [[ $i =~ $re ]] ; then
      echo "Skipping $i since it's not a valid number: $i"
   else
      # point-i
      echo "${pluginPrefix}point-${i}${pluginPostfix}"
      beg='s/(threshold="-)[0-9]+(\.[0-9]+)?"/\1'
      end='"/g'
      echo $pointLine | sed -r "${beg}${i}${end}"
      # region-i
      echo "${pluginPrefix}region-${i}${pluginPostfix}"
      echo $regionLine | sed -r "${beg}${i}${end}"

      # Label clicks-i
      beg='s/Preset:point"/Preset:point-'
      sed "${beg}${i}${end}" "Label clicks.txt" > "out/Label clicks-${i}.txt"
      beg='s/Preset:region"/Preset:region-'
      sed -i "${beg}${i}${end}" "out/Label clicks-${i}.txt"
   fi
done

echo
ls out/*
rm "${tmpSettingsFile}"
echo -e "\nDo NOT put any macros in audacity's macros folder that, if you remove spacing,\nhave the same name! It WILL completely corrupt the next audacity project you open.\nThere's a test project in this folder you can feel free to clobber as a test." | grep --color ".*"