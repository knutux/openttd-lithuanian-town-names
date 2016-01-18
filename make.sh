#!/bin/sh
NMLC=$(which nmlc)
TAR=$(which tar)
ROOT=$(dirname `readlink -f $0`)
VERSION=$(grep VERSION "${ROOT}/src/custom_tags.txt" | cut -d':' -f2)
NAME="lithuanian_town_names"
BUILD="Lithuanian_Town_Names-${VERSION}"

# Preliminary checks.
if [ -z ${NMLC} ]; then
    echo "ERROR: NML not found!"
    exit 1
fi

if [ -z ${VERSION} ]; then
    echo "ERROR: Failed to get version!"
    exit 1
fi

# Create the environment.
mkdir -p "${ROOT}/build/${BUILD}"
mkdir -p "${ROOT}/dist/"

# Build.
${NMLC} --grf="${ROOT}/build/${BUILD}/${NAME}.grf" \
        --custom-tags="${ROOT}/src/custom_tags.txt" \
        --lang-dir="${ROOT}/src/lang" \
        --default-lang="english.lng" \
        "${ROOT}/src/${NAME}.nml"

# If failed, then clean up and exit.
if [ $? -eq 1 ]; then
    echo "\n----------------------------Building failed-----------------------------\n"
    rm -R "${ROOT}/build/${BUILD}" "${ROOT}/parsetab.py"
    exit 1
fi

# Prepare the distribution archive.
for file in "CHANGELOG" "LICENSE" "README"; do
    lower=$(echo "${file}" | tr '[:upper:]' '[:lower:]')
    if [ -f "${ROOT}/${file}.md" ];
    then
        awk 'sub("$", "\r")' "${ROOT}/${file}.md" > "${ROOT}/build/${BUILD}/${lower}.txt"
    else
        awk 'sub("$", "\r")' "${ROOT}/${file}" > "${ROOT}/build/${BUILD}/${lower}.txt"
    fi
done

# Create distribution archive.
cd "${ROOT}/build/"
${TAR} -cf "${ROOT}/dist/${BUILD}.tar" "${BUILD}"

# Clean up.
rm -R "${ROOT}/build/${BUILD}" "${ROOT}/parsetab.py"

# Finish.
echo "\n---------------------------Building finished----------------------------\n"
echo "To install copy the NewGRF archive to the OpenTTD NewGRF directory:"
echo "    \$ cp ${ROOT}/dist/${BUILD}.tar ~/.openttd/newgrf/"
echo " [OR] in Windows git bash environment:"
echo "    \$ cp ${ROOT}/dist/${BUILD}.tar ~/Documents/OpenTTD/newgrf/"
exit 0
