#!/bin/bash
BASHSCRIPT_DIR="/groups/umcg-lifelines/tmp02/projects/ov21_0226/PGI_Pipeline_lalajaasko/BashScripts"
source "${BASHSCRIPT_DIR}/config.sh"

OUTPUT_DIR="/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/INPUT"
mkdir -p "${OUTPUT_DIR}"
OUTPUT="${OUTPUT_DIR}/EApgis.txt"

# Trait definitions: FILE|SHORTNAME
declare -A TRAITS
TRAITS["EA3Cognitive"]="EA3Cog"
TRAITS["EA3NonCognitive"]="EA3NonCog"
TRAITS["EA4"]="EA4"

TRAIT_ORDER="EA3Cognitive EA3NonCognitive EA4"

echo "Combining PGI files..."

# Step 1: Start with base file (FID, IID, FATHER_ID, MOTHER_ID) from first trait
FIRST_TRAIT="EA3Cognitive"
FIRST_FILE="${PGI_SCORES}/${FIRST_TRAIT}/${FIRST_TRAIT}_merged.pgs.txt"

awk 'BEGIN{OFS="\t"} NR>1 {print $1,$2,$3,$4}' "${FIRST_FILE}" > "${OUTPUT_DIR}/base.txt"

# Step 2: For each trait, extract proband + mean(paternal,maternal)
JOINED="${OUTPUT_DIR}/base.txt"

for TRAIT in ${TRAIT_ORDER}; do
    SHORT="${TRAITS[$TRAIT]}"
    MERGED="${PGI_SCORES}/${TRAIT}/${TRAIT}_merged.pgs.txt"

    if [[ ! -f "${MERGED}" ]]; then
        echo "  WARNING: ${MERGED} not found, skipping"
        continue
    fi

    echo "  Processing ${TRAIT} -> pgi_${SHORT}, pgi_sum_${SHORT}..."

    # Extract IID, proband, mean(paternal+maternal)
    awk 'BEGIN{OFS="\t"} NR>1 {
        mean_par = ($6 + $7) / 2
        print $2, $5, mean_par
    }' "${MERGED}" > "${OUTPUT_DIR}/tmp_${SHORT}.txt"

    # Join on IID (col2 of joined, col1 of trait file)
    awk -v short="${SHORT}" 'BEGIN{OFS="\t"}
        NR==FNR {pgi[$1]=$2; sum[$1]=$3; next}
        FNR==1  {next}
        {
            iid=$2
            if (iid in pgi) print $0, pgi[iid], sum[iid]
            else             print $0, "NA", "NA"
        }' "${OUTPUT_DIR}/tmp_${SHORT}.txt" "${JOINED}" > "${OUTPUT_DIR}/tmp_joined.txt"

    mv "${OUTPUT_DIR}/tmp_joined.txt" "${JOINED}"
done

# Step 3: Add header
HEADER="FID\tIID\tFATHER_ID\tMOTHER_ID"
for TRAIT in ${TRAIT_ORDER}; do
    SHORT="${TRAITS[$TRAIT]}"
    HEADER="${HEADER}\tpgi_${SHORT}\tpgi_sum_${SHORT}"
done

{ echo -e "${HEADER}"; cat "${JOINED}"; } > "${OUTPUT}"

# Cleanup
rm -f "${OUTPUT_DIR}/base.txt" "${OUTPUT_DIR}/tmp_"*.txt

n=$(wc -l < "${OUTPUT}")
echo ""
echo "Done: $((n-1)) individuals -> ${OUTPUT}"
echo "Columns: $(head -1 ${OUTPUT})"