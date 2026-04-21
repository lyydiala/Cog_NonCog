#!/bin/bash
CODE_DIR="/groups/umcg-lifelines/tmp02/projects/ov21_0226/PGI_Pipeline_lalajaasko/Code"
source "${CODE_DIR}/config.sh"

OUTPUT_DIR="/groups/umcg-lifelines/tmp02/projects/ov21_0226/lalajaasko/Cog_NonCog/INPUT"
mkdir -p "${OUTPUT_DIR}"
OUTPUT="${OUTPUT_DIR}/EApgis.txt"

TRAITS="EA3Cog EA3NonCog EA4"
METHODS="SBayesR SBayesRC"

echo "Combining PGI files..."

# Step 1: Build base file (FID, IID, FATHER_ID, MOTHER_ID) from first trait/method
FIRST_FILE="${PGI_SCORES}/EA3Cog/SBayesR/merged.pgs.txt"
awk 'BEGIN{OFS="\t"} NR>1 {print $1,$2,$3,$4}' "${FIRST_FILE}" > "${OUTPUT_DIR}/base.txt"

# Step 2: For each trait x method, extract proband + mean(paternal, maternal) and join
JOINED="${OUTPUT_DIR}/base.txt"

for TRAIT in ${TRAITS}; do
    for METHOD in ${METHODS}; do
        LABEL="${TRAIT}_${METHOD}"
        MERGED="${PGI_SCORES}/${TRAIT}/${METHOD}/merged.pgs.txt"

        if [[ ! -f "${MERGED}" ]]; then
            echo "  WARNING: ${MERGED} not found, skipping"
            continue
        fi

        echo "  Processing ${LABEL} -> pgi_${LABEL}, pgi_sum_${LABEL}..."

        # Extract IID, proband score, mean(paternal, maternal)
        awk 'BEGIN{OFS="\t"} NR>1 {
            mean_par = ($6 + $7) / 2
            print $2, $5, mean_par
        }' "${MERGED}" > "${OUTPUT_DIR}/tmp_${LABEL}.txt"

        # Join on IID (col2 of joined, col1 of trait file)
        awk 'BEGIN{OFS="\t"}
            NR==FNR {pgi[$1]=$2; par[$1]=$3; next}
            {
                iid=$2
                if (iid in pgi) print $0, pgi[iid], par[iid]
                else             print $0, "NA", "NA"
            }' "${OUTPUT_DIR}/tmp_${LABEL}.txt" "${JOINED}" > "${OUTPUT_DIR}/tmp_joined.txt"

        mv "${OUTPUT_DIR}/tmp_joined.txt" "${JOINED}"
    done
done

# Step 3: Join PCs on genotyping IID
echo "Joining PCs..."
PC_FILE="/groups/umcg-lifelines/tmp02/projects/ov21_0226/PGI_Pipeline_lalajaasko/OUTPUT/pca/pcs.sscore"

awk 'BEGIN{OFS="\t"}
    NR==FNR {if (FNR>1) pcs[$2]=$5"\t"$6"\t"$7"\t"$8"\t"$9"\t"$10"\t"$11"\t"$12"\t"$13"\t"$14; next}
    {print $0, (($2 in pcs) ? pcs[$2] : "NA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA")}
' "${PC_FILE}" "${JOINED}" > "${OUTPUT_DIR}/tmp_joined.txt"
mv "${OUTPUT_DIR}/tmp_joined.txt" "${JOINED}"

# Step 4: Standardise each PGI pair independently using proband mean and SD
# Column layout (0-based):
#   0-3:   FID, IID, FATHER_ID, MOTHER_ID
#   4,5:   pgi_EA3Cog_SBayesR,   pgi_sum_EA3Cog_SBayesR
#   6,7:   pgi_EA3Cog_SBayesRC,  pgi_sum_EA3Cog_SBayesRC
#   8,9:   pgi_EA3NonCog_SBayesR, pgi_sum_EA3NonCog_SBayesR
#   10,11: pgi_EA3NonCog_SBayesRC, pgi_sum_EA3NonCog_SBayesRC
#   12,13: pgi_EA4_SBayesR,       pgi_sum_EA4_SBayesR
#   14,15: pgi_EA4_SBayesRC,      pgi_sum_EA4_SBayesRC
#   16-25: PC1-PC10
echo "Standardising PGIs..."
python3 << 'PYEOF' - "${JOINED}" "${JOINED}.std"
import sys, math

infile, outfile = sys.argv[1], sys.argv[2]

# Proband cols and their corresponding parental cols (0-based)
proband_cols  = [4,  6,  8,  10, 12, 14]
parental_cols = [5,  7,  9,  11, 13, 15]

with open(infile) as f:
    rows = [line.rstrip("\n").split("\t") for line in f]

# Compute mean and SD per proband column independently
stats = {}
for col in proband_cols:
    vals = []
    for row in rows:
        try: vals.append(float(row[col]))
        except: pass
    n = len(vals)
    mean = sum(vals) / n
    sd   = math.sqrt(sum((v - mean)**2 for v in vals) / (n - 1))
    stats[col] = (mean, sd)

# Standardise proband and parental column using the proband's mean/SD
with open(outfile, "w") as f:
    for row in rows:
        for p_col, par_col in zip(proband_cols, parental_cols):
            mean, sd = stats[p_col]
            for c in [p_col, par_col]:
                try:
                    row[c] = str((float(row[c]) - mean) / sd)
                except:
                    row[c] = "NA"
        f.write("\t".join(row) + "\n")

print(f"Standardised {len(proband_cols)} PGI pairs independently")
PYEOF
mv "${JOINED}.std" "${JOINED}"

# Step 5: Build linkage map (genotyping_ID -> project_pseudo_id)
echo "Building linkage map..."
LINKAGE_MAP="${OUTPUT_DIR}/tmp_linkage.txt"

awk 'BEGIN{OFS="\t"} NR>1 {print $2 "_" $2, $1}' "${LINKAGE_UGLI1}" > "${LINKAGE_MAP}"
awk 'BEGIN{OFS="\t"} NR>1 {gsub(/\r/, ""); print $2, $1}' "${LINKAGE_CYTO}" >> "${LINKAGE_MAP}"
awk 'BEGIN{FS=","; OFS="\t"} NR>1 {print "1_" $2, $1}' "${LINKAGE_UGLI2}" >> "${LINKAGE_MAP}"

# Step 6: Translate IID to project_pseudo_id and write final output
echo "Translating IDs to project_pseudo_id..."
{
    echo -e "IID\tpgi_EA3Cog_SBayesR\tpgi_sum_EA3Cog_SBayesR\tpgi_EA3Cog_SBayesRC\tpgi_sum_EA3Cog_SBayesRC\tpgi_EA3NonCog_SBayesR\tpgi_sum_EA3NonCog_SBayesR\tpgi_EA3NonCog_SBayesRC\tpgi_sum_EA3NonCog_SBayesRC\tpgi_EA4_SBayesR\tpgi_sum_EA4_SBayesR\tpgi_EA4_SBayesRC\tpgi_sum_EA4_SBayesRC\tPC1\tPC2\tPC3\tPC4\tPC5\tPC6\tPC7\tPC8\tPC9\tPC10"
    awk 'BEGIN{OFS="\t"}
        NR==FNR {uuid[$1]=$2; next}
        {
            iid=$2
            if (iid in uuid) print uuid[iid], $5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$20,$21,$22,$23,$24,$25,$26
            else             print "UNMATCHED_" iid, $5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$20,$21,$22,$23,$24,$25,$26
        }' "${LINKAGE_MAP}" "${JOINED}"
} > "${OUTPUT}"

# Cleanup
rm -f "${OUTPUT_DIR}/base.txt" "${OUTPUT_DIR}/tmp_"*.txt "${LINKAGE_MAP}"

n=$(wc -l < "${OUTPUT}")
echo ""
echo "Done: $((n-1)) individuals -> ${OUTPUT}"
echo "Columns: $(head -1 ${OUTPUT})"
unmatched=$(awk 'NR>1 && $1 ~ /^UNMATCHED_/' "${OUTPUT}" | wc -l)
echo "Unmatched IDs: ${unmatched}"