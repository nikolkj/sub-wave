#!/bash/sh
cd api-out-stage/
for input_file in *.wav; do
    [ -f "$input_file" ] || continue
    output_file="${input_file%.wav}.m4a"
    ffmpeg -i "$input_file" "$output_file"
    #echo "${file%.wav}.m4a"
done

