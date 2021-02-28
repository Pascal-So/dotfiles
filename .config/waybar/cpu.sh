c1=$(grep 'cpu ' /proc/stat)
sleep 0.8
c2=$(grep 'cpu ' /proc/stat)

cpu=$(awk '{u=$2+$4; t=$2+$4+$5; if (NR==1){u1=u; t1=t;} else printf("%.1f%", ($2+$4-u1) * 100 / (t-t1)); }' <(echo -e "$c1\n$c2"))
ram=$(free -h | awk 'NR==2{print $3}')

echo "$cpu  $ram"
