# grep is the natural tool here, and it is a separate C program: this row is grep's number with
# bash's process startup around it, not bash scanning text.
a=$(grep -c "LEVEL=" rundir/crosslang-lines.txt)
b=$(grep -c "ERROR" rundir/crosslang-lines.txt)
echo "result=$((a + b))"
