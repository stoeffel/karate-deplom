for f in $(ls assets/*.png)
do
   convert "$f" -transparent white "$f"
done
