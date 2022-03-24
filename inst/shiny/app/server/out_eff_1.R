coln =c("Treatment",
        "N","Mean (SD)",
        "N","Mean (SD)",
        "N","Mean (SD)","LS Mean (95% CI)")
colgr=c(1,2,2,3,3,4,4,4)
colgrn=c("","Baseline","Week 20","Change from Baseline")
collist = lapply(1:ncol(apr0ancova1),function(xx) colDef(name=coln[xx]))
names(collist) = names(apr0ancova1)

reactable(
  apr0ancova1,
  columns = collist,
  columnGroups = list(
    colGroup(name = colgrn[2], columns = names(apr0ancova1)[colgr==2]),
    colGroup(name = colgrn[3], columns = names(apr0ancova1)[colgr==3]),
    colGroup(name = colgrn[4], columns = names(apr0ancova1)[colgr==4])
  )
)
