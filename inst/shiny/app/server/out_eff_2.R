coln =c("",
        "Difference in LS Mean (95% CI)",
        "p-Value")
collist = lapply(1:ncol(apr0ancova2),function(xx){
  if(xx>1){colDef(name=coln[xx])
  }else{colDef(name=coln[xx],footer=apr0ancova3$rmse)}
})
names(collist) = names(apr0ancova2)

reactable(
  apr0ancova2,
  columns = collist,
  defaultColDef = colDef(footerStyle = list(fontStyle = "itatlic"))
)
