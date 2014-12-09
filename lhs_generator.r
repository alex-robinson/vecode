#!/opt/local/bin/Rscript --vanilla
#!/home/robinson/apps/R/R/bin/Rscript --vanilla
args = commandArgs(TRUE)

require(lhs)


if (length(args)<2) {
    cat("\n\n",
        "  Error: lhs_generator requires 2 arguments: np ns\n",
        "      np = number of parameters\n",
        "      ns = number of samples\n\n")
}

## Generate lhs samples as needed
np = as.integer(args[1])
ns = as.integer(args[2])
filename = paste0("LHS/lhs_np",np,"_ns",ns,".txt")

dat = randomLHS(n=ns,k=np)
out = format(dat,digits=3,width=10,justify="right")
write.table(out,file=filename,row.names=FALSE,col.names=FALSE,quote=FALSE)

cat("LHS table written: ",filename,"\n")

