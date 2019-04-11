# figure/title captioner
t.cap <- captioner::captioner(prefix="Table.")
f.cap <- captioner::captioner(prefix="Figure.")

t.ref <- function(label){
  stringr::str_extract(t.cap(label), "[^:]*")
}

f.ref <- function(label){
  stringr::str_extract(f.cap(label), "[^:]*")
}