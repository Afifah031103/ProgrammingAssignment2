# Fungsi untuk membuat objek matriks khusus yang bisa menyimpan inversnya
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # variabel untuk menyimpan invers
  
  set <- function(y) {
    x <<- y      # simpan matriks baru
    inv <<- NULL # reset invers ketika matriks berubah
  }
  
  get <- function() x              # mengambil matriks
  setInverse <- function(inverse) inv <<- inverse  # menyimpan invers
  getInverse <- function() inv     # mengambil invers dari cache
  
  # mengembalikan list berisi fungsi-fungsi di atas
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Fungsi untuk menghitung (atau mengambil) invers dari matriks khusus
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()   # coba ambil invers dari cache
  
  if (!is.null(inv)) {
    message("Mengambil data dari cache...")
    return(inv)
  }
  
  # jika tidak ada di cache, hitung invers
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)       # simpan ke cache
  inv
}
