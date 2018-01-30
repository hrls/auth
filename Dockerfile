from alpine:latest as builder

run apk --no-cache add \
    gcc g++ \
    zlib zlib-dev \
    ghc cabal

run mkdir /root/bin
copy . /auth
workdir /auth
run cabal update
run cabal --bindir=/root/bin install


from alpine:latest
run apk --no-cache add \
    musl gmp zlib libffi

workdir /root/bin
copy --from=builder /root/bin/auth .
# run ldd auth

env TWITTER_API_KEY mMpXSpEsEbc303SHgR2bDvesd
env TWITTER_API_SECRET 176brU8PuQRNccPl881lbzF3atd805fvjKxB58muS7nV8qKfk2

expose 5000
cmd ["./auth"]

