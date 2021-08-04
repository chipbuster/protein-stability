

```sh
mkdir build
cd build
conan install ..

# Change as needed
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++ -DCMAKE_C_COMPILER=gcc
make
./bin/run-mcmc
```
