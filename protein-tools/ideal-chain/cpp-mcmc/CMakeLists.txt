cmake_minimum_required(VERSION 3.10)
project(MD5Encrypter)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

find_package(OpenMP)

add_executable(mcmc-run mcrun.cpp main.cpp)
target_link_libraries(mcmc-run PUBLIC ${CONAN_LIBS})

if(OpenMP_CXX_FOUND)
    target_link_libraries(mcmc-run PUBLIC OpenMP::OpenMP_CXX)
endif()