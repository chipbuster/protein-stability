FROM julia:1.3.1

# /root already exists on this image--use that as the homedir

WORKDIR /root

COPY core /root/core
COPY langevin-sim /root/langevin-sim
COPY setup.jl /root
RUN mkdir /root/data

RUN julia setup.jl

ENTRYPOINT ["julia","langevin-sim/run_sim.jl"]
