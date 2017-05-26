FROM ubuntu:latest
MAINTAINER Random J Farmer <random.j.farmer@gmail.com>

EXPOSE 3000

# netbase for /etc/services, /etc/protocols
RUN apt-get update && apt-get install -y libgmp10 netbase ca-certificates

ENV APPROOT https://dorfl.gmeiner.me/

ADD static/ /static/
ADD config/ /config/
ADD .stack-work/install/x86_64-linux/lts-8.15/8.0.2/bin/hs-little-helper /

CMD ["/hs-little-helper"]
