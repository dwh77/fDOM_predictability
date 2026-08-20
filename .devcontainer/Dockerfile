FROM rocker/tidyverse:latest

ARG DEBIAN_FRONTEND=noninteractive
ARG TZ=America/Los_Angeles
ENV DEBIAN_FRONTEND=${DEBIAN_FRONTEND} \
    TZ=${TZ}

RUN apt-get update && \
	apt-get install --yes \
		bison \
		build-essential \
		clang \
		cmake \
		dirmngr \
		flex \
		git \
		libbsd-dev \
		libfreetype6-dev \
		libfribidi-dev \
		libfontconfig1-dev \
		libgdal-dev \
		libglib2.0 \
		libglib2.0-dev \
		libharfbuzz-dev \
		libjpeg-dev \
		libnetcdf-dev \
		libomp-dev \
		libpq-dev \
		libpng-dev \
		libssl-dev \
		libtiff5-dev \
		libudunits2-dev \
		libxml2-dev \
		pkg-config \
		python3 \
		software-properties-common \
		vim \
		wget && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/*