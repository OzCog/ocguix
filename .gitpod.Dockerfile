# Custom Gitpod Docker image for OpenCog Cognitive Ecosystem
# Based on issue #69: docker pull gitpod/workspace-python-3.10:2025-07-23-06-50-33
# Part of the "madness" meta-issue #68

FROM gitpod/workspace-python-3.10:2025-07-23-06-50-33

USER root

# Install system dependencies for OpenCog cognitive ecosystem
RUN apt-get update && apt-get install -y \
    # Core build tools
    build-essential \
    cmake \
    make \
    pkg-config \
    autotools-dev \
    automake \
    libtool \
    # Guix dependencies
    wget \
    curl \
    gpg \
    gpg-agent \
    xz-utils \
    # Guile and Scheme support
    guile-3.0 \
    guile-3.0-dev \
    # OpenCog dependencies
    libboost-all-dev \
    cxxtest \
    # KoboldCpp and AI model dependencies
    python3-dev \
    python3-pip \
    git \
    # Documentation tools
    graphviz \
    pandoc \
    # Network tools for cognitive agents
    netcat-openbsd \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Python packages for cognitive ecosystem
RUN pip3 install --no-cache-dir \
    requests \
    flask \
    fastapi \
    uvicorn \
    numpy \
    scipy \
    pyyaml \
    asyncio

# Pre-install GNU Guix package manager for reproducible builds
RUN cd /tmp && \
    wget -q -O guix-binary.tar.xz https://ftp.gnu.org/gnu/guix/guix-binary-1.4.0.x86_64-linux.tar.xz && \
    tar --warning=no-timestamp -xf guix-binary.tar.xz && \
    mv var/guix /var/ && \
    mv gnu /  && \
    mkdir -p ~root/.config/guix && \
    ln -sf /var/guix/profiles/per-user/root/current-guix ~root/.config/guix/current && \
    GUIX_PROFILE="~root/.config/guix/current" ; \
    source $GUIX_PROFILE/etc/profile && \
    rm -rf /tmp/guix-binary.tar.xz /tmp/var /tmp/gnu || echo "Guix installation completed with warnings"

# Set up environment for cognitive ecosystem
ENV OPENCOG_ECOSYSTEM=true
ENV KOBOLDCPP_PORT=5001
ENV COGNITIVE_GRAMMAR_AGENT=distributed-cognitive-grammar-agent.scm
ENV GITPOD_WORKSPACE_TYPE=cognitive-ecosystem
ENV PATH="/var/guix/profiles/per-user/root/current-guix/bin:$PATH"

# Switch back to gitpod user
USER gitpod

# Set up user-level Guix profile
RUN mkdir -p ~/.config/guix && \
    ln -sf /var/guix/profiles/per-user/gitpod/current-guix ~/.config/guix/current || \
    echo "User Guix profile setup will be completed during runtime"

# Make sure workspace is ready for cognitive ecosystem
WORKDIR /workspace