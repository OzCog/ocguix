FROM debian:bookworm

# Install basic dependencies
RUN apt-get update && \
    apt-get install -y \
    wget \
    xz-utils \
    git \
    curl \
    build-essential \
    pkg-config \
    sudo \
    locales && \
    rm -rf /var/lib/apt/lists/*

# Set up locale
RUN sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Create a user for development
RUN useradd -m -s /bin/bash vscode && \
    usermod -aG sudo vscode && \
    echo "vscode ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Switch to vscode user for Guix installation
USER vscode
WORKDIR /home/vscode

# Install Guix using the official installation script
RUN wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh && \
    chmod +x guix-install.sh && \
    sudo ./guix-install.sh --no-daemon && \
    rm guix-install.sh

# Set up Guix environment
ENV PATH="/home/vscode/.config/guix/current/bin:${PATH}"
ENV GUIX_LOCPATH="/home/vscode/.guix-profile/lib/locale"

# Initialize Guix and install shepherd and guile
RUN . /home/vscode/.config/guix/current/etc/profile && \
    guix pull && \
    guix install shepherd guile glibc-locales

# Set up environment for shepherd
RUN mkdir -p /home/vscode/.config/shepherd

# Pre-install some common development tools via Guix
RUN . /home/vscode/.config/guix/current/etc/profile && \
    guix install gcc-toolchain cmake pkg-config boost git

# Set the default working directory
WORKDIR /workspace

# Set up the environment properly
COPY --chown=vscode:vscode .devcontainer/setup-environment.sh /home/vscode/setup-environment.sh
RUN chmod +x /home/vscode/setup-environment.sh

# Default command
CMD ["/bin/bash"]