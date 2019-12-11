FROM debian:9

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configuring Elm version
ARG ELM_VERSION=0.19.1

# Configure User
ARG USERNAME=aszenz
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Configure apt and install packages
RUN apt-get update \
  && apt-get -y install --no-install-recommends apt-utils 2>&1 \ 
  #
  # Verify git and needed tools are installed
  && apt-get install -y git procps \
  && apt-get install -y wget curl \
  #  install zsh 
  && apt-get install -y fonts-powerline zsh \
  # install oh-my-zsh and fzf
  && sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)" \
  && git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf \
  && ~/.fzf/install \
  # Install oh-my-zsh plugins
  && git clone https://github.com/zsh-users/zsh-syntax-highlighting ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting \
  && git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions \
  # Install zsh spaceship theme
  && git clone https://github.com/denysdovhan/spaceship-prompt.git ~/.oh-my-zsh/custom/themes/spaceship-prompt \
  && ln -s ~/.oh-my-zsh/custom/themes/spaceship-prompt/spaceship.zsh-theme ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme \
  # import settings
  && wget -O - https://raw.githubusercontent.com/aszenz/linux-settings/master/.zshrc > ~/.zshrc \
  && wget -O - https://raw.githubusercontent.com/aszenz/linux-settings/master/.vimrc> ~/.vimrc \
  && wget -O - https://raw.githubusercontent.com/aszenz/linux-settings/master/.gitconfig> ~/.gitconfig \
  # Install elm globally
  && wget -O - https://github.com/elm/compiler/releases/download/${ELM_VERSION}/binary-for-linux-64-bit.gz | gunzip -c > /usr/local/bin/elm && chmod +x /usr/local/bin/elm \
  # Install node for elm test, elm format and elm repl
  && curl -sL https://deb.nodesource.com/setup_13.x | bash - \
  && apt-get install -y nodejs \
  # install elm related tooling
  && npm install -g elm-format elm-test elm-analyse --unsafe-perm=true \
  # Create a non root user
  && groupadd --gid $USER_GID $USERNAME \
  && useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME \
  # Add sudo support for the non-root user
  && apt-get install -y sudo \
  && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME\
  && chmod 0440 /etc/sudoers.d/$USERNAME \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialogdialog
