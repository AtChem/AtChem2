FROM rockylinux:8.9

COPY docker/install.sh .
COPY docker/entrypoint.sh .

RUN /install.sh

LABEL org.opencontainers.image.source=https://github.com/wacl-york/AtChem2

ENTRYPOINT [ "/entrypoint.sh" ]
#ENTRYPOINT [ "/bin/bash" ]
