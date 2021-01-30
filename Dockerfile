FROM haskell:8.8.4
RUN mkdir /opt/build
COPY . /opt/build
WORKDIR /opt/build
RUN stack build
EXPOSE 8080
CMD ["stack","exec","swedishverbs-haskell-exe"]