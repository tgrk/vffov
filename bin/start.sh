erl -pa ebin/ deps/*/ebin test/ \
    -boot start_sasl \
    +K true \
    -noshell \
    -detached \
    -s vffov
