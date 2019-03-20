#build
mkdir target
elm make src/Main.elm --output=target/index.html

#deploy
aws s3 sync target s3://static-sites.rachelbarrett.co.uk/nim-elm

#cleanup
rm -r target