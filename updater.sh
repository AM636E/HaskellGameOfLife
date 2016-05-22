while inotifywait -e modify src/GameOfLife/Ui/Gui.hs; do
    stack install && ((GameOfLife-exe 1) &)
done
