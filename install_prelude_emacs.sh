curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

cp prelude_configs/init.el ~/.emacs.d/init.el
cp -rf prelude_configs/* ~/.emacs.d/personal/
rm -rf ~/.emacs.d/personal/init.el

# rm -rf ~/self/org
# mkdir -p ~/self
# git clone git@github.com:xingangshi/myorgmode.git ~/self/org
