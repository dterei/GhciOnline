install:
	cp dist/build/GHCiOnline/GHCiOnline /opt/ghc-online/ghci-online
	cp -r static/ /opt/ghc-online/
	cp prod.env /etc/conf.d/ghci-online
	cp systemd/*.service /etc/systemd/system/

