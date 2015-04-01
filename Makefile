install:
	cp dist/build/GHCiOnline/GHCiOnline /var/ghcio/ghcio
	cp -r static/ /var/ghcio/
	cp prod.env /etc/conf.d/ghcio
	cp systemd/*.service /etc/systemd/system/

