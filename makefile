clean:
	rm -rf *.egg-info

deploy:
	python setup.py sdist
	python setup.py sdist upload
	make clean
