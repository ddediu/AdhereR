from setuptools import setup

setup(name='adherer',
      version='0.1',
      description='Allows calling AdhereR from Python',
      long_description='Allos the R package AdhereR to be transparentlly called from Python.',
      classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
        'Operating System :: MacOS',
        'Operating System :: Microsoft :: Windows',
        'Operating System :: POSIX :: Linux',
        'Programming Language :: Python :: 3',
        'Topic :: Scientific/Engineering',
      ],
      url='https://github.com/ddediu/AdhereR',
      author='Dan Dediu',
      author_email='ddediu@gmail.com',
      license='GPLv3',
      packages=['adherer'],
      install_requires=['warnings','subprocess','os','numbers','datetime','pandas','pillow','tempfile','atexit','platform','shutil'],
      zip_safe=False)
