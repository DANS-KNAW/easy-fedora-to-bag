easy-fedora-to-bag
==================
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-fedora-to-bag.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-fedora-to-bag)

Retrieves a dataset from Fedora and transforms it to an AIP bag conforming to DANS-BagIt-Profile v0

SYNOPSIS
--------

    easy-fedora-to-bag {-d <dataset-id> | -i <dataset-ids-file>} [--skip-list <skip-dataset-ids-file>] -o <output-dir> [-s] [-l <log-file>] [-e | -p] -f { AIP | SIP } <transformation>

DESCRIPTION
-----------
Tool for exporting datasets from Fedora and constructing Archival/Submission Information Packages.
An AIP is a [DANS-V0 bag], a SIP is a directory with a bag and a `deposit.properties` file.

[DANS-V0 bag]: https://github.com/DANS-KNAW/dans-bagit-profile/blob/master/docs/versions/0.0.0.md#dans-bagit-profile-v0

ARGUMENTS
---------

     -c, --cutoff  <arg>          No payload files will be exported, when the dataset has more files than the
                                  specified value. (default = 2147483647)
     -d, --datasetId  <arg>       A single easy-dataset-id to be transformed. Use either this or the input-file
                                  argument
     -e, --europeana              If provided, only the largest pdf/image will selected as payload.
     -i, --input-file  <arg>      File containing a newline-separated list of easy-dataset-ids to be transformed.
                                  Use either this or the dataset-id argument. Lines starting with '#' are
                                  ignored.
     -l, --log-file  <arg>        The name of the logfile in csv format. If not provided a file
                                  easy-fedora-to-bag-<timestamp>.csv will be created in the home-dir of the user.
                                  (default = /home/vagrant/easy-fedora-to-bag-2020-02-02T20:20:02.000Z.csv)
     -p, --no-payload             If provided, no payload files will be exported, i.e. only the metadata is
                                  present in the bag.
     -o, --output-dir  <arg>      Empty directory that will be created if it doesn't exist. Successful bags (or
                                  packages) will be moved to this directory.
     -f, --output-format  <arg>   Output format: AIP, SIP. 'SIP' is only implemented for simple, it creates the
                                  bags one directory level deeper. easy-bag-to-deposit completes these sips with
                                  deposit.properties
         --skip-list  <arg>       File containing a newline-separated list of easy-dataset-ids to be skipped
     -s, --strict                 If provided, the transformation will check whether the datasets adhere to the
                                  requirements of the chosen transformation.
     -w, --with-AV                accessible_to value is not changed when visible_to is NONE
     -h, --help                   Show help message
     -v, --version                Show version of this program
    
    trailing arguments:
     transformation (required)   The type of transformation used: simple, thematische-collectie,
                                 original-versioned.

EXAMPLES
--------

    $ easy-fedora-to-bag -d easy-dataset:1001 -o ~/stagedAIPs -f AIP simple
        creates a directory in '~/stagedAIPs'. This directory is an AIP bag, it has the UUID as the directory name, 
        and contains all relevant information from 'easy-dataset:1001' using the 'simple' transformation.
    
    $ easy-fedora-to-bag -d easy-dataset:1001 -s -o ~/stagedAIPs -f AIP simple
        easy-dataset:1001 is transformed according to the simple transformation, 
        but only if it fulfils the requirements. The AIP bag is generated in directory '~/stagedAIPs'.
    
    $ easy-fedora-to-bag -s -i dataset_ids.txt -o ./stagedAIPs -l ./outputLogfile.csv -f AIP simple
        Creates a bag in './stagedAIPs' for each dataset in 'dataset_ids.txt' using the 'simple' transformation.
        If a dataset does not adhere to the 'simple' requirements, or is not deposited by 'testDepositor',
        it will not be considered and an explanation will be recorded in 'outputLogfile.csv'. 

    $ easy-fedora-to-bag -i dataset_ids.txt -f SIP -o ./stagedSIPs -e simple
        Creates bags for all dataset-ids in dataset_ids.txt using the 'simple' transformation.
        The payload consists of only one file, the largest PDF or image in the datasets.

    $ easy-fedora-to-bag -i dataset_ids.txt --skip-list skip_dataset_ids.txt -f SIP -o ./stagedSIPs simple
        Creates bags for all dataset-ids in dataset_ids.txt, but skipping the datasets in skip_dataset_ids.txt.

    $ easy-fedora-to-bag -i dataset_ids.txt fedora-versioned
        Dry run, each line in the log-file will contain one or more dataset IDs.
        The first dataset on a line is the first version for the rest of the datasets on the same line.

    $ easy-fedora-to-bag -i dataset_ids.txt -f SIP -o ./stagedSIPs fedora-versioned
        Takes the output of a dry run to create simple bags. 


RESULTING FILES
---------------

A `<log-file>` is generated in csv format.
* When no output-dir is specified each line will contain one or more dataset IDs.
  The IDs on a single line are versions of one dataset.
* When an output-dir is specified the file will have the following headers:

    easy-dataset-id  input easy-dataset-id
    UUID             UUID created for the resulting package of the specified output format
    doi              doi as it appears in the EMD
    depositor        EASY-User-Account of the depositor of the dataset
    transformation   transformation used
    comments         if the dataset does not conform to the transformation-requirements, it is chronicled here

For every dataset in the input a bag-directory is created (or more in case of a versioned transformation):
* in case of AIP: `<output-dir>/<bag-UUID>`
* in case of SIP: `<output-dir>/<package-UUID>/<bag-UUID>`

In case of problems (possibly incomplete) bags or packages may be left in the directory
defined with `staging.dir` in the `application properties`.

The bag-directories contain the transformed metadata and data in a DANS-Bagit-Profile.
The `bag-info.txt` file will contain at least:

    EASY-User-Account: ...
    Created: ...
    Payload-Oxum: ...
    Bagging-Date: ...
    Bag-Size: ...


TRANSFORMATIONS
---------------
### SIMPLE
A simple transformation transforms the dataset, with no consideration of other datasets, 'thematische collecties' or external storage.  
With the option `--strict` the transformation will check that the input dataset conforms to the following requirements. The dataset

* has a DANS-DOI
* is PUBLISHED
* is REQUEST\_PERMISSION or OPEN\_ACCESS
* has no Jumpoff page (i.e. there is no dans-jumpoff object in Fedora with a isJumpoffPageFor relation to this dataset)
* has no `replaces` or `isVersionOf` relation that references a DANS-DOI, DANS-URN or easy-dataset-id
* is no `thematische collectie` (i.e. the title does not contain `thematische collectie`)
* is not in the vault already (i.e. check in `easy-bag-index`)

### original-versioned
An original-versioned transformation transforms the dataset into two bags, 
if there exists an `original` folder and at least 1 file outside this folder[(*)](https://github.com/DANS-KNAW/easy-fedora-to-bag/blob/94951d6d74dc1be590d959b53f03e1311ff7baf7/src/main/scala/nl/knaw/dans/easy/fedoratobag/filter/package.scala#L25). 
The first bag will contain the content of the original folder. 
The second bag will contain the accessible files from the original folder, and all remaining files.  

Output for a dataset that meets the conditions to produce two bags:

    <output-dir>/<package-1-UUID>/<bag-1-UUID>/bag-info-txt
    <output-dir>/<package-2-UUID>/<bag-2-UUID>/bag-info-txt

The `bag-info.txt` of the second bag will have additional content to refer to the first bag, an example:

    Is-Version-Of: urn:uuid:<package-1-UUID>
    Base-DOI: 10.17026/test-Iiib-z9p-4ywa
    Base-URN: urn:nbn:nl:ui:13-00-1haq

### fedora-versioned
A fedora-versioned transformation takes several dataset-ids that are meant to be versions of each other, and creates a bag-sequence in the given order.
Each line of the `input-file` should contain a list of dataset-ids, in the correct order, first version first.

Omitting the option `output-dir` implies a dry run.
In that case the CSV file will have one bag-sequence per line.
A sequence may contain datasets not in the input if referenced by datasets in the input.
Datasets not in the input that reference a sequence but are not referenced by a sequence won't appear.

Except for the first dataset of each input line,
`bag-info.txt` will have additional content (like for original-versioned)
to refer to the package of the first dataset on the same input line.

INSTALLATION AND CONFIGURATION
------------------------------
Currently this project is build only as an RPM package for RHEL7/CentOS7 and later. The RPM will install the binaries to
`/opt/dans.knaw.nl/easy-fedora-to-bag` and the configuration files to `/etc/opt/dans.knaw.nl/easy-fedora-to-bag`.

BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher
* RPM

Steps:

        git clone https://github.com/DANS-KNAW/easy-fedora-to-bag.git
        cd easy-fedora-to-bag
        mvn clean install

If the `rpm` executable is found at `/usr/local/bin/rpm`, the build profile that includes the RPM
packaging will be activated. If `rpm` is available, but at a different path, then activate it by using
Maven's `-P` switch: `mvn -Pprm install`.
