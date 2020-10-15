<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:dcterms="http://purl.org/dc/terms/" xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/"
    xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/" xmlns:abr="abr.lookup"
    xmlns:foaf="http://xmlns.com/foaf/0.1/"
    xmlns:acdm="http://registry.ariadne-infrastructure.eu/"
    exclude-result-prefixes="xs abr eas emd" version="2.0">
    <!-- ==================================================== -->
    <!-- metadata for ARIADNE Catalogue Data Model (ACDM) -->
    <!-- converting from internal archival metadata: Easy Metadata (EMD) -->
    <!-- ==================================================== -->

    <xsl:output encoding="UTF-8" indent="yes" method="xml"
        omit-xml-declaration="yes" />

    <xsl:template match="/">
        <xsl:call-template name="acdm-root" />
    </xsl:template>

    <xsl:template name="acdm-root">
        <xsl:apply-templates select="emd:easymetadata" />
    </xsl:template>

    <xsl:template match="emd:easymetadata">
        <acdm:ariadne xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xmlns:acdm="http://registry.ariadne-infrastructure.eu/"
            xmlns:dcat="http://www.w3.org/ns/dcat#" xmlns:dcterms="http://purl.org/dc/terms/"
            xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
            xmlns:skos="http://www.w3.org/2004/02/skos/core#"
            xmlns:foaf="http://xmlns.com/foaf/0.1/"
            xmlns:vcard="http://www.w3.org/2006/vcard/ns#"
            xsi:schemaLocation="http://registry.ariadne-infrastructure.eu/ http://registry.ariadne-infrastructure.eu/schema_definition/6.8/acdm.xsd">
            <acdm:ariadneArchaeologicalResource>
            <!-- Note: could use this to change the resource -->
            <xsl:variable name="resourceTypeName">
                <xsl:choose>
                    <xsl:when test="@eas:scheme='DCMI' and text()='Text'">
                        <xsl:text>textualDocument</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text>collection</xsl:text>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <!-- however a textualDocument is to different from a collection !!! -->
            <xsl:element name="acdm:collection">
                <!-- ISPARTOF -->
                <!-- Note that all easy archaeology is easy-collection:4
                and then we have two sub-collections: reports and excavation-archives -->
                <xsl:element name="dcterms:isPartOf">
                    <xsl:choose>
                        <xsl:when test="@eas:scheme='DCMI' and text()='Text'">
                            <xsl:element name="acdm:archaeologicalResourceType">
                                <xsl:text>reports</xsl:text>
                            </xsl:element>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>excavation-archives</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:element>

                <!-- PUBLISHER -->
                <!--
                <xsl:for-each select="emd:publisher/dcterms:publisher">
                    <xsl:element name="acdm:publisher">
                        <xsl:call-template name="organization-agent">
                            <xsl:with-param name="name" select="text()" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:for-each>
                <xsl:for-each select="emd:publisher/dc:publisher">
                    <xsl:element name="acdm:publisher">
                        <xsl:call-template name="organization-agent">
                            <xsl:with-param name="name" select="text()" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:for-each>
                <xsl:if test="not(emd:publisher/dcterms:publisher) and not(emd:publisher/dc:publisher)">
                    <xsl:element name="acdm:publisher">
                        <xsl:call-template name="organization-agent">
                            <xsl:with-param name="name" select="'Not available'" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:if>
                -->
                <!-- Make DANS the publisher, Ariadne portal interprets it as the 'archive publishing the data' -->
                <xsl:element name="acdm:publisher">
                    <xsl:element name="foaf:name"><xsl:text>Data Archiving and Networked Services (DANS)</xsl:text></xsl:element>
                    <xsl:element name="acdm:typeOfAnAgent"><xsl:text>Organization</xsl:text></xsl:element>
                    <xsl:element name="foaf:mbox"><xsl:text>info@dans.knaw.nl</xsl:text></xsl:element>
                </xsl:element>

                <!-- CONTRIBUTER -->
                <!--
                <xsl:for-each select="emd:contributor/dc:contributor">
                    <xsl:element name="acdm:contributor">
                        <xsl:call-template name="person-agent">
                            <xsl:with-param name="name" select="text()" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:for-each>
                <xsl:if test="not(emd:contributor/dc:contributor)">
                    <xsl:element name="acdm:contributor">
                        <xsl:call-template name="person-agent">
                            <xsl:with-param name="name" select="'Not available'" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:if>
                 -->
                <!-- same as for creator -->
                <xsl:for-each select="emd:contributor/dc:contributor">
                    <xsl:element name="acdm:contributor">
                        <xsl:call-template name="person-agent">
                            <xsl:with-param name="name" select="text()" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:for-each>
                <xsl:for-each select="emd:contributor/eas:contributor">
                    <xsl:element name="acdm:contributor">
                        <xsl:element name="foaf:name">
                            <xsl:call-template name="author-to-string" />
                        </xsl:element>
                        <xsl:element name="acdm:typeOfAnAgent">
                            <xsl:choose>
                                <xsl:when test="eas:surname and eas:surname != ''">
                                    <xsl:text>Person</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>Organization</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:element>
                        <xsl:element name="foaf:mbox">
                            <xsl:text>Not available</xsl:text>
                        </xsl:element>
                    </xsl:element>
                </xsl:for-each>
                <xsl:if test="not(emd:contributor/dc:contributor) and not(emd:contributor/eas:contributor) ">
                    <xsl:element name="acdm:contributor">
                        <xsl:call-template name="person-agent">
                            <xsl:with-param name="name" select="'Not available'" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:if>

                <!-- CREATOR -->
                <xsl:for-each select="emd:creator/dc:creator">
                    <xsl:element name="acdm:creator">
                        <xsl:call-template name="person-agent">
                            <xsl:with-param name="name" select="text()" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:for-each>
                <xsl:for-each select="emd:creator/eas:creator">
                    <xsl:element name="acdm:creator">
                        <xsl:element name="foaf:name">
                            <xsl:call-template name="author-to-string" />
                        </xsl:element>
                        <xsl:element name="acdm:typeOfAnAgent">
                            <xsl:choose>
                                <xsl:when test="eas:surname and eas:surname != ''">
                                    <xsl:text>Person</xsl:text>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:text>Organization</xsl:text>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:element>
                        <xsl:element name="foaf:mbox">
                            <xsl:text>Not available</xsl:text>
                        </xsl:element>
                    </xsl:element>
                </xsl:for-each>
                <xsl:if test="not(emd:creator/dc:creator) and not(emd:creator/eas:creator) ">
                    <xsl:element name="acdm:creator">
                        <xsl:call-template name="person-agent">
                            <xsl:with-param name="name" select="'Not available'" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:if>

                <!-- OWNER -->
                <!-- the rightsHolder, but we don’t always have it and mostly it’s the
                same as the publisher -->
                <xsl:for-each select="emd:rights/dcterms:rightsHolder">
                    <xsl:element name="acdm:owner">
                        <xsl:call-template name="organization-agent">
                            <xsl:with-param name="name" select="text()" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:for-each>
                <xsl:if test="not(emd:rights/dcterms:rightsHolder)">
                    <xsl:element name="acdm:owner">
                        <xsl:call-template name="organization-agent">
                            <xsl:with-param name="name" select="'Not available'" />
                        </xsl:call-template>
                    </xsl:element>
                </xsl:if>

                <!--  TECHNICAL RESPONSIBLE, we don't have it! -->
                <xsl:element name="acdm:technicalResponsible">
                     <xsl:call-template name="person-agent">
                           <xsl:with-param name="name" select="'Not available'" />
                     </xsl:call-template>
                </xsl:element>

                <!-- SUBJECT -->
                <!-- make validation work and specify archaeology as subject -->
                <xsl:element name="acdm:ariadneSubject">
                    <xsl:comment>Should be replaced by a mapping from the nativeSubject</xsl:comment>
                    <xsl:element name="acdm:provided_Subject">
                        <xsl:element name="skos:prefLabel">archaeology</xsl:element>
                        <xsl:element name="dc:source"></xsl:element>
                        <xsl:element name="acdm:published"></xsl:element>
                        <xsl:element name="dc:language">en</xsl:element>
                        <xsl:element name="acdm:provided">True</xsl:element><!-- Yes, I kid you not -->
                    </xsl:element>
                </xsl:element>
                <!-- nativeSubject will be mapped to a skos concept (AAT) by the consumer of this -->
                <xsl:for-each select="emd:subject/dc:subject">
                    <xsl:choose>
                        <xsl:when
                            test="@eas:scheme='ABR' and @eas:schemeId='archaeology.dc.subject'">
                            <!-- The vocabulary code into ABR-complex -->
                            <!-- use uri, reference to vocabulary in SKOS -->
                            <xsl:element name="acdm:nativeSubject">
                                <xsl:element name="skos:Concept">
                                    <xsl:attribute name="rdf:about">
                                        <xsl:call-template
                                            name="abr_complexUri">
                                            <xsl:with-param name="code"
                                                select="text()" />
                                        </xsl:call-template>
                                    </xsl:attribute>
                                    <xsl:element name="skos:prefLabel">
                                        <!-- Also use human readable text -->
                                        <xsl:call-template name="abr_complexLabel">
                                            <xsl:with-param name="code" select="text()" />
                                        </xsl:call-template>
                                    </xsl:element>
                                </xsl:element>
                            </xsl:element>
                            <!-- maybe also put readable text (of siblings) in keywords? -->
                        </xsl:when>
                        <xsl:otherwise>
                            <!-- not in vocabulary, these go into the keywords -->
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each>

            <!-- TITLE -->
            <xsl:for-each select="emd:title/dc:title">
                <xsl:element name="dcterms:title">
                    <xsl:value-of select="text()" />
                </xsl:element>
            </xsl:for-each>

            <!-- DESCRIPTION -->
            <!-- just one, so put it all in there -->
            <xsl:element name="dc:description">
                <!-- handle alternative titles, should be short and descriptive -->
                <xsl:for-each select="emd:title/dcterms:alternative">
                    <!-- if dcterms:alternative can't be used than we map to title, but
                        it could also be appended to the title, which potentially makes long titles. -->
                    <xsl:value-of select="text()" />
                    <xsl:text>&#x0A;</xsl:text>
                </xsl:for-each>
                <xsl:for-each select="emd:description/dc:description">
                    <xsl:value-of select="text()" />
                    <xsl:text>&#x0A;</xsl:text>
                </xsl:for-each>
                <xsl:for-each select="emd:description/dcterms:abstract">
                    <xsl:value-of select="text()" />
                    <xsl:text>&#x0A;</xsl:text>
                </xsl:for-each>
                <xsl:for-each select="emd:description/dcterms:tableOfContents">
                    <xsl:value-of select="text()" />
                    <xsl:text>&#x0A;</xsl:text>
                </xsl:for-each>
            </xsl:element>

            <!-- DATE ISSUED -->
            <!-- use created data here instead of issued! -->
            <xsl:for-each select="emd:date/*:created">
                <!-- there is on ea nd only one -->
                <xsl:element name="dcterms:issued">
                    <xsl:call-template name="date-to-string" />
                </xsl:element>
            </xsl:for-each>

            <!-- DATE MODIFIED -->
            <!-- was previously extracted from emd:date/eas:dateSubmitted -->
                <xsl:for-each select="emd:date/*:created">
                <!-- there is on ea nd only one -->
                <xsl:element name="dcterms:modified">
                    <xsl:call-template name="date-to-string" />
                </xsl:element>
            </xsl:for-each>

            <!-- ORIGINAL IDENTIFIER -->
            <xsl:for-each select="emd:identifier/dc:identifier">
                <xsl:choose>
                    <!-- assume one and only one doi -->
                    <xsl:when test="@eas:scheme='DOI' or @eas:scheme='DOI_OTHER_ACCESS'">
                        <!-- the preferred persistent identifier -->
                        <xsl:element name="acdm:originalId">
                            <!-- persistent one is preferred -->
                            <!-- Not valid to the schema
                            <xsl:attribute name="preferred">true</xsl:attribute>
                             -->
                            <xsl:value-of select="text()" />
                        </xsl:element>
                    </xsl:when>
                    <xsl:otherwise>
                        <!-- other (non-preferred and possible non-persistent) identifiers -->
                        <!-- Not valid to the schema IN KEYWORDS
                        <xsl:element name="acdm:originalId">
                            <xsl:attribute name="preferred">false</xsl:attribute>
                            <xsl:value-of select="@eas:scheme" />
                            <xsl:text> </xsl:text>
                            <xsl:value-of select="text()" />
                        </xsl:element>
                        -->
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>

            <!-- KEYWORDS -->
            <!-- possibly map everything left over, but which might
                be important, here -->
                <!-- subjects -->
                <xsl:for-each select="emd:subject/dc:subject">
                    <xsl:choose>
                        <xsl:when
                            test="@eas:scheme='ABR' and @eas:schemeId='archaeology.dc.subject'">
                            <!-- SKIP: in vocabulary, added as subject elsewhere -->
                        </xsl:when>
                        <xsl:otherwise>
                            <!-- not in vocabulary, these can go into the keywords -->
                            <xsl:element name="dcat:keyword">
                                <xsl:value-of select="text()" />
                            </xsl:element>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each>
                <!-- spatial -->
                <xsl:if test="not(emd:coverage/eas:spatial/eas:point/@eas:scheme='RD') and not(emd:coverage/eas:spatial/eas:box/@eas:scheme='RD')">
                    <!-- no coordinates, put text in keywords -->
                    <xsl:for-each select="emd:coverage/dcterms:spatial" >
                        <xsl:element name="dcat:keyword">
                            <xsl:value-of select="text()" />
                        </xsl:element>
                    </xsl:for-each>
                </xsl:if>
                <!-- identifiers -->
                <xsl:for-each select="emd:identifier/dc:identifier">
                        <xsl:if test="not(@eas:scheme='DOI' or @eas:scheme='DOI_OTHER_ACCESS')">
                        <!-- other (non-preferred and possible non-persistent) identifiers -->
                            <xsl:element name="dcat:keyword">
                                <xsl:if test="@eas:scheme">
                                    <!-- prepend with an indication of the system -->
                                    <xsl:value-of select="@eas:scheme" />
                                    <xsl:text> </xsl:text>
                                </xsl:if>
                            <xsl:value-of select="text()" />
                            </xsl:element>
                        </xsl:if>
                </xsl:for-each>
                <!-- not-ABR temporals -->
                <xsl:for-each select="emd:coverage/dcterms:temporal">
                    <xsl:if test="not(@eas:scheme='ABR' and @eas:schemeId='archaeology.dcterms.temporal') and not(document('')/*/abr:periods/abr:period/abr:name = text())">
                        <xsl:element name="dcat:keyword">
                            <xsl:value-of select="text()"/>
                        </xsl:element>
                    </xsl:if>
                </xsl:for-each>

            <!-- LANGUAGE -->
            <xsl:for-each select="emd:language/dc:language">
                <xsl:element name="dc:language">
                    <xsl:call-template name="language-to-string" />
                </xsl:element>
            </xsl:for-each>

            <!-- LANDING PAGE -->
            <!-- note that it contains the DOI (persistent identifier) and is therefore
                the best to use -->
            <xsl:for-each select="emd:identifier/dc:identifier">
                <xsl:if test="@eas:scheme='DOI' or @eas:scheme='DOI_OTHER_ACCESS'">
                    <xsl:element name="dcat:landingPage">
                        <xsl:value-of
                            select="concat(@eas:identification-system, concat('/',text()))" />
                    </xsl:element>
                </xsl:if>
            </xsl:for-each>

            <!-- CONTACTPOINT -->
            <!-- Due to technical problems we leave it out!
            <xsl:element name="dcat:contactPoint">
                <vcard:Organization>
                    <vcard:fn>Data Archiving and Networked Services (DANS)</vcard:fn>
                    <vcard:nickname>DANS</vcard:nickname>
                    <vcard:hasAddress rdf:parseType="Resource">
                        <vcard:hasStreetAddress rdf:parseType="Resource">
                            <vcard:value>PO Box 93067</vcard:value>
                            <rdf:type rdf:resource="http://www.w3.org/2006/vcard/ns#post-office-box" />
                        </vcard:hasStreetAddress>
                        <vcard:locality>Den Haag</vcard:locality>
                        <vcard:postal-code>2509 AB</vcard:postal-code>
                        <vcard:country-name>The Netherlands</vcard:country-name>
                        <vcard:hasEmail rdf:resource="mailto:info@dans.knaw.nl" />
                    </vcard:hasAddress>
                </vcard:Organization>
            </xsl:element>
             -->

            <!-- ACCESSPOLICY -->
            <!-- Same for whole DANS easy archaeology collection, but we want to specify
                this here -->
            <acdm:accessPolicy>
                <xsl:text>http://dans.knaw.nl/en/about/organisation-and-policy/legal-information</xsl:text>
            </acdm:accessPolicy>

            <!-- ACCESSRIGHTS -->
            <xsl:variable name="accessRight" select="emd:rights/dcterms:accessRights"/>
            <xsl:for-each select="$accessRight">
                <xsl:element name="dcterms:accessRights">
                    <xsl:call-template name="access-rights-to-string" />
                </xsl:element>
            </xsl:for-each>

            <!-- RIGHTS -->
            <!-- there is no rightsHolder in ACDM, but we could map our holders in
                here. It would be semantically better to have a dct:rightsHolder in ACDM
                instead of a rc:rights emd has dc:rights and dcterms:rightsHolder and dcterms:license -->
            <!-- it's optional, so we could leave it out for the time being -->
            <!--
            <xsl:for-each select="emd:rights/dc:rights">
                <xsl:element name="dc:rights">
                    <xsl:value-of select="text()" />
                </xsl:element>
            </xsl:for-each>
            -->
            <!-- Same as for whole DANS easy archaeology collection -->
            <xsl:variable name="licenses" select="emd:rights/dcterms:license"/>
            <xsl:variable name="defaultOpenAccessLicenseText" select="'http://creativecommons.org/publicdomain/zero/1.0'"/>
            <xsl:variable name="defaultLicenseText" select="'Metadata (the content of all fields under the &quot;Description&quot; tab in every dataset in EASY, the online archiving system of DANS) is free for use and open access. However the data itself has conditions for use and license agreements.  The user should, particularly upon distribution or disclosure, respect any copyrights and/or database rights on the dataset. This does not apply to data files deposited under the CC Zero Waiver.'"/>
            <xsl:choose>
                <xsl:when test="$licenses">
                    <xsl:for-each select="$licenses">
                            <xsl:choose>
                                <xsl:when test=". = 'accept' and $accessRight = 'OPEN_ACCESS' and count($licenses)=1">
                                    <dc:rights>
                                        <xsl:value-of select="$defaultOpenAccessLicenseText"/>
                                    </dc:rights>
                                </xsl:when>
                                <xsl:when test=". = 'accept' and not($accessRight = 'OPEN_ACCESS')">
                                    <dc:rights>
                                        <xsl:value-of select="$defaultLicenseText"/>
                                    </dc:rights>
                                </xsl:when>
                                <xsl:when test=". = 'accept'"/><!--to prevent it from appearing in the otherwise clause-->
                                <xsl:otherwise>
                                    <dc:rights>
                                        <xsl:value-of select="."/>
                                    </dc:rights>
                                </xsl:otherwise>
                            </xsl:choose>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:element name="dc:rights">
                        <xsl:value-of select="$defaultLicenseText"/>
                    </xsl:element>
                </xsl:otherwise>
            </xsl:choose>

            <!-- AUDIENCE -->
            <!-- Same as for whole DANS easy archaeology collection, but we want to specify
                this here explicitly -->
            <dcterms:audience><xsl:text>Archaeologists</xsl:text></dcterms:audience>

            <!-- archaeologicalResourceType previously ARIADNESUBJECT -->
            <xsl:for-each select="emd:type/dc:type">
                <xsl:choose>
                    <xsl:when test="@eas:scheme='DCMI' and text()='Text'">
                        <xsl:element name="acdm:archaeologicalResourceType">
                            <xsl:text>Event/Intervention resources</xsl:text>
                        </xsl:element>
                    </xsl:when>
                    <xsl:otherwise>
                        <!-- most likely a Dataset, but as long as it is no Text, map to databases -->
                        <xsl:element name="acdm:archaeologicalResourceType">
                            <xsl:text>Fieldwork archives</xsl:text>
                        </xsl:element>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>

            <!-- TEMPORAL -->
            <xsl:for-each select="emd:coverage/dcterms:temporal">
                <xsl:choose>
                    <xsl:when
                        test="@eas:scheme='ABR' and @eas:schemeId='archaeology.dcterms.temporal'">
                        <!-- The code into ABR-periode temporal -->
                        <xsl:element name="acdm:temporal">
                            <xsl:element name="acdm:periodName">
                                <!-- its a concept -->
                                <xsl:element name="skos:Concept">
                                    <xsl:attribute name="rdf:about">
                                        <xsl:call-template name="abr_periodeUri">
                                            <xsl:with-param name="code" select="text()" />
                                        </xsl:call-template>
                                    </xsl:attribute>
                                    <xsl:element name="skos:prefLabel">
                                        <xsl:call-template name="abr_periodeName">
                                            <xsl:with-param name="code" select="text()" />
                                        </xsl:call-template>
                                    </xsl:element>
                                </xsl:element>
                            </xsl:element>
                            <xsl:element name="acdm:from">
                                <xsl:variable name="year">
                                    <xsl:call-template name="abr_periodeFrom">
                                        <xsl:with-param name="code" select="text()" />
                                    </xsl:call-template>
                                </xsl:variable>
                                <xsl:call-template name="year_to_date">
                                    <xsl:with-param name="year" select="$year" />
                                </xsl:call-template>
                                <xsl:text>-01-01</xsl:text><!-- suggesting more accuracy, but without is its not valid -->
                            </xsl:element>
                            <xsl:element name="acdm:until">
                                <xsl:variable name="year">
                                    <xsl:call-template name="abr_periodeTo">
                                        <xsl:with-param name="code" select="text()" />
                                    </xsl:call-template>
                                </xsl:variable>
                                <xsl:call-template name="year_to_date">
                                    <xsl:with-param name="year" select="$year" />
                                </xsl:call-template>
                                <xsl:text>-12-31</xsl:text><!-- suggesting more accuracy, but without is its not valid -->
                            </xsl:element>
                        </xsl:element>
                    </xsl:when>
                    <xsl:when test="document('')/*/abr:periods/abr:period/abr:name = text()">
                        <xsl:element name="acdm:temporal">
                            <xsl:element name="acdm:periodName">
                                <xsl:element name="skos:Concept">
                                    <xsl:attribute name="rdf:about">
                                        <xsl:call-template name="abr_periodeUri_by_name">
                                            <xsl:with-param name="name" select="text()"/>
                                        </xsl:call-template>
                                    </xsl:attribute>
                                    <xsl:element name="skos:prefLabel">
                                        <xsl:call-template name="abr_periodeName_by_name">
                                            <xsl:with-param name="name" select="text()"/>
                                        </xsl:call-template>
                                    </xsl:element>
                                </xsl:element>
                            </xsl:element>
                            <xsl:element name="acdm:from">
                                <xsl:variable name="year">
                                    <xsl:call-template name="abr_periodeFrom_by_name">
                                        <xsl:with-param name="name" select="text()"/>
                                    </xsl:call-template>
                                </xsl:variable>
                                <xsl:call-template name="year_to_date">
                                    <xsl:with-param name="year" select="$year"/>
                                </xsl:call-template>
                                <xsl:text>-01-01</xsl:text><!-- suggesting more accuracy, but without is its not valid -->
                            </xsl:element>
                            <xsl:element name="acdm:until">
                                <xsl:variable name="year">
                                    <xsl:call-template name="abr_periodeTo_by_name">
                                        <xsl:with-param name="name" select="text()" />
                                    </xsl:call-template>
                                </xsl:variable>
                                <xsl:call-template name="year_to_date">
                                    <xsl:with-param name="year" select="$year" />
                                </xsl:call-template>
                                <xsl:text>-12-31</xsl:text><!-- suggesting more accuracy, but without is its not valid -->
                            </xsl:element>
                        </xsl:element>
                    </xsl:when>
                    <xsl:otherwise>
                        <!-- not in vocabulary, these go into the keywords -->
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>

            <!-- SPATIAL -->
            <!-- dcterms:spatial; These have address, placename information, but can't
                assign it -->
             <!-- we mostly have one location, so append all text as a name of the
                location -->
            <!-- construct name -->
            <xsl:variable name="placeName">
                <xsl:choose>
                    <xsl:when test="not(emd:coverage/dcterms:spatial)">
                        <xsl:text>Not available</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="text_concat">
                            <xsl:with-param name="data" select="emd:coverage/dcterms:spatial" />
                            <xsl:with-param name="separator" select="', '" />
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:for-each select="emd:coverage/eas:spatial/eas:point">
                <!-- Be less strick and do not check for @eas:schemeId='archaeology.eas.spatial' -->
                <xsl:if test="@eas:scheme='RD'">
                    <!-- convert to wgs84 -->
                    <xsl:if
                        test="string(number(eas:x)) != 'NaN' and string(number(eas:y)) != 'NaN'">
                        <xsl:variable name="latlon">
                            <xsl:call-template name="rd-to-lat-long">
                                <xsl:with-param name="x" select="eas:x" />
                                <xsl:with-param name="y" select="eas:y" />
                            </xsl:call-template>
                        </xsl:variable>
                        <!-- point -->
                        <xsl:element name="acdm:spatial">
                            <xsl:element name="acdm:placeName">
                                <xsl:value-of select="$placeName" />
                            </xsl:element>
                            <xsl:element name="acdm:coordinateSystem">
                                <xsl:text>http://www.opengis.net/def/crs/EPSG/0/4326</xsl:text>
                            </xsl:element>
                            <xsl:element name="acdm:lat">
                                <xsl:value-of select="substring-before($latlon, ' ')" />
                            </xsl:element>
                            <xsl:element name="acdm:lon">
                                <xsl:value-of select="substring-after($latlon, ' ')" />
                            </xsl:element>
                            <xsl:element name="acdm:country">
                                <xsl:text>The Netherlands</xsl:text><!-- Not always true -->
                            </xsl:element>
                        </xsl:element>
                    </xsl:if>
                </xsl:if>
            </xsl:for-each>
            <xsl:for-each select="emd:coverage/eas:spatial/eas:box">
                <!-- Be less strick and do not check for @eas:schemeId='archaeology.eas.spatial' -->
                <xsl:if test="@eas:scheme='RD'">
                    <!-- convert to wgs84 -->
                    <!-- note that to be correct we should project the four corners and
                        recalculate a bounding box -->
                    <xsl:if
                        test="string(number(eas:north)) != 'NaN' and string(number(eas:east)) != 'NaN' and string(number(eas:south)) != 'NaN' and string(number(eas:west)) != 'NaN'">
                        <xsl:variable name="latlon1">
                            <xsl:call-template name="rd-to-lat-long">
                                <xsl:with-param name="x" select="eas:west" />
                                <xsl:with-param name="y" select="eas:south" />
                            </xsl:call-template>
                        </xsl:variable>
                        <xsl:variable name="latlon2">
                            <xsl:call-template name="rd-to-lat-long">
                                <xsl:with-param name="x" select="eas:east" />
                                <xsl:with-param name="y" select="eas:north" />
                            </xsl:call-template>
                        </xsl:variable>
                        <!-- box -->
                        <xsl:element name="acdm:spatial">
                            <xsl:element name="acdm:placeName">
                                <xsl:value-of select="$placeName" />
                            </xsl:element>
                            <xsl:element name="acdm:coordinateSystem">
                                <xsl:text>http://www.opengis.net/def/crs/EPSG/0/4326</xsl:text>
                            </xsl:element>
                            <xsl:element name="acdm:boundingBoxMinLat">
                                <xsl:value-of select="substring-before($latlon1, ' ')" />
                            </xsl:element>
                            <xsl:element name="acdm:boundingBoxMinLon">
                                <xsl:value-of select="substring-after($latlon1, ' ')" />
                            </xsl:element>
                            <xsl:element name="acdm:boundingBoxMaxLat">
                                <xsl:value-of select="substring-before($latlon2, ' ')" />
                            </xsl:element>
                            <xsl:element name="acdm:boundingBoxMaxLon">
                                <xsl:value-of select="substring-after($latlon2, ' ')" />
                            </xsl:element>
                            <xsl:element name="acdm:country">
                                <xsl:text>The Netherlands</xsl:text><!-- Not always true -->
                            </xsl:element>
                        </xsl:element>
                    </xsl:if>
                </xsl:if>
            </xsl:for-each>
            <xsl:for-each select="emd:coverage/eas:spatial[eas:polygon]">
                <!-- because polygons are not supported, we convert them to a bounding box -->
                <xsl:variable name="north" select="max(eas:polygon/eas:polygon-exterior/eas:polygon-point/eas:x/xs:decimal(.))"/>
                <xsl:variable name="south" select="min(eas:polygon/eas:polygon-exterior/eas:polygon-point/eas:x/xs:decimal(.))"/>
                <xsl:variable name="west" select="min(eas:polygon/eas:polygon-exterior/eas:polygon-point/eas:y/xs:decimal(.))"/>
                <xsl:variable name="east" select="max(eas:polygon/eas:polygon-exterior/eas:polygon-point/eas:y/xs:decimal(.))"/>
                <xsl:if test="eas:polygon[@eas:scheme='RD']">
                    <xsl:variable name="latlon1">
                        <xsl:call-template name="rd-to-lat-long">
                            <xsl:with-param name="x" select="$west"/>
                            <xsl:with-param name="y" select="$south"/>
                        </xsl:call-template>
                    </xsl:variable>
                    <xsl:variable name="latlon2">
                        <xsl:call-template name="rd-to-lat-long">
                            <xsl:with-param name="x" select="$east" />
                            <xsl:with-param name="y" select="$north" />
                        </xsl:call-template>
                    </xsl:variable>
                    <!-- box -->
                    <xsl:element name="acdm:spatial">
                        <xsl:element name="acdm:placeName">
                            <xsl:value-of select="$placeName" />
                        </xsl:element>
                        <xsl:element name="acdm:coordinateSystem">
                            <xsl:text>http://www.opengis.net/def/crs/EPSG/0/4326</xsl:text>
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMinLat">
                            <xsl:value-of select="substring-before($latlon1, ' ')" />
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMinLon">
                            <xsl:value-of select="substring-after($latlon1, ' ')" />
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMaxLat">
                            <xsl:value-of select="substring-before($latlon2, ' ')" />
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMaxLon">
                            <xsl:value-of select="substring-after($latlon2, ' ')" />
                        </xsl:element>
                        <xsl:element name="acdm:country">
                            <xsl:text>The Netherlands</xsl:text><!-- Not always true -->
                        </xsl:element>
                    </xsl:element>
                </xsl:if>
                <xsl:if test="eas:polygon[@eas:scheme='degrees']">
                    <xsl:element name="acdm:spatial">
                        <xsl:element name="acdm:placeName">
                            <xsl:value-of select="$placeName" />
                        </xsl:element>
                        <xsl:element name="acdm:coordinateSystem">
                            <xsl:text>http://www.opengis.net/def/crs/EPSG/0/4326</xsl:text>
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMinLat">
                            <xsl:value-of select="$south" />
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMinLon">
                            <xsl:value-of select="$west" />
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMaxLat">
                            <xsl:value-of select="$north" />
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMaxLon">
                            <xsl:value-of select="$east" />
                        </xsl:element>
                        <xsl:element name="acdm:country">
                            <xsl:text>The Netherlands</xsl:text><!-- Not always true -->
                        </xsl:element>
                    </xsl:element>
                </xsl:if>
            </xsl:for-each>
                <!-- spatial is mandetory, so we add one even if we have none! -->
                <xsl:if test="not(emd:coverage/eas:spatial/eas:point/@eas:scheme='RD') and not(emd:coverage/eas:spatial/eas:box/@eas:scheme='RD') and not(emd:coverage/eas:spatial/eas:polygon)">
                    <xsl:element name="acdm:spatial">
                        <xsl:comment>Not available, but this is the most likely range that covers it</xsl:comment>
                        <xsl:element name="acdm:placeName">
                            <xsl:choose>
                                <xsl:when test="$placeName=''"><xsl:text>Not available</xsl:text></xsl:when>
                                <xsl:otherwise><xsl:value-of select="$placeName" /></xsl:otherwise>
                            </xsl:choose>
                        </xsl:element>
                        <!-- box approximate for The Netherlands, without overseas areas ;-) -->
                        <!--
                        <xsl:element name="acdm:coordinateSystem">
                            <xsl:text>http://www.opengis.net/def/crs/EPSG/0/4326</xsl:text>
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMinLat">
                            <xsl:text>50.56</xsl:text>
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMinLon">
                            <xsl:text>3.60</xsl:text>
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMaxLat">
                            <xsl:text>54.32</xsl:text>
                        </xsl:element>
                        <xsl:element name="acdm:boundingBoxMaxLon">
                            <xsl:text>6.37</xsl:text>
                        </xsl:element>
                         -->
                        <xsl:element name="acdm:country">
                            <xsl:text>The Netherlands</xsl:text><!-- Not always true -->
                        </xsl:element>
                    </xsl:element>
                </xsl:if>

                <!-- DISTRIBUTION -->
                <xsl:element name="acdm:distribution">
                    <xsl:element name="dcterms:title">
                        <xsl:text>DANS Easy Archive</xsl:text>
                    </xsl:element>
                    <xsl:element name="dcterms:issued">
                        <!-- use created data here instead of issued! -->
                        <xsl:for-each select="emd:date/*:created">
                            <!-- there is one and only one -->
                            <xsl:call-template name="date-to-string" />
                        </xsl:for-each>
                    </xsl:element>
                    <xsl:element name="dcterms:modified">
                        <xsl:for-each select="emd:date/eas:dateSubmitted">
                            <!-- there is one and only one -->
                            <xsl:call-template name="date-to-string" />
                        </xsl:for-each>
                    </xsl:element>
                    <xsl:for-each select="emd:identifier/dc:identifier">
                            <xsl:choose>
                                <!-- assume one and only one doi -->
                                <!-- DANS Doi is always redirected to DANS archive, otherwise we don't know -->
                                <xsl:when test="@eas:scheme='DOI'">
                                    <xsl:element name="dcat:accessURL">
                                        <xsl:text>https://doi.org/</xsl:text>
                                        <xsl:value-of select="text()" />
                                    </xsl:element>
                                    <xsl:element name="acdm:publisher">
                                        <xsl:element name="foaf:name"><xsl:text>Data Archiving and Networked Services (DANS)</xsl:text></xsl:element>
                                        <xsl:element name="acdm:typeOfAnAgent"><xsl:text>Organization</xsl:text></xsl:element>
                                        <xsl:element name="foaf:mbox"><xsl:text>info@dans.knaw.nl</xsl:text></xsl:element>
                                    </xsl:element>
                                </xsl:when>
                                <xsl:when test="@eas:scheme='DOI_OTHER_ACCESS'">
                                    <xsl:element name="dcat:accessURL">
                                        <xsl:text>https://doi.org/</xsl:text>
                                        <xsl:value-of select="text()" />
                                    </xsl:element>
                                    <xsl:element name="acdm:publisher">
                                        <xsl:element name="foaf:name"><xsl:text>Not available</xsl:text></xsl:element>
                                        <xsl:element name="acdm:typeOfAnAgent"><xsl:text>Organization</xsl:text></xsl:element>
                                        <xsl:element name="foaf:mbox"><xsl:text>Not available</xsl:text></xsl:element>
                                    </xsl:element>
                                </xsl:when>
                            </xsl:choose>
                    </xsl:for-each>
                </xsl:element>
            </xsl:element>
            </acdm:ariadneArchaeologicalResource>
        </acdm:ariadne>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- convert a year (number) to a valid xs:date yyyy -->
    <!-- =================================================================================== -->
    <xsl:template name='year_to_date'>
        <xsl:param name='year' />
        <xsl:choose>
            <xsl:when test="$year=''">
                <xsl:text>2050</xsl:text><!-- in the future! -->
            </xsl:when>
            <xsl:when test="$year &lt; -9999">
                <!-- don't padd with zeros -->
                <xsl:value-of select="$year" />
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="format-number($year, '0000')" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- construct a person agent -->
    <!-- =================================================================================== -->
    <xsl:template name='person-agent'>
        <xsl:param name='name' />
        <xsl:element name="foaf:name">
            <xsl:value-of select='$name' />
        </xsl:element>
        <xsl:element name="acdm:typeOfAnAgent">
            <xsl:text>Person</xsl:text>
        </xsl:element>
        <xsl:element name="foaf:mbox">
            <xsl:text>Not available</xsl:text>
        </xsl:element>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- construct a organization agent -->
    <!-- =================================================================================== -->
    <xsl:template name='organization-agent'>
        <xsl:param name='name' />
        <xsl:element name="foaf:name">
            <xsl:value-of select='$name' />
        </xsl:element>
        <xsl:element name="acdm:typeOfAnAgent">
            <xsl:text>Organization</xsl:text>
        </xsl:element>
        <xsl:element name="foaf:mbox">
            <xsl:text>Not available</xsl:text>
        </xsl:element>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- concatenate all text nodes into one string using a separator -->
    <!-- =================================================================================== -->
    <xsl:template name='text_concat'>
        <xsl:param name='data' />
        <xsl:param name='separator' />
        <xsl:for-each select='$data/text()'>
            <xsl:value-of select='.' />
            <xsl:if test='position() != last()'>
                <xsl:value-of select='$separator' />
            </xsl:if>
        </xsl:for-each>
    </xsl:template>

    <!-- ==================================================== -->
    <!-- get human readable (display) text for the access rights -->
    <!-- ==================================================== -->
    <xsl:template name="access-rights-to-string">
        <xsl:variable name="access-rights"
            select=".[text()='OPEN_ACCESS' or text()='NO_ACCESS' or text()='REQUEST_PERMISSION' or text()='OPEN_ACCESS_FOR_REGISTERED_USERS' or text()='GROUP_ACCESS']" />
        <xsl:if test="$access-rights">
            <xsl:variable name="access-display-label">
                <xsl:choose>
                    <xsl:when test="$access-rights='OPEN_ACCESS'">
                        <xsl:value-of
                            select="'Everyone'" />
                    </xsl:when>
                    <xsl:when test="$access-rights='OPEN_ACCESS_FOR_REGISTERED_USERS'">
                        <xsl:value-of
                            select="'Registered EASY users'" />
                    </xsl:when>
                    <xsl:when test="$access-rights='GROUP_ACCESS'">
                        <xsl:value-of
                            select="'Registered EASY users, but belonging to the group of professional Dutch archaeologists only'" />
                    </xsl:when>
                    <xsl:when test="$access-rights='REQUEST_PERMISSION'">
                        <xsl:value-of
                            select="'Registered EASY users, but after permission is granted by the depositor'" />
                    </xsl:when>
                    <xsl:when test="$access-rights='NO_ACCESS'">
                        <xsl:value-of
                            select="'Registered EASY users, permission is granted occasionally after special mediation'" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of
                            select="'The data are not available via Easy (they are either accessible in another way or elsewhere)'" />
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <xsl:value-of select="$access-display-label" />
        </xsl:if>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- Author to string -->
    <!-- =================================================================================== -->
    <xsl:template name="author-to-string">
        <!-- displayForm -->
        <xsl:variable name="titles">
            <xsl:if test="eas:title and eas:title != ''">
                <xsl:value-of select="concat(' ', eas:title)" />
            </xsl:if>
        </xsl:variable>
        <xsl:variable name="initials">
            <xsl:if test="eas:initials and eas:initials != ''">
                <xsl:value-of select="concat(' ', eas:initials)" />
            </xsl:if>
        </xsl:variable>
        <xsl:variable name="prefix">
            <xsl:if test="eas:prefix and eas:prefix != ''">
                <xsl:value-of select="concat(' ', eas:prefix)" />
            </xsl:if>
        </xsl:variable>
        <xsl:variable name="organization">
            <xsl:if test="eas:organization and eas:organization != ''">
                <xsl:value-of select="concat(' (', eas:organization, ')')" />
            </xsl:if>
        </xsl:variable>
        <xsl:variable name="dai">
            <xsl:if test="eas:entityId != ''">
                <xsl:choose>
                    <xsl:when test="eas:entityId/@eas:scheme = 'DAI'">
                        <xsl:value-of select="concat(' DAI=info:eu-repo/dai/nl/', eas:entityId)" />
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of
                            select="concat(' ', eas:entityId/@eas:scheme, '=', eas:entityId)" />
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:if>
        </xsl:variable>
        <xsl:variable name="role">
            <xsl:if test="eas:role and eas:role != ''">
                <xsl:value-of select="concat(', ', eas:role)"/>
            </xsl:if>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="eas:surname and eas:surname != ''">
                <xsl:value-of select="concat(eas:surname, ',', $titles, $initials, $prefix, $organization, $dai, $role)" />
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="concat(eas:organization, $role)" />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- date to string -->
    <!-- =================================================================================== -->
    <xsl:template name="date-to-string">
        <xsl:choose>
            <xsl:when test="@eas:format = 'YEAR'">
                <xsl:value-of select="substring(., 1, 4)" />
            </xsl:when>
            <xsl:when test="@eas:format = 'MONTH'">
                <xsl:value-of select="substring(., 1, 7)" />
            </xsl:when>
            <xsl:when test="@eas:format = 'DAY'">
                <xsl:value-of select="substring(., 1, 10)" />
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="." />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- language to string -->
    <!-- =================================================================================== -->
    <xsl:template name="language-to-string">
        <xsl:choose>
            <xsl:when test="@eas:schemeId = 'common.dc.language'">
                <xsl:choose>
                    <xsl:when test=". = 'dut/nld'">
                        <xsl:value-of select="'nl'" />
                    </xsl:when>
                    <xsl:when test=". = 'eng'">
                        <xsl:value-of select="'en'" />
                    </xsl:when>
                    <xsl:when test=". = 'ger/deu'">
                        <xsl:value-of select="'de'" />
                    </xsl:when>
                    <xsl:when test=". = 'fre/fra'">
                        <xsl:value-of select="'fr'" />
                    </xsl:when>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="." />
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- RD x, y to WGS84 latitude longitude. See: http://www.regiolab-delft.nl/?q=node/36 -->
    <!-- =================================================================================== -->
    <xsl:template name="rd-to-lat-long">
        <xsl:param name="x" as="xs:decimal" />
        <xsl:param name="y" as="xs:decimal" />
        <xsl:variable name="p" select="($x - 155000.00) div 100000" />
        <xsl:variable name="q" select="($y - 463000.00) div 100000" />

        <xsl:variable name="df"
            select="(($q*3235.65389)+($p*$p*-32.58297)+($q*$q*-0.24750)+($p*$p*$q*-0.84978)+($q*$q*$q*-0.06550)+($p*$p*$q*$q*-0.01709)+($p*-0.00738)+($p*$p*$p*$p*0.00530)+($p*$p*$q*$q*$q*-0.00039)+($p*$p*$p*$p*$q*0.00033)+($p*$q*-0.00012)) div 3600" />
        <xsl:variable name="dl"
            select="(($p*5260.52916)+($p*$q*105.94684)+($p*$q*$q*2.45656)+($p*$p*$p*-0.81885)+($p*$q*$q*$q*0.05594)+($p*$p*$p*$q*-0.05607)+($q*0.01199)+($p*$p*$p*$q*$q*-0.00256)+($p*$q*$q*$q*$q*0.00128)+($q*$q*0.00022)+($p*$p*-0.00022)+($p*$p*$p*$p*$p*0.00026)) div 3600" />
        <xsl:variable name="lat"
            select="(round((52.15517440+$df)*100000000.00)) div 100000000.00" />
        <xsl:variable name="lon"
            select="(round((5.387206210+$dl)*100000000.00)) div 100000000.00" />
        <xsl:value-of select="concat($lat, ' ', $lon)" />
    </xsl:template>


    <!-- =================================================================================== -->
    <!-- Use lookup for the ABR-periods, could be made into external file -->
    <!-- Note that EASY has 'old' ABR-periods, and not the new ABR+ from the 
        RCE -->
    <!-- =================================================================================== -->
    <abr:periods>
        <!-- Paleolithicum -->
        <abr:period>
            <abr:code>PALEO</abr:code>
            <abr:name>Palaeolithic</abr:name>
            <abr:from></abr:from>
            <abr:to>-8799</abr:to>
            <abr:uri>http://www.rnaproject.org/data/af11135a-93c1-4c50-b825-7c74bf3dfc1b
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>PALEOV</abr:code>
            <abr:name>Palaeolithic early</abr:name>
            <abr:from></abr:from>
            <abr:to>-299999</abr:to>
            <abr:uri>http://www.rnaproject.org/data/a11c6113-69c2-4a5c-8181-fde4f5af7668
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>PALEOM</abr:code>
            <abr:name>Palaeolithic middle</abr:name>
            <abr:from>-299999</abr:from>
            <abr:to>-34999</abr:to>
            <abr:uri>http://www.rnaproject.org/data/78be8d08-68dc-4bdd-a1cd-5651f29d4dfd
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>PALEOL</abr:code>
            <abr:name>Palaeolithic late</abr:name>
            <abr:from>-34999</abr:from>
            <abr:to>-8799</abr:to>
            <abr:uri>http://www.rnaproject.org/data/e89d8e22-0b01-4cb7-abac-c6c440a09727
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>PALEOLA</abr:code>
            <abr:name>Palaeolithic late A</abr:name>
            <abr:from>-34999</abr:from>
            <abr:to>-17999</abr:to>
            <abr:uri>http://www.rnaproject.org/data/bb658d3d-db83-4f06-99be-f86d0fbd006a
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>PALEOLB</abr:code>
            <abr:name>Palaeolithic late B</abr:name>
            <abr:from>-17999</abr:from>
            <abr:to>-8799</abr:to>
            <abr:uri>http://www.rnaproject.org/data/4f02b5f6-4e11-48dd-8408-17b74412e2bb
            </abr:uri>
        </abr:period>
        <!-- Mesolithicum -->
        <abr:period>
            <abr:code>MESO</abr:code>
            <abr:name>Mesolithic</abr:name>
            <abr:from>-8799</abr:from>
            <abr:to>-4899</abr:to>
            <abr:uri>http://www.rnaproject.org/data/3d05a169-e4bc-4275-9081-5d33440db421
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>MESOV</abr:code>
            <abr:name>Mesolithic early</abr:name>
            <abr:from>-8799</abr:from>
            <abr:to>-7099</abr:to>
            <abr:uri>http://www.rnaproject.org/data/8c5b28c3-1ae6-427c-8484-c611f14b9e1b
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>MESOM</abr:code>
            <abr:name>Mesolithic middle</abr:name>
            <abr:from>-7099</abr:from>
            <abr:to>-6449</abr:to>
            <abr:uri>http://www.rnaproject.org/data/6d601c74-d9de-492c-8319-3d338a38f495
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>MESOL</abr:code>
            <abr:name>Mesolithic late</abr:name>
            <abr:from>-6449</abr:from>
            <abr:to>-4899</abr:to>
            <abr:uri>http://www.rnaproject.org/data/a1c50284-256f-48f7-a02f-45c5230a01ec
            </abr:uri>
        </abr:period>
        <!-- Neolithicum -->
        <abr:period>
            <abr:code>NEO</abr:code>
            <abr:name>Neolithic</abr:name>
            <abr:from>-5299</abr:from>
            <abr:to>-2000</abr:to>
            <abr:uri>http://www.rnaproject.org/data/c67d5c0f-6bea-4953-960f-abeb6360ba8d
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOV</abr:code>
            <abr:name>Neolithic early</abr:name>
            <abr:from>-5299</abr:from>
            <abr:to>-4199</abr:to>
            <abr:uri>http://www.rnaproject.org/data/0e9da988-0463-432e-9d46-4e574adf5f28
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOVA</abr:code>
            <abr:name>Neolithic early A</abr:name>
            <abr:from>-5299</abr:from>
            <abr:to>-4899</abr:to>
            <abr:uri>http://www.rnaproject.org/data/e6a11f08-1d64-4b5e-8eae-74a9fe08a30f
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOVB</abr:code>
            <abr:name>Neolithic early B</abr:name>
            <abr:from>-4899</abr:from>
            <abr:to>-4199</abr:to>
            <abr:uri>http://www.rnaproject.org/data/03b1857f-7d26-4bce-b572-63ade1e56137
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOM</abr:code>
            <abr:name>Neolithic middle</abr:name>
            <abr:from>-4199</abr:from>
            <abr:to>-2849</abr:to>
            <abr:uri>http://www.rnaproject.org/data/422b5381-a048-4b3d-b54b-e9041e4c32d0
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOMA</abr:code>
            <abr:name>Neolithic middle A</abr:name>
            <abr:from>-4199</abr:from>
            <abr:to>-3399</abr:to>
            <abr:uri>http://www.rnaproject.org/data/ea7ad5b1-bbf4-410c-9aa4-bca72cd7f13d
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOMB</abr:code>
            <abr:name>Neolithic middle B</abr:name>
            <abr:from>-3399</abr:from>
            <abr:to>-2849</abr:to>
            <abr:uri>http://www.rnaproject.org/data/164803a7-df69-4866-b05b-73bbeafdaa03
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOL</abr:code>
            <abr:name>Neolithic late</abr:name>
            <abr:from>-2849</abr:from>
            <abr:to>-1999</abr:to>
            <abr:uri>http://www.rnaproject.org/data/0840def5-a771-4a0b-b5d6-9f4d119491b7
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOLA</abr:code>
            <abr:name>Neolithic late A</abr:name>
            <abr:from>-2849</abr:from>
            <abr:to>-2449</abr:to>
            <abr:uri>http://www.rnaproject.org/data/5aedc1fb-2d6b-451f-8f1e-07569a25fadf
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NEOLB</abr:code>
            <abr:name>Neolithic late B</abr:name>
            <abr:from>-2449</abr:from>
            <abr:to>-1999</abr:to>
            <abr:uri>http://www.rnaproject.org/data/3d9d4a36-2aa2-4914-b0fd-d06fb955fbd2
            </abr:uri>
        </abr:period>
        <!-- Bronstijd -->
        <abr:period>
            <abr:code>BRONS</abr:code>
            <abr:name>Bronze Age</abr:name>
            <abr:from>-1999</abr:from>
            <abr:to>-799</abr:to>
            <abr:uri>http://www.rnaproject.org/data/8d6c5137-4abf-41ec-a693-a9658acbdbfb
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>BRONSV</abr:code>
            <abr:name>Bronze Age early</abr:name>
            <abr:from>-1999</abr:from>
            <abr:to>-1799</abr:to>
            <abr:uri>http://www.rnaproject.org/data/ff691c2c-59fd-488b-b4fb-03c7a3f3b5e0
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>BRONSM</abr:code>
            <abr:name>Bronze Age middle</abr:name>
            <abr:from>-1799</abr:from>
            <abr:to>-1099</abr:to>
            <abr:uri>http://www.rnaproject.org/data/d0be91f6-d74f-480e-8d0a-7c3dabde9bcc
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>BRONSMA</abr:code>
            <abr:name>Bronze Age middle A</abr:name>
            <abr:from>-1799</abr:from>
            <abr:to>-1499</abr:to>
            <abr:uri>http://www.rnaproject.org/data/df4015d3-31dc-4ad3-a290-8b22ca619f7b
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>BRONSMB</abr:code>
            <abr:name>Bronze Age middle B</abr:name>
            <abr:from>-1499</abr:from>
            <abr:to>-1099</abr:to>
            <abr:uri>http://www.rnaproject.org/data/b58eb145-164b-4433-aa62-9004c6b62bbc
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>BRONSL</abr:code>
            <abr:name>Bronze Age late</abr:name>
            <abr:from>-1099</abr:from>
            <abr:to>-799</abr:to>
            <abr:uri>http://www.rnaproject.org/data/c5abc713-0239-4b33-929c-b3e7c0a49a4a
            </abr:uri>
        </abr:period>
        <!-- IJzertijd -->
        <abr:period>
            <abr:code>IJZ</abr:code>
            <abr:name>Iron Age</abr:name>
            <abr:from>-799</abr:from>
            <abr:to>-11</abr:to>
            <abr:uri>http://www.rnaproject.org/data/b7dbe295-d2ae-4a7e-9511-77c8a35cdb8d
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>IJZV</abr:code>
            <abr:name>Iron Age early</abr:name>
            <abr:from>-799</abr:from>
            <abr:to>-499</abr:to>
            <abr:uri>http://www.rnaproject.org/data/c26887da-702d-4315-a8b5-5ce57071aea5
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>IJZM</abr:code>
            <abr:name>Iron Age middle</abr:name>
            <abr:from>-499</abr:from>
            <abr:to>-249</abr:to>
            <abr:uri>http://www.rnaproject.org/data/93a3e045-b05f-4af7-8b68-c6438f338abe
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>IJZL</abr:code>
            <abr:name>Iron Age late</abr:name>
            <abr:from>-249</abr:from>
            <abr:to>-11</abr:to>
            <abr:uri>http://www.rnaproject.org/data/6d115c5b-642c-4859-8864-fc0256e52283
            </abr:uri>
        </abr:period>
        <!-- Romeinse tijd -->
        <abr:period>
            <abr:code>ROM</abr:code>
            <abr:name>Roman period</abr:name>
            <abr:from>-11</abr:from>
            <abr:to>450</abr:to>
            <abr:uri>http://www.rnaproject.org/data/000c6eeb-83ac-47d5-b18f-c9e5d5f08b69
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMV</abr:code>
            <abr:name>Roman period early</abr:name>
            <abr:from>-11</abr:from>
            <abr:to>70</abr:to>
            <abr:uri>http://www.rnaproject.org/data/fb4eda61-662b-4006-8964-ce3d89785eca
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMVA</abr:code>
            <abr:name>Roman period early A</abr:name>
            <abr:from>-11</abr:from>
            <abr:to>25</abr:to>
            <abr:uri>http://www.rnaproject.org/data/ea52b83f-3656-4283-9600-da689fce2b2e
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMVB</abr:code>
            <abr:name>Roman period early B</abr:name>
            <abr:from>25</abr:from>
            <abr:to>70</abr:to>
            <abr:uri>http://www.rnaproject.org/data/fd9078c9-66cd-44a8-b208-5475ea319b7a
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMM</abr:code>
            <abr:name>Roman period middle</abr:name>
            <abr:from>70</abr:from>
            <abr:to>270</abr:to>
            <abr:uri>http://www.rnaproject.org/data/1b3ca670-f25b-473d-bad4-1d9f0109316c
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMMA</abr:code>
            <abr:name>Roman period middle A</abr:name>
            <abr:from>70</abr:from>
            <abr:to>150</abr:to>
            <abr:uri>http://www.rnaproject.org/data/551c5221-5569-4858-9c24-18ef333c9293
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMMB</abr:code>
            <abr:name>Roman period middle B</abr:name>
            <abr:from>150</abr:from>
            <abr:to>270</abr:to>
            <abr:uri>http://www.rnaproject.org/data/df391e9c-e3e2-413c-85c6-6163021b21bf
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROML</abr:code>
            <abr:name>Roman period late</abr:name>
            <abr:from>270</abr:from>
            <abr:to>450</abr:to>
            <abr:uri>http://www.rnaproject.org/data/cb7b44cb-9a62-4ad1-8348-bbee8662ad9f
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMLA</abr:code>
            <abr:name>Roman period late A</abr:name>
            <abr:from>270</abr:from>
            <abr:to>350</abr:to>
            <abr:uri>http://www.rnaproject.org/data/949bb33b-a4ec-4b9d-ac42-48fec129eacb
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>ROMLB</abr:code>
            <abr:name>Roman period late B</abr:name>
            <abr:from>350</abr:from>
            <abr:to>450</abr:to>
            <abr:uri>http://www.rnaproject.org/data/0bc49749-4280-425a-9db1-95f75d9b05e6
            </abr:uri>
        </abr:period>
        <!-- Middeleeuwen, ABR+ differs -->
        <abr:period>
            <abr:code>XME</abr:code>
            <abr:name>Middle Ages</abr:name>
            <abr:from>450</abr:from>
            <abr:to>1500</abr:to>
            <abr:uri>http://www.rnaproject.org/data/9deee0d5-bf7f-48ab-9d17-c42f30fdfcce
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>VME</abr:code>
            <abr:name>Middle Ages early</abr:name>
            <abr:from>450</abr:from>
            <abr:to>1050</abr:to>
            <abr:uri>http://www.rnaproject.org/data/f55f7f27-2f55-430d-a96a-509d2e24b5e0
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>VMEA</abr:code>
            <abr:name>Middle Ages early A</abr:name>
            <abr:from>450</abr:from>
            <abr:to>525</abr:to>
            <abr:uri>http://www.rnaproject.org/data/19716f87-1a5f-42c3-bc86-e579bce80a7e
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>VMEB</abr:code>
            <abr:name>Middle Ages early B</abr:name>
            <abr:from>525</abr:from>
            <abr:to>725</abr:to>
            <abr:uri>http://www.rnaproject.org/data/c68c1eaa-2f83-416d-b503-d8f622972e1f
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>VMEC</abr:code>
            <abr:name>Middle Ages early C</abr:name>
            <abr:from>725</abr:from>
            <abr:to>900</abr:to>
            <abr:uri>http://www.rnaproject.org/data/38ce0787-6846-48b8-a007-072b4e10044f
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>VMED</abr:code>
            <abr:name>Middle Ages early D</abr:name>
            <abr:from>900</abr:from>
            <abr:to>1050</abr:to>
            <abr:uri>http://www.rnaproject.org/data/66971105-a65a-402b-b202-78e1e7b9193d
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>LME</abr:code>
            <abr:name>Middle Ages late</abr:name>
            <abr:from>1050</abr:from>
            <abr:to>1500</abr:to>
            <abr:uri>http://www.rnaproject.org/data/45246f6c-83a0-4c24-8b5b-3fac06874927
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>LMEA</abr:code>
            <abr:name>Middle Ages late A</abr:name>
            <abr:from>1050</abr:from>
            <abr:to>1250</abr:to>
            <abr:uri>http://www.rnaproject.org/data/ac40aa75-90db-474d-9c69-ff3a944ed3d8
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>LMEB</abr:code>
            <abr:name>Middle Ages late B</abr:name>
            <abr:from>1250</abr:from>
            <abr:to>1500</abr:to>
            <abr:uri>http://www.rnaproject.org/data/7a680b04-d5c2-4b07-8899-c55118b229e3
            </abr:uri>
        </abr:period>
        <!-- Nieuwe tijd, ABR+ differs -->
        <abr:period>
            <abr:code>NT</abr:code>
            <abr:name>Modern period</abr:name>
            <abr:from>1500</abr:from>
            <abr:to></abr:to>
            <abr:uri>http://www.rnaproject.org/data/2184f19a-aea4-46b5-8731-66521b7d7797
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NTA</abr:code>
            <abr:name>Modern period A</abr:name>
            <abr:from>1500</abr:from>
            <abr:to>1650</abr:to>
            <abr:uri>http://www.rnaproject.org/data/3434245f-500f-452a-9289-75c081146308
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NTB</abr:code>
            <abr:name>Modern period B</abr:name>
            <abr:from>1650</abr:from>
            <abr:to>1850</abr:to>
            <abr:uri>http://www.rnaproject.org/data/a38936c4-8fab-4cda-9bff-4f7436324768
            </abr:uri>
        </abr:period>
        <abr:period>
            <abr:code>NTC</abr:code>
            <abr:name>Modern period C</abr:name>
            <abr:from>1850</abr:from>
            <abr:to></abr:to>
            <abr:uri>http://www.rnaproject.org/data/98c0a134-2236-4f19-a39c-60a22d7a6230
            </abr:uri>
        </abr:period>
        <!-- Onbekend -->
        <abr:period>
            <abr:code>XXX</abr:code>
            <abr:name>Unknown</abr:name>
            <abr:from></abr:from>
            <abr:to></abr:to>
            <abr:uri>http://www.rnaproject.org/data/3d7a46aa-9315-4b63-a7f5-f6bbcbe48601
            </abr:uri>
        </abr:period>
    </abr:periods>

    <xsl:key name="period-lookup" match="abr:period" use="abr:code" />
    <xsl:variable name="periods-top" select="document('')/*/abr:periods" />
    <xsl:template name="abr_periodeName">
        <xsl:param name="code" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="key('period-lookup', $code)/abr:name" />
        </xsl:for-each>
    </xsl:template>
    <xsl:template name="abr_periodeFrom">
        <xsl:param name="code" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="key('period-lookup', $code)/abr:from" />
        </xsl:for-each>
    </xsl:template>
    <xsl:template name="abr_periodeTo">
        <xsl:param name="code" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="key('period-lookup', $code)/abr:to" />
        </xsl:for-each>
    </xsl:template>
    <xsl:template name="abr_periodeUri">
        <xsl:param name="code" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="normalize-space(key('period-lookup', $code)/abr:uri)" />
        </xsl:for-each>
    </xsl:template>

    <xsl:key name="period-lookup-by-name" match="abr:period" use="abr:name" />
    <xsl:template name="abr_periodeName_by_name">
        <xsl:param name="name" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="key('period-lookup-by-name', $name)/abr:name" />
        </xsl:for-each>
    </xsl:template>
    <xsl:template name="abr_periodeFrom_by_name">
        <xsl:param name="name" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="key('period-lookup-by-name', $name)/abr:from" />
        </xsl:for-each>
    </xsl:template>
    <xsl:template name="abr_periodeTo_by_name">
        <xsl:param name="name" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="key('period-lookup-by-name', $name)/abr:to" />
        </xsl:for-each>
    </xsl:template>
    <xsl:template name="abr_periodeUri_by_name">
        <xsl:param name="name" />
        <xsl:for-each select="$periods-top"><!-- change context to periods table -->
            <xsl:value-of select="normalize-space(key('period-lookup-by-name', $name)/abr:uri)" />
        </xsl:for-each>
    </xsl:template>

    <!-- =================================================================================== -->
    <!-- Use lookup for the ABR-complex, could be made into external file -->
    <!-- =================================================================================== -->
    <abr:complexlist>
        <abr:complex>
            <abr:code>DEPO</abr:code>
            <abr:label>Depot</abr:label>
            <abr:uri>http://www.rnaproject.org/data/b97ef902-059a-4c14-b925-273c74bace30
            </abr:uri>
        </abr:complex>
        <!-- Economie -->
        <abr:complex>
            <abr:code>EGKW</abr:code>
            <abr:label>Economie - Kleiwinning</abr:label>
            <abr:uri>http://www.rnaproject.org/data/42e305f6-631b-4ad8-8c53-a53005de7529
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EGMW</abr:code>
            <abr:label>Economie - Mergel-/kalkwinning</abr:label>
            <abr:uri>http://www.rnaproject.org/data/35240121-7752-4567-a613-74bca32ff311
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EGVU</abr:code>
            <abr:label>Economie - Vuursteenwinning</abr:label>
            <abr:uri>http://www.rnaproject.org/data/a3a50eea-4b08-4e4a-913d-1f6a56ae755f
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EGVW</abr:code>
            <abr:label>Economie - Veenwinning</abr:label>
            <abr:uri>http://www.rnaproject.org/data/4d8012b9-0f95-43ba-9c36-76d86758a6bf
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EGX</abr:code>
            <abr:label>Economie - Grondstofwinning</abr:label>
            <abr:uri>http://www.rnaproject.org/data/db8e148a-9cf6-4ea8-8ab3-0a9950417dc5
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EGYW</abr:code>
            <abr:label>Economie - IJzerwinning</abr:label>
            <abr:uri>http://www.rnaproject.org/data/1c68a564-e260-42fc-a7b3-8c760685cd13
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EGZW</abr:code>
            <abr:label>Economie - Zoutwinning/moernering</abr:label>
            <abr:uri>http://www.rnaproject.org/data/a1ea369d-dd92-45ab-9727-d79cb80a9160
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIB</abr:code>
            <abr:label>Economie - Brouwerij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/e3dcc4a3-0e2b-43ae-9990-f6629c3b1535
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIBB</abr:code>
            <abr:label>Economie - Beenbewerking</abr:label>
            <abr:uri>http://www.rnaproject.org/data/cdd06a0c-9f49-4bcb-ae0f-89114b486878
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIGB</abr:code>
            <abr:label>Economie - Glasblazerij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f1c534f7-48bf-4a35-9076-adef3287197a
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIHB</abr:code>
            <abr:label>Economie - Houtbewerking</abr:label>
            <abr:uri>http://www.rnaproject.org/data/74e446c9-fbc8-4695-9f34-655f483ae65a
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIHK</abr:code>
            <abr:label>Economie - Houtskool-/kolenbranderij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/2aecb1f3-7f14-4e12-8bfb-d4ba9147851c
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIKB</abr:code>
            <abr:label>Economie - Kalkbranderij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/97d59086-ff33-4e2d-9749-92f3a52f7b47
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EILL</abr:code>
            <abr:label>Economie - Leerlooierij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/44fff26c-c64c-489e-86d8-e89972b8aa40
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIM</abr:code>
            <abr:label>Economie - Molen</abr:label>
            <abr:uri>http://www.rnaproject.org/data/d3241408-54b7-4b21-9f4f-12ad485e3ec0
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIMB</abr:code>
            <abr:label>Economie - Metaalbewerking/smederij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/810768e0-0001-4bcb-91a5-20b25443e61a
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIPB</abr:code>
            <abr:label>Economie - Pottenbakkerij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/de8dc6bc-8f06-453c-ab8d-18586eb0ec5f
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EISB</abr:code>
            <abr:label>Economie - Steen-/pannenbakkerij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/e3fb98d2-9c9a-4c1b-9cae-ccdae7063e5b
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EISM</abr:code>
            <abr:label>Economie - Smelterij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/8dc4a810-8e1c-4e3c-8735-4635528272c2
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EITN</abr:code>
            <abr:label>Economie - Textielnijverheid</abr:label>
            <abr:uri>http://www.rnaproject.org/data/6d83b0c6-d379-4ba8-b822-8a7aa19edb63
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIVB</abr:code>
            <abr:label>Economie - Vuursteenbewerking</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f254bb6d-95ec-4e0d-ad54-ed6d09b5cc92
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EIX</abr:code>
            <abr:label>Economie - Industrie/nijverheid</abr:label>
            <abr:uri>http://www.rnaproject.org/data/dccb4e11-44da-443c-939d-ef149a54d9b7
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ELA</abr:code>
            <abr:label>Economie - Akker/tuin</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f3875055-f139-4835-9f5a-22f9d43e4f9d
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ELCF</abr:code>
            <abr:label>Economie - Celtic field/raatakker</abr:label>
            <abr:uri>http://www.rnaproject.org/data/a44460a9-8252-41a5-9d3e-380c86757c9a
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ELDP</abr:code>
            <abr:label>Economie - Drenkplaats/dobbe</abr:label>
            <abr:uri>http://www.rnaproject.org/data/8e3e1f10-5b97-44fb-8d68-6ec474864c9f
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ELEK</abr:code>
            <abr:label>Economie - Eendekooi</abr:label>
            <abr:uri>http://www.rnaproject.org/data/e4e24f1a-6148-4ad6-935a-b8bfa5d3b468
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ELVK</abr:code>
            <abr:label>Economie - Veekraal/schaapskooi</abr:label>
            <abr:uri>http://www.rnaproject.org/data/e189c2bc-44f2-4af9-8bee-b05eaadcb774
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ELX</abr:code>
            <abr:label>Economie - Landbouw</abr:label>
            <abr:uri>http://www.rnaproject.org/data/033974e0-156d-4f6b-8c6b-65e3b4be5c47
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ESCH</abr:code>
            <abr:label>Economie - Scheepvaart</abr:label>
            <abr:uri>http://www.rnaproject.org/data/bf9b0250-aa7f-4da8-b2c4-a577649d7870
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EVX</abr:code>
            <abr:label>Economie - Visserij</abr:label>
            <abr:uri>http://www.rnaproject.org/data/16c6f0cc-5d41-4fca-96ca-cfac46ca1754
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>EX</abr:code>
            <abr:label>Economie, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/a5b002d1-533c-4322-b176-1816a8a5e042
            </abr:uri>
        </abr:complex>
        <!-- begraving -->
        <abr:complex>
            <abr:code>GC</abr:code>
            <abr:label>Begraving - Crematiegraf</abr:label>
            <abr:uri>http://www.rnaproject.org/data/23f38c08-4c62-40f7-a3d8-085bb5f1ed95
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GCV</abr:code>
            <abr:label>Begraving - Vlakgraf, crematie</abr:label>
            <abr:uri>http://www.rnaproject.org/data/1b496321-17ed-4f52-841a-fa21aac9093f
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GD</abr:code>
            <abr:label>Begraving - Dierengraf</abr:label>
            <abr:uri>http://www.rnaproject.org/data/df2a0682-0392-40ca-a396-4b5edf8d52a8
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GHC</abr:code>
            <abr:label>Begraving - Grafheuvel, crematie</abr:label>
            <abr:uri>http://www.rnaproject.org/data/fbd0bd66-9837-4f6d-a53a-53d08458de11
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GHI</abr:code>
            <abr:label>Begraving - Grafheuvel, inhumatie</abr:label>
            <abr:uri>http://www.rnaproject.org/data/9970aa8b-73ae-493a-a950-895e510fc03b
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GHIC</abr:code>
            <abr:label>Begraving - Grafheuvel, gemengd</abr:label>
            <abr:uri>http://www.rnaproject.org/data/777bb1e2-fe55-4f65-850a-8d4708839074
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GHX</abr:code>
            <abr:label>Begraving - Grafheuvel, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/1b9d280a-7023-4ad9-8281-110b171c1007
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GI</abr:code>
            <abr:label>Begraving - Inhumatiegraf</abr:label>
            <abr:uri>http://www.rnaproject.org/data/876e0fe7-abe9-4022-9626-daa2f7444153
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GIV</abr:code>
            <abr:label>Begraving - Vlakgraf, inhumatie</abr:label>
            <abr:uri>http://www.rnaproject.org/data/44e63215-8ce0-4682-a663-5b4fcbaa51d4
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GMEG</abr:code>
            <abr:label>Begraving - Megalietgraf</abr:label>
            <abr:uri>http://www.rnaproject.org/data/333d2fbc-6817-41e1-b9ce-7e6d756fd53b
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVC</abr:code>
            <abr:label>Begraving - Grafveld, crematies</abr:label>
            <abr:uri>http://www.rnaproject.org/data/e97803f6-8477-493b-ad7a-e8e1865a033d
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVCU</abr:code>
            <abr:label>Begraving - Urnenveld</abr:label>
            <abr:uri>http://www.rnaproject.org/data/7f623fa8-fad1-4178-8e2d-7dbd03eeaad2
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVI</abr:code>
            <abr:label>Begraving - Grafveld, inhumaties</abr:label>
            <abr:uri>http://www.rnaproject.org/data/01f4f3f4-e33a-413a-9734-1e3f411981af
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVIC</abr:code>
            <abr:label>Begraving - Grafveld, gemengd</abr:label>
            <abr:uri>http://www.rnaproject.org/data/2a35dccc-a050-4ec4-9912-3bdb9c63548a
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVIK</abr:code>
            <abr:label>Begraving - Kerkhof</abr:label>
            <abr:uri>http://www.rnaproject.org/data/8dec6ef7-dfad-45a3-8d79-b038b6029371
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVIR</abr:code>
            <abr:label>Begraving - Rijengrafveld</abr:label>
            <abr:uri>http://www.rnaproject.org/data/a9fb73ea-979e-4c50-a5b1-f7106ec1aa73
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GVX</abr:code>
            <abr:label>Begraving - Grafveld, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/4af9d348-ee15-4eec-95c8-150eef6919b8
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GX</abr:code>
            <abr:label>Begraving, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/557ed9d6-ca6d-4dc4-acd3-42f454085722
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>GXV</abr:code>
            <abr:label>Begraving - Vlakgraf, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/8d6aedb6-3806-40f1-a805-faef60157069
            </abr:uri>
        </abr:complex>
        <!-- Infrastructure -->
        <abr:complex>
            <abr:code>IBRU</abr:code>
            <abr:label>Infrastructuur - Brug</abr:label>
            <abr:uri>http://www.rnaproject.org/data/60077521-9d24-4521-97b8-d7d908a1f94d
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IDAM</abr:code>
            <abr:label>Infrastructuur - Dam</abr:label>
            <abr:uri>http://www.rnaproject.org/data/fb17334e-1488-471b-aaff-74cd21740318
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IDIJ</abr:code>
            <abr:label>Infrastructuur - Dijk</abr:label>
            <abr:uri>http://www.rnaproject.org/data/366d9166-992c-412a-ade4-c3e906aa4268
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IDUI</abr:code>
            <abr:label>Infrastructuur - Duiker</abr:label>
            <abr:uri>http://www.rnaproject.org/data/570880fb-b263-480d-956a-ad3836e37566
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IGEM</abr:code>
            <abr:label>Infrastructuur - Gemaal</abr:label>
            <abr:uri>http://www.rnaproject.org/data/05641fb4-94ae-4248-afdf-a284b4f57824
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IHAV</abr:code>
            <abr:label>Infrastructuur - Haven</abr:label>
            <abr:uri>http://www.rnaproject.org/data/86378d1e-d61e-46e2-a98d-706bcfa2a49b
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IKAN</abr:code>
            <abr:label>Infrastructuur - Kanaal/vaarweg</abr:label>
            <abr:uri>http://www.rnaproject.org/data/70686979-86ad-4ce6-acac-ef9fcfe33073
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IPER</abr:code>
            <abr:label>Infrastructuur - Percelering/verkaveling</abr:label>
            <abr:uri>http://www.rnaproject.org/data/bace124f-c466-4953-8d67-1cfaca631aa8
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ISLU</abr:code>
            <abr:label>Infrastructuur - Sluis</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f083fba5-671e-4b76-bb1f-b2fc51623ce3
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>ISTE</abr:code>
            <abr:label>Infrastructuur - Steiger</abr:label>
            <abr:uri>http://www.rnaproject.org/data/3fa0e953-710e-4e1a-847f-cc9b861f736f
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IVW</abr:code>
            <abr:label>Infrastructuur - Veenweg/veenbrug</abr:label>
            <abr:uri>http://www.rnaproject.org/data/959701d7-413c-4581-b5fe-7bcba9dbc967
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IWAT</abr:code>
            <abr:label>Infrastructuur - Waterweg (natuurlijk)</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f55d1bd5-8951-497a-aa8c-2dff13aa5ec2
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IWEG</abr:code>
            <abr:label>Infrastructuur - Weg</abr:label>
            <abr:uri>http://www.rnaproject.org/data/b24007fd-4888-4a47-a6e9-7313c3f376ba
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>IX</abr:code>
            <abr:label>Infrastructuur, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f53cb4cc-f7e4-47ea-be30-9d0acc14542a
            </abr:uri>
        </abr:complex>
        <!-- Nederzetting -->
        <abr:complex>
            <abr:code>NBAS</abr:code>
            <abr:label>Nederzetting - Basiskamp/basisnederzetting</abr:label>
            <abr:uri>http://www.rnaproject.org/data/7ab6ce7a-8f67-443f-9c90-b163d81d5cbc
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NEXT</abr:code>
            <abr:label>Nederzetting - Extractiekamp/-nederzetting</abr:label>
            <abr:uri>http://www.rnaproject.org/data/38431ccd-cedd-46f3-ba08-dae230e7344e
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NHP</abr:code>
            <abr:label>Nederzetting - Huisplaats, onverhoogd</abr:label>
            <abr:uri>http://www.rnaproject.org/data/a94a7fff-8d3d-495f-9b56-713179ff6a4a
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NHT</abr:code>
            <abr:label>Nederzetting - Huisterp</abr:label>
            <abr:uri>http://www.rnaproject.org/data/64fea0bf-f62f-4093-a327-5c10191472c4
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NKD</abr:code>
            <abr:label>Nederzetting - Kampdorp</abr:label>
            <abr:uri>http://www.rnaproject.org/data/13f4c9a5-96dd-43b2-a06b-efa2d49fdc87
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NMS</abr:code>
            <abr:label>Nederzetting - Moated site</abr:label>
            <abr:uri>http://www.rnaproject.org/data/b13ae2fb-0e25-4eb1-bb9f-b25b73cb3938
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NRV</abr:code>
            <abr:label>Nederzetting - Romeins villa(complex)</abr:label>
            <abr:uri>http://www.rnaproject.org/data/1ff31102-f8af-480b-9b10-630ed6bfaccb
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NS</abr:code>
            <abr:label>Nederzetting - Stad</abr:label>
            <abr:uri>http://www.rnaproject.org/data/ac6a2702-d360-439c-b278-ca067390502c
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NT</abr:code>
            <abr:label>Nederzetting - Terp/wierde</abr:label>
            <abr:uri>http://www.rnaproject.org/data/70707398-14d6-477a-acdb-bdca1d56d426
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NVB</abr:code>
            <abr:label>Nederzetting - Borg/stins/versterkt huis</abr:label>
            <abr:uri>http://www.rnaproject.org/data/058a5abe-f6de-4c3a-a05e-8f9b2e997b3c
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NVH</abr:code>
            <abr:label>Nederzetting - Havezathe/ridderhofstad</abr:label>
            <abr:uri>http://www.rnaproject.org/data/4e7951e6-4713-4fe4-b08d-d81c743aac2d
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NWD</abr:code>
            <abr:label>Nederzetting - Wegdorp</abr:label>
            <abr:uri>http://www.rnaproject.org/data/ccc1242f-96db-441f-b56d-399700f4202f
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>NX</abr:code>
            <abr:label>Nederzetting, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/85ae2aa0-caae-4745-aecb-6cc765a8782f
            </abr:uri>
        </abr:complex>
        <!-- Religie -->
        <abr:complex>
            <abr:code>RCP</abr:code>
            <abr:label>Religie - Cultusplaats/heiligdom/tempel</abr:label>
            <abr:uri>http://www.rnaproject.org/data/92b5cb57-ca49-473c-bee7-ea2afb9e0638
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>RKAP</abr:code>
            <abr:label>Religie - Kapel</abr:label>
            <abr:uri>http://www.rnaproject.org/data/678d5ce1-010c-41f1-b7f0-e83492d6e249
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>RKER</abr:code>
            <abr:label>Religie - Kerk</abr:label>
            <abr:uri>http://www.rnaproject.org/data/2904916e-704c-415d-8724-fd1b97a8f7e7
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>RKLO</abr:code>
            <abr:label>Religie - Klooster(complex)</abr:label>
            <abr:uri>http://www.rnaproject.org/data/54f419f0-d185-4ea5-a188-57e25493a5e0
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>RX</abr:code>
            <abr:label>Religie, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/4ffb1fd8-8c7b-4f55-8daa-b100c7e7fef9
            </abr:uri>
        </abr:complex>
        <!-- Versterking -->
        <abr:complex>
            <abr:code>VK</abr:code>
            <abr:label>Versterking - Kasteel</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f16ee988-133a-4ddd-9068-24985d4dfba4
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VKM</abr:code>
            <abr:label>Versterking - Motte/kasteelheuvel/vliedberg</abr:label>
            <abr:uri>http://www.rnaproject.org/data/05062c27-fe7a-4afa-837a-ae95ddf4dacc
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VKW</abr:code>
            <abr:label>Versterking - Waterburcht</abr:label>
            <abr:uri>http://www.rnaproject.org/data/61bf7838-56b1-4270-9648-4bb8d2e393d0
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VLP</abr:code>
            <abr:label>Versterking - Legerplaats</abr:label>
            <abr:uri>http://www.rnaproject.org/data/cc4f029d-68cf-4437-bba7-980f85f62e69
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VLW</abr:code>
            <abr:label>Versterking - Landweer</abr:label>
            <abr:uri>http://www.rnaproject.org/data/99ed8c5b-9e5f-4311-8e37-3484edd6e24c
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VSCH</abr:code>
            <abr:label>Versterking - Schans</abr:label>
            <abr:uri>http://www.rnaproject.org/data/f4a06fe5-c3d7-4800-ac1d-759dc46ae0a0
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VWAL</abr:code>
            <abr:label>Versterking - Wal/omwalling</abr:label>
            <abr:uri>http://www.rnaproject.org/data/cb67f68a-1a01-4f16-a995-f8a1eb708b2e
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VWB</abr:code>
            <abr:label>Versterking - Wal-/vluchtburcht</abr:label>
            <abr:uri>http://www.rnaproject.org/data/eb2ad681-41d1-4172-b8cf-62156b147ea2
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VWP</abr:code>
            <abr:label>Versterking - Wachtpost</abr:label>
            <abr:uri>http://www.rnaproject.org/data/cfe4e909-9d6f-4125-a831-805f304b8ecd
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>VX</abr:code>
            <abr:label>Versterking, onbepaald</abr:label>
            <abr:uri>http://www.rnaproject.org/data/99af2299-a7c5-4167-9cce-bc9efb25f5b9
            </abr:uri>
        </abr:complex>
        <abr:complex>
            <abr:code>XXX</abr:code>
            <abr:label>Onbekend</abr:label>
            <abr:uri>http://www.rnaproject.org/data/39a61516-5ebd-43ad-9cde-98b5089c71ff
            </abr:uri>
        </abr:complex>
    </abr:complexlist>
    <!-- -->

    <xsl:key name="complex-lookup" match="abr:complex" use="abr:code" />
    <xsl:variable name="complexlist-top" select="document('')/*/abr:complexlist" />
    <xsl:template name="abr_complexUri">
        <xsl:param name="code" />
        <xsl:for-each select="$complexlist-top"><!-- change context to periods table -->
            <xsl:value-of select="normalize-space(key('complex-lookup', $code)/abr:uri)" />
        </xsl:for-each>
    </xsl:template>

    <xsl:template name="abr_complexLabel">
        <xsl:param name="code" />
        <xsl:for-each select="$complexlist-top"><!-- change context to periods table -->
            <xsl:value-of select="key('complex-lookup', $code)/abr:label" />
        </xsl:for-each>
    </xsl:template>
</xsl:stylesheet>
