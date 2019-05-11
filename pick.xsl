<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes" omit-xml-declaration="yes" />
  <xsl:template match="/">
    <xsl:for-each select="results/a">
title:<xsl:value-of select="./@title"/>||||||||||href:<xsl:value-of select="./@href"/>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
