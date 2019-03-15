package com.sforce.soap.tooling.metadata;

/**
 * This is a generated class for the SObject Enterprise API.
 * Do not edit this file, as your changes will be lost.
 */
public class StandardValue extends com.sforce.soap.tooling.metadata.CustomValue {

    /**
     * Constructor
     */
    public StandardValue() {}

    /* Cache the typeInfo instead of declaring static fields throughout*/
    private transient java.util.Map<String, com.sforce.ws.bind.TypeInfo> typeInfoCache = new java.util.HashMap<String, com.sforce.ws.bind.TypeInfo>();
    private com.sforce.ws.bind.TypeInfo _lookupTypeInfo(String fieldName, String namespace, String name, String typeNS, String type, int minOcc, int maxOcc, boolean elementForm) {
      com.sforce.ws.bind.TypeInfo typeInfo = typeInfoCache.get(fieldName);
      if (typeInfo == null) {
        typeInfo = new com.sforce.ws.bind.TypeInfo(namespace, name, typeNS, type, minOcc, maxOcc, elementForm);
        typeInfoCache.put(fieldName, typeInfo);
      }
      return typeInfo;
    }

    /**
     * element : allowEmail of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean allowEmail__is_set = false;

    private boolean allowEmail;

    public boolean getAllowEmail() {
      return allowEmail;
    }

    public boolean isAllowEmail() {
      return allowEmail;
    }

    public void setAllowEmail(boolean allowEmail) {
      this.allowEmail = allowEmail;
      allowEmail__is_set = true;
    }

    protected void setAllowEmail(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("allowEmail", "urn:metadata.tooling.soap.sforce.com","allowEmail","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setAllowEmail(__typeMapper.readBoolean(__in, _lookupTypeInfo("allowEmail", "urn:metadata.tooling.soap.sforce.com","allowEmail","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldAllowEmail(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("allowEmail", "urn:metadata.tooling.soap.sforce.com","allowEmail","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), allowEmail, allowEmail__is_set);
    }

    /**
     * element : closed of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean closed__is_set = false;

    private boolean closed;

    public boolean getClosed() {
      return closed;
    }

    public boolean isClosed() {
      return closed;
    }

    public void setClosed(boolean closed) {
      this.closed = closed;
      closed__is_set = true;
    }

    protected void setClosed(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("closed", "urn:metadata.tooling.soap.sforce.com","closed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setClosed(__typeMapper.readBoolean(__in, _lookupTypeInfo("closed", "urn:metadata.tooling.soap.sforce.com","closed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldClosed(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("closed", "urn:metadata.tooling.soap.sforce.com","closed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), closed, closed__is_set);
    }

    /**
     * element : converted of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean converted__is_set = false;

    private boolean converted;

    public boolean getConverted() {
      return converted;
    }

    public boolean isConverted() {
      return converted;
    }

    public void setConverted(boolean converted) {
      this.converted = converted;
      converted__is_set = true;
    }

    protected void setConverted(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("converted", "urn:metadata.tooling.soap.sforce.com","converted","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setConverted(__typeMapper.readBoolean(__in, _lookupTypeInfo("converted", "urn:metadata.tooling.soap.sforce.com","converted","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldConverted(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("converted", "urn:metadata.tooling.soap.sforce.com","converted","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), converted, converted__is_set);
    }

    /**
     * element : cssExposed of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean cssExposed__is_set = false;

    private boolean cssExposed;

    public boolean getCssExposed() {
      return cssExposed;
    }

    public boolean isCssExposed() {
      return cssExposed;
    }

    public void setCssExposed(boolean cssExposed) {
      this.cssExposed = cssExposed;
      cssExposed__is_set = true;
    }

    protected void setCssExposed(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("cssExposed", "urn:metadata.tooling.soap.sforce.com","cssExposed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setCssExposed(__typeMapper.readBoolean(__in, _lookupTypeInfo("cssExposed", "urn:metadata.tooling.soap.sforce.com","cssExposed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldCssExposed(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("cssExposed", "urn:metadata.tooling.soap.sforce.com","cssExposed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), cssExposed, cssExposed__is_set);
    }

    /**
     * element : forecastCategory of type {urn:tooling.soap.sforce.com}ForecastCategories
     * java type: com.sforce.soap.tooling.ForecastCategories
     */
    private boolean forecastCategory__is_set = false;

    private com.sforce.soap.tooling.ForecastCategories forecastCategory;

    public com.sforce.soap.tooling.ForecastCategories getForecastCategory() {
      return forecastCategory;
    }

    public void setForecastCategory(com.sforce.soap.tooling.ForecastCategories forecastCategory) {
      this.forecastCategory = forecastCategory;
      forecastCategory__is_set = true;
    }

    protected void setForecastCategory(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("forecastCategory", "urn:metadata.tooling.soap.sforce.com","forecastCategory","urn:tooling.soap.sforce.com","ForecastCategories",0,1,true))) {
        setForecastCategory((com.sforce.soap.tooling.ForecastCategories)__typeMapper.readObject(__in, _lookupTypeInfo("forecastCategory", "urn:metadata.tooling.soap.sforce.com","forecastCategory","urn:tooling.soap.sforce.com","ForecastCategories",0,1,true), com.sforce.soap.tooling.ForecastCategories.class));
      }
    }

    private void writeFieldForecastCategory(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("forecastCategory", "urn:metadata.tooling.soap.sforce.com","forecastCategory","urn:tooling.soap.sforce.com","ForecastCategories",0,1,true), forecastCategory, forecastCategory__is_set);
    }

    /**
     * element : groupingString of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean groupingString__is_set = false;

    private java.lang.String groupingString;

    public java.lang.String getGroupingString() {
      return groupingString;
    }

    public void setGroupingString(java.lang.String groupingString) {
      this.groupingString = groupingString;
      groupingString__is_set = true;
    }

    protected void setGroupingString(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("groupingString", "urn:metadata.tooling.soap.sforce.com","groupingString","http://www.w3.org/2001/XMLSchema","string",0,1,true))) {
        setGroupingString(__typeMapper.readString(__in, _lookupTypeInfo("groupingString", "urn:metadata.tooling.soap.sforce.com","groupingString","http://www.w3.org/2001/XMLSchema","string",0,1,true), java.lang.String.class));
      }
    }

    private void writeFieldGroupingString(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("groupingString", "urn:metadata.tooling.soap.sforce.com","groupingString","http://www.w3.org/2001/XMLSchema","string",0,1,true), groupingString, groupingString__is_set);
    }

    /**
     * element : highPriority of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean highPriority__is_set = false;

    private boolean highPriority;

    public boolean getHighPriority() {
      return highPriority;
    }

    public boolean isHighPriority() {
      return highPriority;
    }

    public void setHighPriority(boolean highPriority) {
      this.highPriority = highPriority;
      highPriority__is_set = true;
    }

    protected void setHighPriority(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("highPriority", "urn:metadata.tooling.soap.sforce.com","highPriority","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setHighPriority(__typeMapper.readBoolean(__in, _lookupTypeInfo("highPriority", "urn:metadata.tooling.soap.sforce.com","highPriority","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldHighPriority(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("highPriority", "urn:metadata.tooling.soap.sforce.com","highPriority","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), highPriority, highPriority__is_set);
    }

    /**
     * element : probability of type {http://www.w3.org/2001/XMLSchema}int
     * java type: int
     */
    private boolean probability__is_set = false;

    private int probability;

    public int getProbability() {
      return probability;
    }

    public void setProbability(int probability) {
      this.probability = probability;
      probability__is_set = true;
    }

    protected void setProbability(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("probability", "urn:metadata.tooling.soap.sforce.com","probability","http://www.w3.org/2001/XMLSchema","int",0,1,true))) {
        setProbability((int)__typeMapper.readInt(__in, _lookupTypeInfo("probability", "urn:metadata.tooling.soap.sforce.com","probability","http://www.w3.org/2001/XMLSchema","int",0,1,true), int.class));
      }
    }

    private void writeFieldProbability(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("probability", "urn:metadata.tooling.soap.sforce.com","probability","http://www.w3.org/2001/XMLSchema","int",0,1,true), probability, probability__is_set);
    }

    /**
     * element : reverseRole of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean reverseRole__is_set = false;

    private java.lang.String reverseRole;

    public java.lang.String getReverseRole() {
      return reverseRole;
    }

    public void setReverseRole(java.lang.String reverseRole) {
      this.reverseRole = reverseRole;
      reverseRole__is_set = true;
    }

    protected void setReverseRole(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("reverseRole", "urn:metadata.tooling.soap.sforce.com","reverseRole","http://www.w3.org/2001/XMLSchema","string",0,1,true))) {
        setReverseRole(__typeMapper.readString(__in, _lookupTypeInfo("reverseRole", "urn:metadata.tooling.soap.sforce.com","reverseRole","http://www.w3.org/2001/XMLSchema","string",0,1,true), java.lang.String.class));
      }
    }

    private void writeFieldReverseRole(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("reverseRole", "urn:metadata.tooling.soap.sforce.com","reverseRole","http://www.w3.org/2001/XMLSchema","string",0,1,true), reverseRole, reverseRole__is_set);
    }

    /**
     * element : reviewed of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean reviewed__is_set = false;

    private boolean reviewed;

    public boolean getReviewed() {
      return reviewed;
    }

    public boolean isReviewed() {
      return reviewed;
    }

    public void setReviewed(boolean reviewed) {
      this.reviewed = reviewed;
      reviewed__is_set = true;
    }

    protected void setReviewed(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("reviewed", "urn:metadata.tooling.soap.sforce.com","reviewed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setReviewed(__typeMapper.readBoolean(__in, _lookupTypeInfo("reviewed", "urn:metadata.tooling.soap.sforce.com","reviewed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldReviewed(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("reviewed", "urn:metadata.tooling.soap.sforce.com","reviewed","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), reviewed, reviewed__is_set);
    }

    /**
     * element : won of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: boolean
     */
    private boolean won__is_set = false;

    private boolean won;

    public boolean getWon() {
      return won;
    }

    public boolean isWon() {
      return won;
    }

    public void setWon(boolean won) {
      this.won = won;
      won__is_set = true;
    }

    protected void setWon(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("won", "urn:metadata.tooling.soap.sforce.com","won","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setWon(__typeMapper.readBoolean(__in, _lookupTypeInfo("won", "urn:metadata.tooling.soap.sforce.com","won","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), boolean.class));
      }
    }

    private void writeFieldWon(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("won", "urn:metadata.tooling.soap.sforce.com","won","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), won, won__is_set);
    }

    /**
     */
    @Override
    public void write(javax.xml.namespace.QName __element,
        com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper)
        throws java.io.IOException {
      __out.writeStartTag(__element.getNamespaceURI(), __element.getLocalPart());
      __typeMapper.writeXsiType(__out, "urn:metadata.tooling.soap.sforce.com", "StandardValue");
      writeFields(__out, __typeMapper);
      __out.writeEndTag(__element.getNamespaceURI(), __element.getLocalPart());
    }

    protected void writeFields(com.sforce.ws.parser.XmlOutputStream __out,
         com.sforce.ws.bind.TypeMapper __typeMapper)
         throws java.io.IOException {
       super.writeFields(__out, __typeMapper);
       writeFields1(__out, __typeMapper);
    }

    @Override
    public void load(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __typeMapper.consumeStartTag(__in);
      loadFields(__in, __typeMapper);
      __typeMapper.consumeEndTag(__in);
    }

    protected void loadFields(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
        super.loadFields(__in, __typeMapper);
        loadFields1(__in, __typeMapper);
    }

    @Override
    public String toString() {
      java.lang.StringBuilder sb = new java.lang.StringBuilder();
      sb.append("[StandardValue ");
      sb.append(super.toString());
      toString1(sb);

      sb.append("]\n");
      return sb.toString();
    }

    private void toStringHelper(StringBuilder sb, String name, Object value) {
      sb.append(' ').append(name).append("='").append(com.sforce.ws.util.Verbose.toString(value)).append("'\n");
    }

    private void writeFields1(com.sforce.ws.parser.XmlOutputStream __out,
         com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      writeFieldAllowEmail(__out, __typeMapper);
      writeFieldClosed(__out, __typeMapper);
      writeFieldConverted(__out, __typeMapper);
      writeFieldCssExposed(__out, __typeMapper);
      writeFieldForecastCategory(__out, __typeMapper);
      writeFieldGroupingString(__out, __typeMapper);
      writeFieldHighPriority(__out, __typeMapper);
      writeFieldProbability(__out, __typeMapper);
      writeFieldReverseRole(__out, __typeMapper);
      writeFieldReviewed(__out, __typeMapper);
      writeFieldWon(__out, __typeMapper);
    }

    private void loadFields1(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      setAllowEmail(__in, __typeMapper);
      setClosed(__in, __typeMapper);
      setConverted(__in, __typeMapper);
      setCssExposed(__in, __typeMapper);
      setForecastCategory(__in, __typeMapper);
      setGroupingString(__in, __typeMapper);
      setHighPriority(__in, __typeMapper);
      setProbability(__in, __typeMapper);
      setReverseRole(__in, __typeMapper);
      setReviewed(__in, __typeMapper);
      setWon(__in, __typeMapper);
    }

    private void toString1(StringBuilder sb) {
      toStringHelper(sb, "allowEmail", allowEmail);
      toStringHelper(sb, "closed", closed);
      toStringHelper(sb, "converted", converted);
      toStringHelper(sb, "cssExposed", cssExposed);
      toStringHelper(sb, "forecastCategory", forecastCategory);
      toStringHelper(sb, "groupingString", groupingString);
      toStringHelper(sb, "highPriority", highPriority);
      toStringHelper(sb, "probability", probability);
      toStringHelper(sb, "reverseRole", reverseRole);
      toStringHelper(sb, "reviewed", reviewed);
      toStringHelper(sb, "won", won);
    }


}