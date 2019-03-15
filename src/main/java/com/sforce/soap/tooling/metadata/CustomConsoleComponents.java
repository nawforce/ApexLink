package com.sforce.soap.tooling.metadata;

/**
 * This is a generated class for the SObject Enterprise API.
 * Do not edit this file, as your changes will be lost.
 */
public class CustomConsoleComponents implements com.sforce.ws.bind.XMLizable {

    /**
     * Constructor
     */
    public CustomConsoleComponents() {}

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
     * element : primaryTabComponents of type {urn:metadata.tooling.soap.sforce.com}PrimaryTabComponents
     * java type: com.sforce.soap.tooling.metadata.PrimaryTabComponents
     */
    private boolean primaryTabComponents__is_set = false;

    private com.sforce.soap.tooling.metadata.PrimaryTabComponents primaryTabComponents;

    public com.sforce.soap.tooling.metadata.PrimaryTabComponents getPrimaryTabComponents() {
      return primaryTabComponents;
    }

    public void setPrimaryTabComponents(com.sforce.soap.tooling.metadata.PrimaryTabComponents primaryTabComponents) {
      this.primaryTabComponents = primaryTabComponents;
      primaryTabComponents__is_set = true;
    }

    protected void setPrimaryTabComponents(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("primaryTabComponents", "urn:metadata.tooling.soap.sforce.com","primaryTabComponents","urn:metadata.tooling.soap.sforce.com","PrimaryTabComponents",0,1,true))) {
        setPrimaryTabComponents((com.sforce.soap.tooling.metadata.PrimaryTabComponents)__typeMapper.readObject(__in, _lookupTypeInfo("primaryTabComponents", "urn:metadata.tooling.soap.sforce.com","primaryTabComponents","urn:metadata.tooling.soap.sforce.com","PrimaryTabComponents",0,1,true), com.sforce.soap.tooling.metadata.PrimaryTabComponents.class));
      }
    }

    private void writeFieldPrimaryTabComponents(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("primaryTabComponents", "urn:metadata.tooling.soap.sforce.com","primaryTabComponents","urn:metadata.tooling.soap.sforce.com","PrimaryTabComponents",0,1,true), primaryTabComponents, primaryTabComponents__is_set);
    }

    /**
     * element : subtabComponents of type {urn:metadata.tooling.soap.sforce.com}SubtabComponents
     * java type: com.sforce.soap.tooling.metadata.SubtabComponents
     */
    private boolean subtabComponents__is_set = false;

    private com.sforce.soap.tooling.metadata.SubtabComponents subtabComponents;

    public com.sforce.soap.tooling.metadata.SubtabComponents getSubtabComponents() {
      return subtabComponents;
    }

    public void setSubtabComponents(com.sforce.soap.tooling.metadata.SubtabComponents subtabComponents) {
      this.subtabComponents = subtabComponents;
      subtabComponents__is_set = true;
    }

    protected void setSubtabComponents(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("subtabComponents", "urn:metadata.tooling.soap.sforce.com","subtabComponents","urn:metadata.tooling.soap.sforce.com","SubtabComponents",0,1,true))) {
        setSubtabComponents((com.sforce.soap.tooling.metadata.SubtabComponents)__typeMapper.readObject(__in, _lookupTypeInfo("subtabComponents", "urn:metadata.tooling.soap.sforce.com","subtabComponents","urn:metadata.tooling.soap.sforce.com","SubtabComponents",0,1,true), com.sforce.soap.tooling.metadata.SubtabComponents.class));
      }
    }

    private void writeFieldSubtabComponents(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("subtabComponents", "urn:metadata.tooling.soap.sforce.com","subtabComponents","urn:metadata.tooling.soap.sforce.com","SubtabComponents",0,1,true), subtabComponents, subtabComponents__is_set);
    }

    /**
     */
    @Override
    public void write(javax.xml.namespace.QName __element,
        com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper)
        throws java.io.IOException {
      __out.writeStartTag(__element.getNamespaceURI(), __element.getLocalPart());
      writeFields(__out, __typeMapper);
      __out.writeEndTag(__element.getNamespaceURI(), __element.getLocalPart());
    }

    protected void writeFields(com.sforce.ws.parser.XmlOutputStream __out,
         com.sforce.ws.bind.TypeMapper __typeMapper)
         throws java.io.IOException {
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
        loadFields1(__in, __typeMapper);
    }

    @Override
    public String toString() {
      java.lang.StringBuilder sb = new java.lang.StringBuilder();
      sb.append("[CustomConsoleComponents ");
      toString1(sb);

      sb.append("]\n");
      return sb.toString();
    }

    private void toStringHelper(StringBuilder sb, String name, Object value) {
      sb.append(' ').append(name).append("='").append(com.sforce.ws.util.Verbose.toString(value)).append("'\n");
    }

    private void writeFields1(com.sforce.ws.parser.XmlOutputStream __out,
         com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      writeFieldPrimaryTabComponents(__out, __typeMapper);
      writeFieldSubtabComponents(__out, __typeMapper);
    }

    private void loadFields1(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      setPrimaryTabComponents(__in, __typeMapper);
      setSubtabComponents(__in, __typeMapper);
    }

    private void toString1(StringBuilder sb) {
      toStringHelper(sb, "primaryTabComponents", primaryTabComponents);
      toStringHelper(sb, "subtabComponents", subtabComponents);
    }


}