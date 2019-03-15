package com.sforce.soap.tooling.metadata;

/**
 * This is a generated class for the SObject Enterprise API.
 * Do not edit this file, as your changes will be lost.
 */
public class FieldSet extends com.sforce.soap.tooling.metadata.Metadata {

    /**
     * Constructor
     */
    public FieldSet() {}

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
     * element : availableFields of type {urn:metadata.tooling.soap.sforce.com}FieldSetItem
     * java type: com.sforce.soap.tooling.metadata.FieldSetItem[]
     */
    private boolean availableFields__is_set = false;

    private com.sforce.soap.tooling.metadata.FieldSetItem[] availableFields = new com.sforce.soap.tooling.metadata.FieldSetItem[0];

    public com.sforce.soap.tooling.metadata.FieldSetItem[] getAvailableFields() {
      return availableFields;
    }

    public void setAvailableFields(com.sforce.soap.tooling.metadata.FieldSetItem[] availableFields) {
      this.availableFields = availableFields;
      availableFields__is_set = true;
    }

    protected void setAvailableFields(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("availableFields", "urn:metadata.tooling.soap.sforce.com","availableFields","urn:metadata.tooling.soap.sforce.com","FieldSetItem",0,-1,true))) {
        setAvailableFields((com.sforce.soap.tooling.metadata.FieldSetItem[])__typeMapper.readObject(__in, _lookupTypeInfo("availableFields", "urn:metadata.tooling.soap.sforce.com","availableFields","urn:metadata.tooling.soap.sforce.com","FieldSetItem",0,-1,true), com.sforce.soap.tooling.metadata.FieldSetItem[].class));
      }
    }

    private void writeFieldAvailableFields(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("availableFields", "urn:metadata.tooling.soap.sforce.com","availableFields","urn:metadata.tooling.soap.sforce.com","FieldSetItem",0,-1,true), availableFields, availableFields__is_set);
    }

    /**
     * element : description of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean description__is_set = false;

    private java.lang.String description;

    public java.lang.String getDescription() {
      return description;
    }

    public void setDescription(java.lang.String description) {
      this.description = description;
      description__is_set = true;
    }

    protected void setDescription(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.verifyElement(__in, _lookupTypeInfo("description", "urn:metadata.tooling.soap.sforce.com","description","http://www.w3.org/2001/XMLSchema","string",1,1,true))) {
        setDescription(__typeMapper.readString(__in, _lookupTypeInfo("description", "urn:metadata.tooling.soap.sforce.com","description","http://www.w3.org/2001/XMLSchema","string",1,1,true), java.lang.String.class));
      }
    }

    private void writeFieldDescription(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("description", "urn:metadata.tooling.soap.sforce.com","description","http://www.w3.org/2001/XMLSchema","string",1,1,true), description, description__is_set);
    }

    /**
     * element : displayedFields of type {urn:metadata.tooling.soap.sforce.com}FieldSetItem
     * java type: com.sforce.soap.tooling.metadata.FieldSetItem[]
     */
    private boolean displayedFields__is_set = false;

    private com.sforce.soap.tooling.metadata.FieldSetItem[] displayedFields = new com.sforce.soap.tooling.metadata.FieldSetItem[0];

    public com.sforce.soap.tooling.metadata.FieldSetItem[] getDisplayedFields() {
      return displayedFields;
    }

    public void setDisplayedFields(com.sforce.soap.tooling.metadata.FieldSetItem[] displayedFields) {
      this.displayedFields = displayedFields;
      displayedFields__is_set = true;
    }

    protected void setDisplayedFields(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("displayedFields", "urn:metadata.tooling.soap.sforce.com","displayedFields","urn:metadata.tooling.soap.sforce.com","FieldSetItem",0,-1,true))) {
        setDisplayedFields((com.sforce.soap.tooling.metadata.FieldSetItem[])__typeMapper.readObject(__in, _lookupTypeInfo("displayedFields", "urn:metadata.tooling.soap.sforce.com","displayedFields","urn:metadata.tooling.soap.sforce.com","FieldSetItem",0,-1,true), com.sforce.soap.tooling.metadata.FieldSetItem[].class));
      }
    }

    private void writeFieldDisplayedFields(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("displayedFields", "urn:metadata.tooling.soap.sforce.com","displayedFields","urn:metadata.tooling.soap.sforce.com","FieldSetItem",0,-1,true), displayedFields, displayedFields__is_set);
    }

    /**
     * element : label of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean label__is_set = false;

    private java.lang.String label;

    public java.lang.String getLabel() {
      return label;
    }

    public void setLabel(java.lang.String label) {
      this.label = label;
      label__is_set = true;
    }

    protected void setLabel(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.verifyElement(__in, _lookupTypeInfo("label", "urn:metadata.tooling.soap.sforce.com","label","http://www.w3.org/2001/XMLSchema","string",1,1,true))) {
        setLabel(__typeMapper.readString(__in, _lookupTypeInfo("label", "urn:metadata.tooling.soap.sforce.com","label","http://www.w3.org/2001/XMLSchema","string",1,1,true), java.lang.String.class));
      }
    }

    private void writeFieldLabel(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("label", "urn:metadata.tooling.soap.sforce.com","label","http://www.w3.org/2001/XMLSchema","string",1,1,true), label, label__is_set);
    }

    /**
     */
    @Override
    public void write(javax.xml.namespace.QName __element,
        com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper)
        throws java.io.IOException {
      __out.writeStartTag(__element.getNamespaceURI(), __element.getLocalPart());
      __typeMapper.writeXsiType(__out, "urn:metadata.tooling.soap.sforce.com", "FieldSet");
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
      sb.append("[FieldSet ");
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
      writeFieldAvailableFields(__out, __typeMapper);
      writeFieldDescription(__out, __typeMapper);
      writeFieldDisplayedFields(__out, __typeMapper);
      writeFieldLabel(__out, __typeMapper);
    }

    private void loadFields1(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      setAvailableFields(__in, __typeMapper);
      setDescription(__in, __typeMapper);
      setDisplayedFields(__in, __typeMapper);
      setLabel(__in, __typeMapper);
    }

    private void toString1(StringBuilder sb) {
      toStringHelper(sb, "availableFields", availableFields);
      toStringHelper(sb, "description", description);
      toStringHelper(sb, "displayedFields", displayedFields);
      toStringHelper(sb, "label", label);
    }


}