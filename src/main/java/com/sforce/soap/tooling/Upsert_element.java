package com.sforce.soap.tooling;

/**
 * This is a generated class for the SObject Enterprise API.
 * Do not edit this file, as your changes will be lost.
 */
public class Upsert_element implements com.sforce.ws.bind.XMLizable {

    /**
     * Constructor
     */
    public Upsert_element() {}

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
     * element : fieldName of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean fieldName__is_set = false;

    private java.lang.String fieldName;

    public java.lang.String getFieldName() {
      return fieldName;
    }

    public void setFieldName(java.lang.String fieldName) {
      this.fieldName = fieldName;
      fieldName__is_set = true;
    }

    protected void setFieldName(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.verifyElement(__in, _lookupTypeInfo("fieldName", "urn:tooling.soap.sforce.com","fieldName","http://www.w3.org/2001/XMLSchema","string",1,1,true))) {
        setFieldName(__typeMapper.readString(__in, _lookupTypeInfo("fieldName", "urn:tooling.soap.sforce.com","fieldName","http://www.w3.org/2001/XMLSchema","string",1,1,true), java.lang.String.class));
      }
    }

    private void writeFieldFieldName(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("fieldName", "urn:tooling.soap.sforce.com","fieldName","http://www.w3.org/2001/XMLSchema","string",1,1,true), fieldName, fieldName__is_set);
    }

    /**
     * element : entities of type {urn:sobject.tooling.soap.sforce.com}sObject
     * java type: com.sforce.soap.tooling.sobject.SObject[]
     */
    private boolean entities__is_set = false;

    private com.sforce.soap.tooling.sobject.SObject[] entities = new com.sforce.soap.tooling.sobject.SObject[0];

    public com.sforce.soap.tooling.sobject.SObject[] getEntities() {
      return entities;
    }

    public void setEntities(com.sforce.soap.tooling.sobject.SObject[] entities) {
      this.entities = entities;
      entities__is_set = true;
    }

    protected void setEntities(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("entities", "urn:tooling.soap.sforce.com","entities","urn:sobject.tooling.soap.sforce.com","sObject",0,-1,true))) {
        setEntities((com.sforce.soap.tooling.sobject.SObject[])__typeMapper.readObject(__in, _lookupTypeInfo("entities", "urn:tooling.soap.sforce.com","entities","urn:sobject.tooling.soap.sforce.com","sObject",0,-1,true), com.sforce.soap.tooling.sobject.SObject[].class));
      }
    }

    private void writeFieldEntities(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("entities", "urn:tooling.soap.sforce.com","entities","urn:sobject.tooling.soap.sforce.com","sObject",0,-1,true), entities, entities__is_set);
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
      sb.append("[Upsert_element ");
      toString1(sb);

      sb.append("]\n");
      return sb.toString();
    }

    private void toStringHelper(StringBuilder sb, String name, Object value) {
      sb.append(' ').append(name).append("='").append(com.sforce.ws.util.Verbose.toString(value)).append("'\n");
    }

    private void writeFields1(com.sforce.ws.parser.XmlOutputStream __out,
         com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      writeFieldFieldName(__out, __typeMapper);
      writeFieldEntities(__out, __typeMapper);
    }

    private void loadFields1(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      setFieldName(__in, __typeMapper);
      setEntities(__in, __typeMapper);
    }

    private void toString1(StringBuilder sb) {
      toStringHelper(sb, "fieldName", fieldName);
      toStringHelper(sb, "entities", entities);
    }


}