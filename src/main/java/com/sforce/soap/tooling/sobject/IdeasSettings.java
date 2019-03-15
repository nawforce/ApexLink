package com.sforce.soap.tooling.sobject;

/**
 * This is a generated class for the SObject Enterprise API.
 * Do not edit this file, as your changes will be lost.
 */
public class IdeasSettings extends com.sforce.soap.tooling.sobject.SObject {

    /**
     * Constructor
     */
    public IdeasSettings() {}

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
     * element : DurableId of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean DurableId__is_set = false;

    private java.lang.String DurableId;

    public java.lang.String getDurableId() {
      return DurableId;
    }

    public void setDurableId(java.lang.String DurableId) {
      this.DurableId = DurableId;
      DurableId__is_set = true;
    }

    protected void setDurableId(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("DurableId", "urn:sobject.tooling.soap.sforce.com","DurableId","http://www.w3.org/2001/XMLSchema","string",0,1,true))) {
        setDurableId(__typeMapper.readString(__in, _lookupTypeInfo("DurableId", "urn:sobject.tooling.soap.sforce.com","DurableId","http://www.w3.org/2001/XMLSchema","string",0,1,true), java.lang.String.class));
      }
    }

    private void writeFieldDurableId(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("DurableId", "urn:sobject.tooling.soap.sforce.com","DurableId","http://www.w3.org/2001/XMLSchema","string",0,1,true), DurableId, DurableId__is_set);
    }

    /**
     * element : FullName of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean FullName__is_set = false;

    private java.lang.String FullName;

    public java.lang.String getFullName() {
      return FullName;
    }

    public void setFullName(java.lang.String FullName) {
      this.FullName = FullName;
      FullName__is_set = true;
    }

    protected void setFullName(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("FullName", "urn:sobject.tooling.soap.sforce.com","FullName","http://www.w3.org/2001/XMLSchema","string",0,1,true))) {
        setFullName(__typeMapper.readString(__in, _lookupTypeInfo("FullName", "urn:sobject.tooling.soap.sforce.com","FullName","http://www.w3.org/2001/XMLSchema","string",0,1,true), java.lang.String.class));
      }
    }

    private void writeFieldFullName(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("FullName", "urn:sobject.tooling.soap.sforce.com","FullName","http://www.w3.org/2001/XMLSchema","string",0,1,true), FullName, FullName__is_set);
    }

    /**
     * element : HalfLife of type {http://www.w3.org/2001/XMLSchema}double
     * java type: java.lang.Double
     */
    private boolean HalfLife__is_set = false;

    private java.lang.Double HalfLife;

    public java.lang.Double getHalfLife() {
      return HalfLife;
    }

    public void setHalfLife(java.lang.Double HalfLife) {
      this.HalfLife = HalfLife;
      HalfLife__is_set = true;
    }

    protected void setHalfLife(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("HalfLife", "urn:sobject.tooling.soap.sforce.com","HalfLife","http://www.w3.org/2001/XMLSchema","double",0,1,true))) {
        setHalfLife((java.lang.Double)__typeMapper.readObject(__in, _lookupTypeInfo("HalfLife", "urn:sobject.tooling.soap.sforce.com","HalfLife","http://www.w3.org/2001/XMLSchema","double",0,1,true), java.lang.Double.class));
      }
    }

    private void writeFieldHalfLife(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("HalfLife", "urn:sobject.tooling.soap.sforce.com","HalfLife","http://www.w3.org/2001/XMLSchema","double",0,1,true), HalfLife, HalfLife__is_set);
    }

    /**
     * element : IdeasProfilePage of type {http://www.w3.org/2001/XMLSchema}string
     * java type: java.lang.String
     */
    private boolean IdeasProfilePage__is_set = false;

    private java.lang.String IdeasProfilePage;

    public java.lang.String getIdeasProfilePage() {
      return IdeasProfilePage;
    }

    public void setIdeasProfilePage(java.lang.String IdeasProfilePage) {
      this.IdeasProfilePage = IdeasProfilePage;
      IdeasProfilePage__is_set = true;
    }

    protected void setIdeasProfilePage(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("IdeasProfilePage", "urn:sobject.tooling.soap.sforce.com","IdeasProfilePage","http://www.w3.org/2001/XMLSchema","string",0,1,true))) {
        setIdeasProfilePage(__typeMapper.readString(__in, _lookupTypeInfo("IdeasProfilePage", "urn:sobject.tooling.soap.sforce.com","IdeasProfilePage","http://www.w3.org/2001/XMLSchema","string",0,1,true), java.lang.String.class));
      }
    }

    private void writeFieldIdeasProfilePage(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("IdeasProfilePage", "urn:sobject.tooling.soap.sforce.com","IdeasProfilePage","http://www.w3.org/2001/XMLSchema","string",0,1,true), IdeasProfilePage, IdeasProfilePage__is_set);
    }

    /**
     * element : IsChatterProfileEnabled of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: java.lang.Boolean
     */
    private boolean IsChatterProfileEnabled__is_set = false;

    private java.lang.Boolean IsChatterProfileEnabled;

    public java.lang.Boolean getIsChatterProfileEnabled() {
      return IsChatterProfileEnabled;
    }

    public void setIsChatterProfileEnabled(java.lang.Boolean IsChatterProfileEnabled) {
      this.IsChatterProfileEnabled = IsChatterProfileEnabled;
      IsChatterProfileEnabled__is_set = true;
    }

    protected void setIsChatterProfileEnabled(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("IsChatterProfileEnabled", "urn:sobject.tooling.soap.sforce.com","IsChatterProfileEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setIsChatterProfileEnabled((java.lang.Boolean)__typeMapper.readObject(__in, _lookupTypeInfo("IsChatterProfileEnabled", "urn:sobject.tooling.soap.sforce.com","IsChatterProfileEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), java.lang.Boolean.class));
      }
    }

    private void writeFieldIsChatterProfileEnabled(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("IsChatterProfileEnabled", "urn:sobject.tooling.soap.sforce.com","IsChatterProfileEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), IsChatterProfileEnabled, IsChatterProfileEnabled__is_set);
    }

    /**
     * element : IsIdeaThemesEnabled of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: java.lang.Boolean
     */
    private boolean IsIdeaThemesEnabled__is_set = false;

    private java.lang.Boolean IsIdeaThemesEnabled;

    public java.lang.Boolean getIsIdeaThemesEnabled() {
      return IsIdeaThemesEnabled;
    }

    public void setIsIdeaThemesEnabled(java.lang.Boolean IsIdeaThemesEnabled) {
      this.IsIdeaThemesEnabled = IsIdeaThemesEnabled;
      IsIdeaThemesEnabled__is_set = true;
    }

    protected void setIsIdeaThemesEnabled(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("IsIdeaThemesEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeaThemesEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setIsIdeaThemesEnabled((java.lang.Boolean)__typeMapper.readObject(__in, _lookupTypeInfo("IsIdeaThemesEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeaThemesEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), java.lang.Boolean.class));
      }
    }

    private void writeFieldIsIdeaThemesEnabled(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("IsIdeaThemesEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeaThemesEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), IsIdeaThemesEnabled, IsIdeaThemesEnabled__is_set);
    }

    /**
     * element : IsIdeasEnabled of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: java.lang.Boolean
     */
    private boolean IsIdeasEnabled__is_set = false;

    private java.lang.Boolean IsIdeasEnabled;

    public java.lang.Boolean getIsIdeasEnabled() {
      return IsIdeasEnabled;
    }

    public void setIsIdeasEnabled(java.lang.Boolean IsIdeasEnabled) {
      this.IsIdeasEnabled = IsIdeasEnabled;
      IsIdeasEnabled__is_set = true;
    }

    protected void setIsIdeasEnabled(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("IsIdeasEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeasEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setIsIdeasEnabled((java.lang.Boolean)__typeMapper.readObject(__in, _lookupTypeInfo("IsIdeasEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeasEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), java.lang.Boolean.class));
      }
    }

    private void writeFieldIsIdeasEnabled(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("IsIdeasEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeasEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), IsIdeasEnabled, IsIdeasEnabled__is_set);
    }

    /**
     * element : IsIdeasReputationEnabled of type {http://www.w3.org/2001/XMLSchema}boolean
     * java type: java.lang.Boolean
     */
    private boolean IsIdeasReputationEnabled__is_set = false;

    private java.lang.Boolean IsIdeasReputationEnabled;

    public java.lang.Boolean getIsIdeasReputationEnabled() {
      return IsIdeasReputationEnabled;
    }

    public void setIsIdeasReputationEnabled(java.lang.Boolean IsIdeasReputationEnabled) {
      this.IsIdeasReputationEnabled = IsIdeasReputationEnabled;
      IsIdeasReputationEnabled__is_set = true;
    }

    protected void setIsIdeasReputationEnabled(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("IsIdeasReputationEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeasReputationEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true))) {
        setIsIdeasReputationEnabled((java.lang.Boolean)__typeMapper.readObject(__in, _lookupTypeInfo("IsIdeasReputationEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeasReputationEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), java.lang.Boolean.class));
      }
    }

    private void writeFieldIsIdeasReputationEnabled(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("IsIdeasReputationEnabled", "urn:sobject.tooling.soap.sforce.com","IsIdeasReputationEnabled","http://www.w3.org/2001/XMLSchema","boolean",0,1,true), IsIdeasReputationEnabled, IsIdeasReputationEnabled__is_set);
    }

    /**
     * element : Metadata of type {urn:metadata.tooling.soap.sforce.com}IdeasSettings
     * java type: com.sforce.soap.tooling.metadata.IdeasSettings
     */
    private boolean Metadata__is_set = false;

    private com.sforce.soap.tooling.metadata.IdeasSettings Metadata;

    public com.sforce.soap.tooling.metadata.IdeasSettings getMetadata() {
      return Metadata;
    }

    public void setMetadata(com.sforce.soap.tooling.metadata.IdeasSettings Metadata) {
      this.Metadata = Metadata;
      Metadata__is_set = true;
    }

    protected void setMetadata(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      __in.peekTag();
      if (__typeMapper.isElement(__in, _lookupTypeInfo("Metadata", "urn:sobject.tooling.soap.sforce.com","Metadata","urn:metadata.tooling.soap.sforce.com","IdeasSettings",0,1,true))) {
        setMetadata((com.sforce.soap.tooling.metadata.IdeasSettings)__typeMapper.readObject(__in, _lookupTypeInfo("Metadata", "urn:sobject.tooling.soap.sforce.com","Metadata","urn:metadata.tooling.soap.sforce.com","IdeasSettings",0,1,true), com.sforce.soap.tooling.metadata.IdeasSettings.class));
      }
    }

    private void writeFieldMetadata(com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException {
      __typeMapper.writeObject(__out, _lookupTypeInfo("Metadata", "urn:sobject.tooling.soap.sforce.com","Metadata","urn:metadata.tooling.soap.sforce.com","IdeasSettings",0,1,true), Metadata, Metadata__is_set);
    }

    /**
     */
    @Override
    public void write(javax.xml.namespace.QName __element,
        com.sforce.ws.parser.XmlOutputStream __out, com.sforce.ws.bind.TypeMapper __typeMapper)
        throws java.io.IOException {
      __out.writeStartTag(__element.getNamespaceURI(), __element.getLocalPart());
      __typeMapper.writeXsiType(__out, "urn:sobject.tooling.soap.sforce.com", "IdeasSettings");
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
      sb.append("[IdeasSettings ");
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
      writeFieldDurableId(__out, __typeMapper);
      writeFieldFullName(__out, __typeMapper);
      writeFieldHalfLife(__out, __typeMapper);
      writeFieldIdeasProfilePage(__out, __typeMapper);
      writeFieldIsChatterProfileEnabled(__out, __typeMapper);
      writeFieldIsIdeaThemesEnabled(__out, __typeMapper);
      writeFieldIsIdeasEnabled(__out, __typeMapper);
      writeFieldIsIdeasReputationEnabled(__out, __typeMapper);
      writeFieldMetadata(__out, __typeMapper);
    }

    private void loadFields1(com.sforce.ws.parser.XmlInputStream __in,
        com.sforce.ws.bind.TypeMapper __typeMapper) throws java.io.IOException, com.sforce.ws.ConnectionException {
      setDurableId(__in, __typeMapper);
      setFullName(__in, __typeMapper);
      setHalfLife(__in, __typeMapper);
      setIdeasProfilePage(__in, __typeMapper);
      setIsChatterProfileEnabled(__in, __typeMapper);
      setIsIdeaThemesEnabled(__in, __typeMapper);
      setIsIdeasEnabled(__in, __typeMapper);
      setIsIdeasReputationEnabled(__in, __typeMapper);
      setMetadata(__in, __typeMapper);
    }

    private void toString1(StringBuilder sb) {
      toStringHelper(sb, "DurableId", DurableId);
      toStringHelper(sb, "FullName", FullName);
      toStringHelper(sb, "HalfLife", HalfLife);
      toStringHelper(sb, "IdeasProfilePage", IdeasProfilePage);
      toStringHelper(sb, "IsChatterProfileEnabled", IsChatterProfileEnabled);
      toStringHelper(sb, "IsIdeaThemesEnabled", IsIdeaThemesEnabled);
      toStringHelper(sb, "IsIdeasEnabled", IsIdeasEnabled);
      toStringHelper(sb, "IsIdeasReputationEnabled", IsIdeasReputationEnabled);
      toStringHelper(sb, "Metadata", Metadata);
    }


}