/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.platform.SObjects;

import com.nawforce.platform.Internal.SObjectFields$;
import com.nawforce.platform.Internal.SObjectType$;
import com.nawforce.platform.System.Boolean;
import com.nawforce.platform.System.Integer;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class UserAppMenuItem extends SObject {
	public static SObjectType$<UserAppMenuItem> SObjectType;
	public static SObjectFields$<UserAppMenuItem> Fields;

	public String AppMenuItemId;
	public Id ApplicationId;
	public SObject Application;
	public String Description;
	public String IconUrl;
	public Id Id;
	public String InfoUrl;
	public Boolean IsUsingAdminAuthorization;
	public Boolean IsVisible;
	public String Label;
	public String LogoUrl;
	public String MobileStartUrl;
	public String Name;
	public Integer SortOrder;
	public String StartUrl;
	public String Type;
	public Integer UserSortOrder;

	public UserAppMenuItem clone$() {throw new java.lang.UnsupportedOperationException();}
	public UserAppMenuItem clone$(Boolean preserveId) {throw new java.lang.UnsupportedOperationException();}
	public UserAppMenuItem clone$(Boolean preserveId, Boolean isDeepClone) {throw new java.lang.UnsupportedOperationException();}
	public UserAppMenuItem clone$(Boolean preserveId, Boolean isDeepClone, Boolean preserveReadonlyTimestamps) {throw new java.lang.UnsupportedOperationException();}
	public UserAppMenuItem clone$(Boolean preserveId, Boolean isDeepClone, Boolean preserveReadonlyTimestamps, Boolean preserveAutonumber) {throw new java.lang.UnsupportedOperationException();}
}
