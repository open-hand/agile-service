package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2022-07-11
 */
public class StatusTransferType {

    public static final String SPECIFIER = "specifier";
    public static final String PROJECT_OWNER = "projectOwner";
    public static final String OTHER = "other";
    public static final String ROLE = "role";

    public static boolean isSpecifier(String value) {
        return SPECIFIER.equals(value);
    }

    public static boolean isRole(String value) {
        return ROLE.equals(value);
    }

    public static boolean isOther(String value) {
        return OTHER.equals(value);
    }
}
