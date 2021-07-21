package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2021-07-20
 */
public enum FieldPermissionScope {
    /**
     * 只读
     */
    READ("read"),
    /**
     * 修改，新增，删除
     */
    WRITE("write");

    private String value;

    FieldPermissionScope(String value) {
        this.value = value;
    }

    public String value() {
        return this.value;
    }

    public static boolean contains(String value) {
        for (FieldPermissionScope scope : values()) {
            if (scope.value().equals(value)) {
                return true;
            }
        }
        return false;
    }

    public static boolean isRead(String value) {
        return READ.value().equals(value);
    }

    public static boolean isWrite(String value) {
        return WRITE.value().equals(value);
    }
}
