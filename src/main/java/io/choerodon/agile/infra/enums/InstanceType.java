package io.choerodon.agile.infra.enums;

import io.choerodon.core.exception.CommonException;
import org.springframework.lang.NonNull;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/5/24
 */
public enum InstanceType {

    ISSUE("issue"),

    BACKLOG("backlog");

    String value;

    InstanceType(String value) {
        this.value = value;
    }

    public String value() {
        return value;
    }

    public static InstanceType of(@NonNull String instanceType) {
        try {
            return valueOf(instanceType.toUpperCase());
        } catch (IllegalArgumentException exception) {
            throw new CommonException("error.instance.transfer", instanceType);
        }
    }

}
