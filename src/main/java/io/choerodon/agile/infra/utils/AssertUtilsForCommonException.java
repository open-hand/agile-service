package io.choerodon.agile.infra.utils;

import io.choerodon.core.exception.CommonException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.Nullable;

/**
 * @author superlee
 * @since 2021-03-24
 */
public class AssertUtilsForCommonException {

    public static void notNull(@Nullable Object object, String message, Object... parameters) {
        if (object == null) {
            throw new CommonException(message, parameters);
        }
    }

    public static void notEmpty(@Nullable String value, String message, Object... parameters) {
        if (StringUtils.isEmpty(value)) {
            throw new CommonException(message, parameters);
        }
    }
}
