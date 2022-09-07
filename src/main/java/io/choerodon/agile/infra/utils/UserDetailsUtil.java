package io.choerodon.agile.infra.utils;

import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;

import java.util.Optional;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/7/6
 */

public class UserDetailsUtil {

    public static Long getCurrentUserId() {
        return Optional.ofNullable(DetailsHelper.getUserDetails())
                .map(CustomUserDetails::getUserId)
                .orElseThrow(() ->new CommonException("error.current.user.is.null"));
    }
}
